{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.Base
-- Description : Shared implementation for OpenAI-compatible LLM providers
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides the common implementation for providers that use
-- the OpenAI chat completions API format (OpenAI, OpenRouter, Ollama).

module Tapir.Client.LLM.Base
  ( -- * Generic Client
    GenericLLMClient(..)
  , mkGenericClient

    -- * Request Execution
  , sendGenericRequest
  , streamGenericRequest

    -- * Configuration
  , ProviderEndpoint(..)
  ) where

import Control.Concurrent.STM (TVar)
import Control.Exception (try)
import Control.Monad (when)
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Tapir.Client.LLM.Error (LLMError(..))
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types
import Tapir.Client.LLM.SSE (processSSEStream)

-- | Provider-specific endpoint configuration
data ProviderEndpoint = ProviderEndpoint
  { peBaseUrl      :: !String
    -- ^ API endpoint URL
  , peAuthHeader   :: !(Maybe (Text -> Request -> Request))
    -- ^ Function to add auth header given API key
  , peExtraHeaders :: !(Request -> Request)
    -- ^ Additional provider-specific headers
  , peRequiresAuth :: !Bool
    -- ^ Whether this provider requires an API key
  }

-- | Generic client that works with any OpenAI-compatible API
data GenericLLMClient = GenericLLMClient
  { glcManager   :: !Manager
  , glcConfig    :: !ProviderConfig
  , glcEndpoint  :: !ProviderEndpoint
  , glcGetApiKey :: !(IO (Maybe Text))
    -- ^ IO action to retrieve the API key (from config or env)
  }

-- | Create a generic client with the given manager and endpoint config
mkGenericClient
  :: Manager
  -> ProviderConfig
  -> ProviderEndpoint
  -> IO (Maybe Text)  -- ^ API key retrieval action
  -> GenericLLMClient
mkGenericClient mgr cfg endpoint getKey = GenericLLMClient
  { glcManager   = mgr
  , glcConfig    = cfg
  , glcEndpoint  = endpoint
  , glcGetApiKey = getKey
  }

-- | Send a non-streaming chat completion request
sendGenericRequest
  :: GenericLLMClient
  -> ChatRequest
  -> IO (Either LLMError ChatResponse)
sendGenericRequest GenericLLMClient{..} chatReq = do
  -- Check for API key if required
  mApiKey <- glcGetApiKey
  case (peRequiresAuth glcEndpoint, mApiKey) of
    (True, Nothing) -> pure $ Left LLMAPIKeyMissing
    _ -> do
      result <- try $ do
        -- Build request
        initReq <- parseRequest (peBaseUrl glcEndpoint)
        let reqBody = encode chatReq { crStream = False }

        -- Apply auth header if we have a key and a header function
        let withAuth = case (mApiKey, peAuthHeader glcEndpoint) of
              (Just key, Just addAuth) -> addAuth key
              _ -> id

        let req = withAuth $ peExtraHeaders glcEndpoint $ initReq
              { method = "POST"
              , requestHeaders =
                  ("Content-Type", "application/json") : requestHeaders initReq
              , requestBody = RequestBodyLBS reqBody
              }

        -- Send request
        resp <- httpLbs req glcManager

        -- Parse response
        let status = statusCode $ responseStatus resp
        let body = responseBody resp

        if status >= 200 && status < 300
          then case eitherDecode body of
            Right chatResp -> pure $ Right chatResp
            Left err -> pure $ Left $ LLMAPIError status (T.pack err)
          else case decode body of
            Just (APIErrorResponse err) ->
              pure $ Left $ LLMAPIError status (aedMessage err)
            Nothing ->
              pure $ Left $ LLMAPIError status $ T.pack $ "HTTP " <> show status

      case result of
        Left (e :: HttpException) -> pure $ Left $ LLMNetworkError $ T.pack $ show e
        Right r -> pure r

-- | Send a streaming chat completion request
streamGenericRequest
  :: GenericLLMClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)  -- ^ Optional cancel flag
  -> IO (Either LLMError StreamResult)
streamGenericRequest GenericLLMClient{..} chatReq onToken mCancelFlag = do
  -- Check for API key if required
  mApiKey <- glcGetApiKey
  case (peRequiresAuth glcEndpoint, mApiKey) of
    (True, Nothing) -> pure $ Left LLMAPIKeyMissing
    _ -> do
      result <- try $ do
        -- Build request
        initReq <- parseRequest (peBaseUrl glcEndpoint)
        let reqBody = encode chatReq { crStream = True }

        -- Apply auth header if we have a key and a header function
        let withAuth = case (mApiKey, peAuthHeader glcEndpoint) of
              (Just key, Just addAuth) -> addAuth key
              _ -> id

        let req = withAuth $ peExtraHeaders glcEndpoint $ initReq
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Accept", "text/event-stream")
                  ] ++ requestHeaders initReq
              , requestBody = RequestBodyLBS reqBody
              }

        -- Accumulators
        fullResponseRef <- newIORef T.empty
        modelRef <- newIORef T.empty

        -- Send request and process streaming response
        withResponse req glcManager $ \resp -> do
          let status = statusCode $ responseStatus resp

          if status >= 200 && status < 300
            then do
              -- Process SSE stream
              processSSEStream (responseBody resp) mCancelFlag $ \chunk -> do
                case scChoices chunk of
                  (choice:_) -> do
                    -- Update model (from first chunk)
                    currentModel <- readIORef modelRef
                    when (T.null currentModel) $
                      modifyIORef' modelRef (const $ scModel chunk)

                    -- Extract and emit content
                    case deltaContent (streamChoiceDelta choice) of
                      Just content -> do
                        modifyIORef' fullResponseRef (<> content)
                        onToken content
                      Nothing -> pure ()
                  [] -> pure ()

              -- Build result
              fullResponse <- readIORef fullResponseRef
              model <- readIORef modelRef
              pure $ Right $ StreamResult
                { srFullResponse = fullResponse
                , srModel = if T.null model then crModel chatReq else model
                , srTokensUsed = Nothing
                }
            else do
              -- Read error body
              bodyChunks <- brConsume (responseBody resp)
              let body = BL.fromChunks bodyChunks
              case decode body of
                Just (APIErrorResponse err) ->
                  pure $ Left $ LLMAPIError status (aedMessage err)
                Nothing ->
                  pure $ Left $ LLMAPIError status $ T.pack $ "HTTP " <> show status

      case result of
        Left (e :: HttpException) -> pure $ Left $ LLMNetworkError $ T.pack $ show e
        Right r -> pure r
