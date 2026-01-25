{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.OpenAI
-- Description : OpenAI API client implementation
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module implements the OpenAI chat completions API with support
-- for both streaming and non-streaming requests.
--
-- API Documentation: https://platform.openai.com/docs/api-reference/chat

module Tapir.Client.LLM.OpenAI
  ( -- * Client Creation
    OpenAIClient(..)
  , mkClient

    -- * Request Functions
  , sendRequest
  , streamRequest

    -- * Types
  , StreamResult(..)
  , StreamCallback

    -- * Utilities
  , checkConfigured
  , getApiKey
  ) where

import Control.Concurrent.STM (TVar)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

import Tapir.Types (TapirError(..))
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types
import Tapir.Client.LLM.SSE (processSSEStream)

-- ════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════

-- | OpenAI API base URL
openAIBaseUrl :: String
openAIBaseUrl = "https://api.openai.com/v1/chat/completions"

-- ════════════════════════════════════════════════════════════════
-- CLIENT
-- ════════════════════════════════════════════════════════════════

-- | OpenAI client with HTTP manager
data OpenAIClient = OpenAIClient
  { oacManager :: !Manager
  , oacConfig  :: !ProviderConfig
  }

-- | Create an OpenAI client
mkClient :: ProviderConfig -> IO OpenAIClient
mkClient cfg = do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (providerTimeoutSeconds cfg * 1000000)
    }
  pure OpenAIClient
    { oacManager = manager
    , oacConfig  = cfg
    }

-- | Check if the client has a valid API key
checkConfigured :: ProviderConfig -> IO Bool
checkConfigured cfg = do
  key <- getApiKey cfg
  pure $ case key of
    Just k  -> not (T.null k)
    Nothing -> False

-- | Get API key from config or environment
-- Falls back to OPENAI_API_KEY environment variable
getApiKey :: ProviderConfig -> IO (Maybe Text)
getApiKey cfg = case providerApiKey cfg of
  Just k  -> pure $ Just k
  Nothing -> do
    -- First try the configured env var, then fall back to OPENAI_API_KEY
    let envVar = providerApiKeyEnv cfg
    mEnvKey <- lookupEnv (T.unpack envVar)
    case mEnvKey of
      Just k -> pure $ Just $ T.pack k
      Nothing -> do
        -- Fall back to standard OpenAI env var if configured var is different
        if envVar /= "OPENAI_API_KEY"
          then (fmap T.pack) <$> lookupEnv "OPENAI_API_KEY"
          else pure Nothing

-- ════════════════════════════════════════════════════════════════
-- STREAMING TYPES
-- ════════════════════════════════════════════════════════════════

-- | Callback for streaming tokens
type StreamCallback = Text -> IO ()

-- | Result of a streaming completion
data StreamResult = StreamResult
  { srFullResponse :: !Text        -- ^ Complete concatenated response
  , srModel        :: !Text        -- ^ Model that generated the response
  , srTokensUsed   :: !(Maybe Int) -- ^ Token count if available
  } deriving (Eq, Show)

-- ════════════════════════════════════════════════════════════════
-- NON-STREAMING REQUEST
-- ════════════════════════════════════════════════════════════════

-- | Send a non-streaming chat completion request
sendRequest :: OpenAIClient -> ChatRequest -> IO (Either TapirError ChatResponse)
sendRequest OpenAIClient{..} chatReq = do
  mApiKey <- getApiKey oacConfig
  case mApiKey of
    Nothing -> pure $ Left APIKeyMissing
    Just apiKey -> do
      result <- try $ do
        -- Build request
        initReq <- parseRequest openAIBaseUrl
        let reqBody = encode chatReq { crStream = False }
        let req = initReq
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
                  ]
              , requestBody = RequestBodyLBS reqBody
              }

        -- Send request
        resp <- httpLbs req oacManager

        -- Parse response
        let status = statusCode $ responseStatus resp
        let body = responseBody resp

        if status >= 200 && status < 300
          then case eitherDecode body of
            Right chatResp -> pure $ Right chatResp
            Left err -> pure $ Left $ APIError status (T.pack err)
          else case decode body of
            Just (APIErrorResponse err) ->
              pure $ Left $ APIError status (aedMessage err)
            Nothing ->
              pure $ Left $ APIError status $ T.pack $ "HTTP " <> show status

      case result of
        Left (e :: SomeException) -> pure $ Left $ NetworkError $ T.pack $ show e
        Right r -> pure r

-- ════════════════════════════════════════════════════════════════
-- STREAMING REQUEST
-- ════════════════════════════════════════════════════════════════

-- | Send a streaming chat completion request
streamRequest
  :: OpenAIClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)  -- ^ Optional cancel flag
  -> IO (Either TapirError StreamResult)
streamRequest OpenAIClient{..} chatReq onToken mCancelFlag = do
  mApiKey <- getApiKey oacConfig
  case mApiKey of
    Nothing -> pure $ Left APIKeyMissing
    Just apiKey -> do
      result <- try $ do
        -- Build request
        initReq <- parseRequest openAIBaseUrl
        let reqBody = encode chatReq { crStream = True }
        let req = initReq
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
                  , ("Accept", "text/event-stream")
                  ]
              , requestBody = RequestBodyLBS reqBody
              }

        -- Accumulators
        fullResponseRef <- newIORef T.empty
        modelRef <- newIORef T.empty

        -- Send request and process streaming response
        withResponse req oacManager $ \resp -> do
          let status = statusCode $ responseStatus resp

          if status >= 200 && status < 300
            then do
              -- Process SSE stream
              processSSEStream (responseBody resp) mCancelFlag $ \chunk -> do
                -- Extract content from chunk
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
                , srTokensUsed = Nothing  -- Not available in streaming
                }
            else do
              -- Read error body
              bodyChunks <- brConsume (responseBody resp)
              let body = BL.fromChunks bodyChunks
              case decode body of
                Just (APIErrorResponse err) ->
                  pure $ Left $ APIError status (aedMessage err)
                Nothing ->
                  pure $ Left $ APIError status $ T.pack $ "HTTP " <> show status

      case result of
        Left (e :: SomeException) -> pure $ Left $ NetworkError $ T.pack $ show e
        Right r -> pure r
