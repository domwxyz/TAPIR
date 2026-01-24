{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.Ollama
-- Description : Ollama API client implementation (OpenAI-compatible)
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module implements the Ollama chat completions API using its
-- OpenAI-compatible endpoint (/v1/chat/completions) with support
-- for both streaming and non-streaming requests.
--
-- Ollama runs locally and does not require authentication.
--
-- API Documentation: https://ollama.ai/docs/api

module Tapir.Client.LLM.Ollama
  ( -- * Client Creation
    OllamaClient(..)
  , mkClient

    -- * Request Functions
  , sendRequest
  , streamRequest

    -- * Types
  , StreamResult(..)
  , StreamCallback

    -- * Utilities
  , checkConfigured
  , getBaseUrl
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (try, SomeException)
import Control.Monad (unless, when)
import Data.Aeson (encode, decode, eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Tapir.Types (TapirError(..))
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types

-- ════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════

-- | Default Ollama API base URL (OpenAI-compatible endpoint)
defaultOllamaBaseUrl :: String
defaultOllamaBaseUrl = "http://localhost:11434/v1/chat/completions"

-- | Get the base URL to use (supports custom base_url in config)
getBaseUrl :: ProviderConfig -> String
getBaseUrl cfg = case providerBaseUrl cfg of
  Just url -> T.unpack url <> "/v1/chat/completions"
  Nothing  -> defaultOllamaBaseUrl

-- ════════════════════════════════════════════════════════════════
-- CLIENT
-- ════════════════════════════════════════════════════════════════

-- | Ollama client with HTTP manager
data OllamaClient = OllamaClient
  { olcManager :: !Manager
  , olcConfig  :: !ProviderConfig
  , olcBaseUrl :: !String
  }

-- | Create an Ollama client
-- Note: Uses plain HTTP settings since Ollama typically runs locally
mkClient :: ProviderConfig -> IO OllamaClient
mkClient cfg = do
  manager <- newManager defaultManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (providerTimeoutSeconds cfg * 1000000)
    }
  pure OllamaClient
    { olcManager = manager
    , olcConfig  = cfg
    , olcBaseUrl = getBaseUrl cfg
    }

-- | Check if Ollama is available
-- For Ollama, we're always "configured" since no API key is needed.
-- The actual availability check would require a health check request.
checkConfigured :: ProviderConfig -> IO Bool
checkConfigured _ = pure True

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
sendRequest :: OllamaClient -> ChatRequest -> IO (Either TapirError ChatResponse)
sendRequest OllamaClient{..} chatReq = do
  result <- try $ do
    -- Build request
    initReq <- parseRequest olcBaseUrl
    let reqBody = encode chatReq { crStream = False }
    let req = initReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              ]
          , requestBody = RequestBodyLBS reqBody
          }

    -- Send request
    resp <- httpLbs req olcManager

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
  :: OllamaClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)  -- ^ Optional cancel flag
  -> IO (Either TapirError StreamResult)
streamRequest OllamaClient{..} chatReq onToken mCancelFlag = do
  result <- try $ do
    -- Build request
    initReq <- parseRequest olcBaseUrl
    let reqBody = encode chatReq { crStream = True }
    let req = initReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "text/event-stream")
              ]
          , requestBody = RequestBodyLBS reqBody
          }

    -- Accumulators
    fullResponseRef <- newIORef T.empty
    modelRef <- newIORef T.empty

    -- Send request and process streaming response
    withResponse req olcManager $ \resp -> do
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

-- ════════════════════════════════════════════════════════════════
-- SSE STREAM PROCESSING
-- ════════════════════════════════════════════════════════════════

-- | Process a Server-Sent Events stream
processSSEStream
  :: BodyReader
  -> Maybe (TVar Bool)           -- ^ Cancel flag
  -> (StreamChunk -> IO ())      -- ^ Chunk handler
  -> IO ()
processSSEStream bodyReader mCancelFlag onChunk = do
  -- Buffer for incomplete lines
  bufferRef <- newIORef BS.empty

  let loop = do
        -- Check for cancellation
        cancelled <- case mCancelFlag of
          Just flag -> readTVarIO flag
          Nothing   -> pure False

        unless cancelled $ do
          -- Read next chunk of data
          chunk <- brRead bodyReader

          unless (BS.null chunk) $ do
            -- Append to buffer
            buffer <- readIORef bufferRef
            let fullBuffer = buffer <> chunk

            -- Process complete lines
            let (completeLines, remainder) = splitLines fullBuffer
            modifyIORef' bufferRef (const remainder)

            -- Process each line
            mapM_ processLine completeLines

            -- Continue reading
            loop

  loop
  where
    -- Split buffer into complete lines and remainder
    splitLines :: ByteString -> ([ByteString], ByteString)
    splitLines bs =
      let parts = BS8.split '\n' bs
      in case parts of
        []  -> ([], BS.empty)
        [x] -> ([], x)  -- Incomplete line
        xs  -> (init xs, last xs)

    -- Process a single SSE line
    processLine :: ByteString -> IO ()
    processLine line
      -- Skip empty lines
      | BS.null line = pure ()
      -- Skip SSE comments (lines starting with ':')
      | Just (0x3A, _) <- BS.uncons line = pure ()  -- 0x3A = ':'
      -- Handle data lines
      | "data: " `BS.isPrefixOf` line = do
          let jsonData = BS.drop 6 line
          -- Check for end of stream
          if jsonData == "[DONE]"
            then pure ()
            else case decode (BL.fromStrict jsonData) of
              Just chunk -> onChunk chunk
              Nothing    -> pure ()  -- Skip malformed chunks
      -- Skip other lines (event:, id:, retry:)
      | otherwise = pure ()
