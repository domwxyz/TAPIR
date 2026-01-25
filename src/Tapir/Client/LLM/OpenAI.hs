{-# LANGUAGE OverloadedStrings #-}
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
    OpenAIClient
  , mkClient

    -- * Request Functions
  , sendRequest
  , streamRequest

    -- * Utilities
  , checkConfigured
  , getApiKey

    -- * Re-exports
  , StreamResult(..)
  , StreamCallback
  ) where

import Control.Concurrent.STM (TVar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)

import Tapir.Types (TapirError)
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types (ChatRequest, ChatResponse, StreamResult(..), StreamCallback)
import Tapir.Client.LLM.Base

-- | OpenAI client (wrapper around generic client)
type OpenAIClient = GenericLLMClient

-- | OpenAI API endpoint
openAIEndpoint :: ProviderEndpoint
openAIEndpoint = ProviderEndpoint
  { peBaseUrl = "https://api.openai.com/v1/chat/completions"
  , peAuthHeader = Just bearerAuth
  , peExtraHeaders = id  -- No extra headers needed
  , peRequiresAuth = True
  }
  where
    bearerAuth apiKey req = req
      { requestHeaders =
          ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey) : requestHeaders req
      }

-- | Create an OpenAI client
mkClient :: ProviderConfig -> IO OpenAIClient
mkClient cfg = do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (providerTimeoutSeconds cfg * 1000000)
    }
  pure $ mkGenericClient manager cfg openAIEndpoint (getApiKey cfg)

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
      Nothing ->
        -- Fall back to standard OpenAI env var if configured var is different
        if envVar /= "OPENAI_API_KEY"
          then (fmap T.pack) <$> lookupEnv "OPENAI_API_KEY"
          else pure Nothing

-- | Send a non-streaming request
sendRequest :: OpenAIClient -> ChatRequest -> IO (Either TapirError ChatResponse)
sendRequest = sendGenericRequest

-- | Send a streaming request
streamRequest
  :: OpenAIClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)
  -> IO (Either TapirError StreamResult)
streamRequest = streamGenericRequest
