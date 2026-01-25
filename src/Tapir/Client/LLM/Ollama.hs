{-# LANGUAGE OverloadedStrings #-}
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
    OllamaClient
  , mkClient

    -- * Request Functions
  , sendRequest
  , streamRequest

    -- * Utilities
  , checkConfigured
  , getBaseUrl

    -- * Re-exports
  , StreamResult(..)
  , StreamCallback
  ) where

import Control.Concurrent.STM (TVar)
import qualified Data.Text as T
import Network.HTTP.Client

import Tapir.Types (TapirError)
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types (ChatRequest, ChatResponse, StreamResult(..), StreamCallback)
import Tapir.Client.LLM.Base
import Tapir.Core.Constants (ollamaDefaultEndpoint)

-- | Ollama client (wrapper around generic client)
type OllamaClient = GenericLLMClient

-- | Get the base URL (supports custom base_url in config)
getBaseUrl :: ProviderConfig -> String
getBaseUrl cfg = case providerBaseUrl cfg of
  Just url -> T.unpack url <> "/v1/chat/completions"
  Nothing  -> ollamaDefaultEndpoint

-- | Create Ollama endpoint config
ollamaEndpoint :: ProviderConfig -> ProviderEndpoint
ollamaEndpoint cfg = ProviderEndpoint
  { peBaseUrl = getBaseUrl cfg
  , peAuthHeader = Nothing  -- Ollama doesn't need auth
  , peExtraHeaders = id
  , peRequiresAuth = False
  }

-- | Create an Ollama client
-- Note: Uses plain HTTP settings since Ollama typically runs locally
mkClient :: ProviderConfig -> IO OllamaClient
mkClient cfg = do
  manager <- newManager defaultManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (providerTimeoutSeconds cfg * 1000000)
    }
  pure $ mkGenericClient manager cfg (ollamaEndpoint cfg) (pure Nothing)

-- | Ollama is always "configured" (no API key needed)
checkConfigured :: ProviderConfig -> IO Bool
checkConfigured _ = pure True

-- | Send a non-streaming request
sendRequest :: OllamaClient -> ChatRequest -> IO (Either TapirError ChatResponse)
sendRequest = sendGenericRequest

-- | Send a streaming request
streamRequest
  :: OllamaClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)
  -> IO (Either TapirError StreamResult)
streamRequest = streamGenericRequest
