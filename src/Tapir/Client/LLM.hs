{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM
-- Description : Abstract LLM client interface
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides a provider-agnostic interface for LLM interactions.
-- It supports both streaming and non-streaming completions.

module Tapir.Client.LLM
  ( -- * Client Interface
    LLMClient(..)
  , mkLLMClient

    -- * Re-exports (includes StreamCallback and StreamResult)
  , module Tapir.Client.LLM.Types
  ) where

import Control.Concurrent.STM (TVar)
import Data.Text (Text)

import Tapir.Types (TapirError(..), ProviderType(..))
import Tapir.Types.Provider (ProviderConfig(..))
import Tapir.Client.LLM.Types
import qualified Tapir.Client.LLM.OpenRouter as OpenRouter
import qualified Tapir.Client.LLM.OpenAI as OpenAI
import qualified Tapir.Client.LLM.Ollama as Ollama
import Tapir.Core.Constants (providerNameOpenRouter, providerNameOpenAI, providerNameOllama)

-- ════════════════════════════════════════════════════════════════
-- CLIENT INTERFACE
-- ════════════════════════════════════════════════════════════════

-- | Abstract LLM client interface
data LLMClient = LLMClient
  { -- | Provider name for display/logging
    llmProviderName :: !Text

    -- | Send a non-streaming completion request
  , llmComplete :: ChatRequest -> IO (Either TapirError ChatResponse)

    -- | Send a streaming completion request
    -- The callback is invoked for each token received
    -- Returns the full response when complete
  , llmStreamComplete :: ChatRequest
                      -> StreamCallback
                      -> Maybe (TVar Bool)  -- ^ Optional cancel flag
                      -> IO (Either TapirError StreamResult)

    -- | Check if the client is properly configured (has API key, etc.)
  , llmIsConfigured :: IO Bool
  }

-- ════════════════════════════════════════════════════════════════
-- CLIENT CONSTRUCTION
-- ════════════════════════════════════════════════════════════════

-- | Create an LLM client based on provider configuration
mkLLMClient :: ProviderConfig -> IO LLMClient
mkLLMClient cfg = case providerType cfg of
  OpenRouter -> mkOpenRouterClient cfg
  Anthropic  -> pure $ notImplementedClient "Anthropic"
  OpenAI     -> mkOpenAIClient cfg
  Ollama     -> mkOllamaClient cfg

-- | Create OpenRouter client wrapper
mkOpenRouterClient :: ProviderConfig -> IO LLMClient
mkOpenRouterClient cfg = do
  client <- OpenRouter.mkClient cfg
  pure LLMClient
    { llmProviderName = providerNameOpenRouter
    , llmComplete = OpenRouter.sendRequest client
    , llmStreamComplete = OpenRouter.streamRequest client
    , llmIsConfigured = OpenRouter.checkConfigured cfg
    }

-- | Create OpenAI client wrapper
mkOpenAIClient :: ProviderConfig -> IO LLMClient
mkOpenAIClient cfg = do
  client <- OpenAI.mkClient cfg
  pure LLMClient
    { llmProviderName = providerNameOpenAI
    , llmComplete = OpenAI.sendRequest client
    , llmStreamComplete = OpenAI.streamRequest client
    , llmIsConfigured = OpenAI.checkConfigured cfg
    }

-- | Create Ollama client wrapper
mkOllamaClient :: ProviderConfig -> IO LLMClient
mkOllamaClient cfg = do
  client <- Ollama.mkClient cfg
  pure LLMClient
    { llmProviderName = providerNameOllama
    , llmComplete = Ollama.sendRequest client
    , llmStreamComplete = Ollama.streamRequest client
    , llmIsConfigured = Ollama.checkConfigured cfg
    }

-- | Placeholder client for unimplemented providers
notImplementedClient :: Text -> LLMClient
notImplementedClient name = LLMClient
  { llmProviderName = name
  , llmComplete = \_ -> pure $ Left $ InternalError $ name <> " provider not yet implemented"
  , llmStreamComplete = \_ _ _ -> pure $ Left $ InternalError $ name <> " provider not yet implemented"
  , llmIsConfigured = pure False
  }
