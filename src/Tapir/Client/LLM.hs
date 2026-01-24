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

    -- * Streaming
  , StreamCallback
  , StreamResult(..)

    -- * Re-exports
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

-- ════════════════════════════════════════════════════════════════
-- CLIENT INTERFACE
-- ════════════════════════════════════════════════════════════════

-- | Callback for streaming tokens
-- The callback receives each token as it arrives
type StreamCallback = Text -> IO ()

-- | Result of a streaming completion
data StreamResult = StreamResult
  { srFullResponse :: !Text        -- ^ Complete concatenated response
  , srModel        :: !Text        -- ^ Model that generated the response
  , srTokensUsed   :: !(Maybe Int) -- ^ Token count if available
  } deriving (Eq, Show)

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
    { llmProviderName = "OpenRouter"
    , llmComplete = OpenRouter.sendRequest client
    , llmStreamComplete = \req cb cancel ->
        convertOpenRouterResult <$> OpenRouter.streamRequest client req cb cancel
    , llmIsConfigured = OpenRouter.checkConfigured cfg
    }
  where
    -- Convert OpenRouter.StreamResult to our StreamResult
    convertOpenRouterResult :: Either TapirError OpenRouter.StreamResult -> Either TapirError StreamResult
    convertOpenRouterResult = fmap $ \r -> StreamResult
      { srFullResponse = OpenRouter.srFullResponse r
      , srModel = OpenRouter.srModel r
      , srTokensUsed = OpenRouter.srTokensUsed r
      }

-- | Create OpenAI client wrapper
mkOpenAIClient :: ProviderConfig -> IO LLMClient
mkOpenAIClient cfg = do
  client <- OpenAI.mkClient cfg
  pure LLMClient
    { llmProviderName = "OpenAI"
    , llmComplete = OpenAI.sendRequest client
    , llmStreamComplete = \req cb cancel ->
        convertOpenAIResult <$> OpenAI.streamRequest client req cb cancel
    , llmIsConfigured = OpenAI.checkConfigured cfg
    }
  where
    -- Convert OpenAI.StreamResult to our StreamResult
    convertOpenAIResult :: Either TapirError OpenAI.StreamResult -> Either TapirError StreamResult
    convertOpenAIResult = fmap $ \r -> StreamResult
      { srFullResponse = OpenAI.srFullResponse r
      , srModel = OpenAI.srModel r
      , srTokensUsed = OpenAI.srTokensUsed r
      }

-- | Create Ollama client wrapper
mkOllamaClient :: ProviderConfig -> IO LLMClient
mkOllamaClient cfg = do
  client <- Ollama.mkClient cfg
  pure LLMClient
    { llmProviderName = "Ollama"
    , llmComplete = Ollama.sendRequest client
    , llmStreamComplete = \req cb cancel ->
        convertOllamaResult <$> Ollama.streamRequest client req cb cancel
    , llmIsConfigured = Ollama.checkConfigured cfg
    }
  where
    -- Convert Ollama.StreamResult to our StreamResult
    convertOllamaResult :: Either TapirError Ollama.StreamResult -> Either TapirError StreamResult
    convertOllamaResult = fmap $ \r -> StreamResult
      { srFullResponse = Ollama.srFullResponse r
      , srModel = Ollama.srModel r
      , srTokensUsed = Ollama.srTokensUsed r
      }

-- | Placeholder client for unimplemented providers
notImplementedClient :: Text -> LLMClient
notImplementedClient name = LLMClient
  { llmProviderName = name
  , llmComplete = \_ -> pure $ Left $ InternalError $ name <> " provider not yet implemented"
  , llmStreamComplete = \_ _ _ -> pure $ Left $ InternalError $ name <> " provider not yet implemented"
  , llmIsConfigured = pure False
  }
