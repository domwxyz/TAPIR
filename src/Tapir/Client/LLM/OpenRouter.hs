{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.OpenRouter
-- Description : OpenRouter API client implementation
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module implements the OpenRouter chat completions API with support
-- for both streaming and non-streaming requests.
--
-- OpenRouter provides access to many models through a unified API that
-- follows the OpenAI chat completions format.
--
-- API Documentation: https://openrouter.ai/docs

module Tapir.Client.LLM.OpenRouter
  ( -- * Client Creation
    OpenRouterClient
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

-- | OpenRouter client (wrapper around generic client)
type OpenRouterClient = GenericLLMClient

-- | OpenRouter API endpoint
openRouterEndpoint :: ProviderEndpoint
openRouterEndpoint = ProviderEndpoint
  { peBaseUrl = "https://openrouter.ai/api/v1/chat/completions"
  , peAuthHeader = Just bearerAuth
  , peExtraHeaders = addOpenRouterHeaders
  , peRequiresAuth = True
  }
  where
    addOpenRouterHeaders req = req
      { requestHeaders = requestHeaders req ++
          [ ("HTTP-Referer", "https://github.com/tapir-language/tapir")
          , ("X-Title", "TAPIR Language Learning Assistant")
          ]
      }

    bearerAuth apiKey req = req
      { requestHeaders =
          ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey) : requestHeaders req
      }

-- | Create an OpenRouter client
mkClient :: ProviderConfig -> IO OpenRouterClient
mkClient cfg = do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (providerTimeoutSeconds cfg * 1000000)
    }
  pure $ mkGenericClient manager cfg openRouterEndpoint (getApiKey cfg)

-- | Check if the client has a valid API key
checkConfigured :: ProviderConfig -> IO Bool
checkConfigured cfg = do
  key <- getApiKey cfg
  pure $ case key of
    Just k  -> not (T.null k)
    Nothing -> False

-- | Get API key from config or environment
getApiKey :: ProviderConfig -> IO (Maybe Text)
getApiKey cfg = case providerApiKey cfg of
  Just k  -> pure $ Just k
  Nothing -> do
    mEnvKey <- lookupEnv (T.unpack $ providerApiKeyEnv cfg)
    pure $ T.pack <$> mEnvKey

-- | Send a non-streaming request
sendRequest :: OpenRouterClient -> ChatRequest -> IO (Either TapirError ChatResponse)
sendRequest = sendGenericRequest

-- | Send a streaming request
streamRequest
  :: OpenRouterClient
  -> ChatRequest
  -> StreamCallback
  -> Maybe (TVar Bool)
  -> IO (Either TapirError StreamResult)
streamRequest = streamGenericRequest
