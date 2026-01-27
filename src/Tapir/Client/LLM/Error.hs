{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Client.LLM.Error
-- Description : LLM client error types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.Error
  ( LLMError(..)
  , displayLLMError
  , shortLLMError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Errors specific to LLM client operations
data LLMError
  = LLMAPIKeyMissing
  | LLMAPIError !Int !Text           -- ^ HTTP status code and message
  | LLMNetworkError !Text
  | LLMStreamingError !Text
  | LLMModelNotAvailable !Text
  | LLMRateLimitExceeded !Int        -- ^ Retry after seconds
  | LLMProviderNotImplemented !Text
  deriving (Eq, Show)

-- | User-friendly error messages
displayLLMError :: LLMError -> Text
displayLLMError = \case
  LLMAPIKeyMissing ->
    "API key not set. Set the appropriate environment variable or add to config."
  LLMAPIError code msg ->
    "API error (" <> T.pack (show code) <> "): " <> msg
  LLMNetworkError msg ->
    "Network error: " <> msg <> ". Check your internet connection."
  LLMStreamingError msg ->
    "Streaming error: " <> msg
  LLMModelNotAvailable model ->
    "Model '" <> model <> "' not available. Check your provider settings."
  LLMRateLimitExceeded secs ->
    "Rate limit exceeded. Retry after " <> T.pack (show secs) <> " seconds."
  LLMProviderNotImplemented name ->
    name <> " provider is not yet implemented."

-- | Short error messages for status bar
shortLLMError :: LLMError -> Text
shortLLMError = \case
  LLMAPIKeyMissing        -> "API key missing"
  LLMAPIError code _      -> "API error " <> T.pack (show code)
  LLMNetworkError _       -> "Network error"
  LLMStreamingError _     -> "Streaming error"
  LLMModelNotAvailable _  -> "Model not available"
  LLMRateLimitExceeded _  -> "Rate limited"
  LLMProviderNotImplemented _ -> "Provider not implemented"
