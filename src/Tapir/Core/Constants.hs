{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Core.Constants
-- Description : Centralized string constants
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- All magic strings should be defined here to ensure consistency
-- and make changes easier.

module Tapir.Core.Constants
  ( -- * Provider Names
    providerNameOpenRouter
  , providerNameOpenAI
  , providerNameOllama
  , providerNameAnthropic

    -- * Anki
  , ankiDefaultNoteType
  , ankiDefaultDeckSuffix

    -- * Environment Variables
  , envOpenRouterApiKey
  , envOpenAIApiKey
  , envAnthropicApiKey

    -- * API Endpoints
  , openRouterApiEndpoint
  , openAIApiEndpoint
  , ollamaDefaultEndpoint

    -- * Application
  , appName
  , appVersion
  ) where

import Data.Text (Text)

-- ════════════════════════════════════════════════════════════════
-- PROVIDER NAMES
-- ════════════════════════════════════════════════════════════════

providerNameOpenRouter :: Text
providerNameOpenRouter = "OpenRouter"

providerNameOpenAI :: Text
providerNameOpenAI = "OpenAI"

providerNameOllama :: Text
providerNameOllama = "Ollama"

providerNameAnthropic :: Text
providerNameAnthropic = "Anthropic"

-- ════════════════════════════════════════════════════════════════
-- ANKI
-- ════════════════════════════════════════════════════════════════

-- | Default Anki note type for flashcards
ankiDefaultNoteType :: Text
ankiDefaultNoteType = "Basic"

-- | Default suffix for Anki deck names
ankiDefaultDeckSuffix :: Text
ankiDefaultDeckSuffix = "::TAPIR"

-- ════════════════════════════════════════════════════════════════
-- ENVIRONMENT VARIABLES
-- ════════════════════════════════════════════════════════════════

envOpenRouterApiKey :: String
envOpenRouterApiKey = "OPENROUTER_API_KEY"

envOpenAIApiKey :: String
envOpenAIApiKey = "OPENAI_API_KEY"

envAnthropicApiKey :: String
envAnthropicApiKey = "ANTHROPIC_API_KEY"

-- ════════════════════════════════════════════════════════════════
-- API ENDPOINTS
-- ════════════════════════════════════════════════════════════════

openRouterApiEndpoint :: String
openRouterApiEndpoint = "https://openrouter.ai/api/v1/chat/completions"

openAIApiEndpoint :: String
openAIApiEndpoint = "https://api.openai.com/v1/chat/completions"

ollamaDefaultEndpoint :: String
ollamaDefaultEndpoint = "http://localhost:11434/v1/chat/completions"

-- ════════════════════════════════════════════════════════════════
-- APPLICATION
-- ════════════════════════════════════════════════════════════════

appName :: Text
appName = "TAPIR"

appVersion :: Text
appVersion = "1.0.0"
