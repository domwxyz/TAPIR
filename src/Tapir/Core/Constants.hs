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

    -- * LLM Request Limits
  , maxHistoryMessages
  , defaultLLMTemperature
  , defaultLLMMaxTokens

    -- * UI Dimensions
  , sessionListViewportHeight
  , inputAreaMaxHeight
  , minTextWrapWidth
  , textWrapHorizontalPadding
  , maxVocabHighlightsInChat

    -- * Database Query Limits
  , recentSessionsLimit

    -- * Async / Concurrency
  , eventChannelBufferSize

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
-- LLM REQUEST LIMITS
-- ════════════════════════════════════════════════════════════════

-- | Maximum number of conversation history messages to include in LLM requests.
-- Keeps context window manageable while providing enough context.
maxHistoryMessages :: Int
maxHistoryMessages = 20

-- | Default temperature for LLM requests when not specified in config.
-- Range: 0.0 (deterministic) to 2.0 (creative). 0.7 balances coherence and variety.
defaultLLMTemperature :: Double
defaultLLMTemperature = 0.7

-- | Default max tokens for LLM responses when not specified in config.
defaultLLMMaxTokens :: Int
defaultLLMMaxTokens = 2000

-- ════════════════════════════════════════════════════════════════
-- UI DIMENSIONS
-- ════════════════════════════════════════════════════════════════

-- | Height of the session list viewport in the sessions modal.
sessionListViewportHeight :: Int
sessionListViewportHeight = 14

-- | Maximum height of the input text area (in lines).
inputAreaMaxHeight :: Int
inputAreaMaxHeight = 3

-- | Minimum width for text wrapping (prevents overly narrow columns).
minTextWrapWidth :: Int
minTextWrapWidth = 20

-- | Horizontal padding subtracted from available width for text wrapping.
textWrapHorizontalPadding :: Int
textWrapHorizontalPadding = 2

-- | Maximum vocabulary highlights to show in conversation mode responses.
maxVocabHighlightsInChat :: Int
maxVocabHighlightsInChat = 3

-- ════════════════════════════════════════════════════════════════
-- DATABASE QUERY LIMITS
-- ════════════════════════════════════════════════════════════════

-- | Number of recent sessions to load for the session list modal.
recentSessionsLimit :: Int
recentSessionsLimit = 50

-- ════════════════════════════════════════════════════════════════
-- ASYNC / CONCURRENCY
-- ════════════════════════════════════════════════════════════════

-- | Size of the BChan buffer for async events.
-- Larger values prevent blocking on rapid events but use more memory.
eventChannelBufferSize :: Int
eventChannelBufferSize = 10

-- ════════════════════════════════════════════════════════════════
-- APPLICATION
-- ════════════════════════════════════════════════════════════════

appName :: Text
appName = "TAPIR"

appVersion :: Text
appVersion = "1.0.0"
