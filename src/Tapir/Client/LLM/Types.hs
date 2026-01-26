{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Client.LLM.Types
-- Description : Types for LLM API interactions
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the types used for LLM API requests and responses,
-- following the OpenAI-compatible chat completions format used by OpenRouter.

module Tapir.Client.LLM.Types
  ( -- * Request Types
    ChatMessage(..)
  , ChatRequest(..)
  , defaultChatRequest

    -- * Response Types
  , ChatResponse(..)
  , Choice(..)
  , ResponseMessage(..)
  , Usage(..)

    -- * Streaming Types
  , StreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
  , ToolCallDelta(..)
  , FunctionCallDelta(..)

    -- * Streaming Result Types (shared across providers)
  , StreamCallback
  , StreamResult(..)

    -- * Tool Call Types
  , ToolCall(..)
  , FunctionCall(..)

    -- * Error Types
  , APIErrorResponse(..)
  , APIErrorDetail(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Tapir.Client.LLM.Tools (Tool, ToolChoice)
import Tapir.Core.Constants (defaultLLMTemperature, defaultLLMMaxTokens)

-- ════════════════════════════════════════════════════════════════
-- REQUEST TYPES
-- ════════════════════════════════════════════════════════════════

-- | Chat message in the conversation
data ChatMessage = ChatMessage
  { cmRole    :: !Text  -- ^ "system" | "user" | "assistant"
  , cmContent :: !Text  -- ^ Message content
  } deriving (Eq, Show, Generic)

instance ToJSON ChatMessage where
  toJSON ChatMessage{..} = object
    [ "role"    .= cmRole
    , "content" .= cmContent
    ]

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v -> ChatMessage
    <$> v .: "role"
    <*> v .: "content"

-- | Chat completion request
data ChatRequest = ChatRequest
  { crModel       :: !Text              -- ^ Model identifier (e.g., "z-ai/glm-4.7")
  , crMessages    :: ![ChatMessage]     -- ^ Conversation history
  , crStream      :: !Bool               -- ^ Enable streaming
  , crTemperature :: !(Maybe Double)     -- ^ Sampling temperature (0.0-2.0)
  , crMaxTokens   :: !(Maybe Int)        -- ^ Maximum tokens to generate
  , crTopP        :: !(Maybe Double)     -- ^ Nucleus sampling parameter
  , crStop        :: !(Maybe [Text])      -- ^ Stop sequences
  , crTools       :: !(Maybe [Tool])      -- ^ Tool definitions for function calling
  , crToolChoice  :: !(Maybe ToolChoice)  -- ^ Tool selection mode
  } deriving (Eq, Show, Generic)

instance ToJSON ChatRequest where
  toJSON ChatRequest{..} = object $ filter ((/= Null) . snd)
    [ "model"       .= crModel
    , "messages"    .= crMessages
    , "stream"      .= crStream
    , "temperature" .= crTemperature
    , "max_tokens"  .= crMaxTokens
    , "top_p"       .= crTopP
    , "stop"        .= crStop
    , "tools"       .= crTools
    , "tool_choice" .= crToolChoice
    ]

-- | Default chat request with sensible defaults
defaultChatRequest :: Text -> [ChatMessage] -> ChatRequest
defaultChatRequest model msgs = ChatRequest
  { crModel       = model
  , crMessages    = msgs
  , crStream      = True
  , crTemperature = Just defaultLLMTemperature
  , crMaxTokens   = Just defaultLLMMaxTokens
  , crTopP        = Nothing
  , crStop        = Nothing
  , crTools       = Nothing
  , crToolChoice  = Nothing
  }

-- ════════════════════════════════════════════════════════════════
-- RESPONSE TYPES (Non-streaming)
-- ════════════════════════════════════════════════════════════════

-- | Complete chat response (non-streaming)
data ChatResponse = ChatResponse
  { respId      :: !Text
  , respModel   :: !Text
  , respChoices :: ![Choice]
  , respUsage   :: !(Maybe Usage)
  } deriving (Eq, Show, Generic)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v -> ChatResponse
    <$> v .: "id"
    <*> v .: "model"
    <*> v .: "choices"
    <*> v .:? "usage"

-- | Response choice
data Choice = Choice
  { choiceIndex        :: !Int
  , choiceMessage      :: !ResponseMessage
  , choiceFinishReason :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v -> Choice
    <$> v .: "index"
    <*> v .: "message"
    <*> v .:? "finish_reason"

-- | Response message
data ResponseMessage = ResponseMessage
  { rmRole      :: !Text
  , rmContent   :: !(Maybe Text)       -- ^ May be null when using tools
  , rmToolCalls :: !(Maybe [ToolCall]) -- ^ Tool calls made
  } deriving (Eq, Show, Generic)

instance FromJSON ResponseMessage where
  parseJSON = withObject "ResponseMessage" $ \v -> ResponseMessage
    <$> v .: "role"
    <*> v .:? "content"
    <*> v .:? "tool_calls"

-- | Token usage statistics
data Usage = Usage
  { usagePromptTokens     :: !Int
  , usageCompletionTokens :: !Int
  , usageTotalTokens      :: !Int
  } deriving (Eq, Show, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v -> Usage
    <$> v .: "prompt_tokens"
    <*> v .: "completion_tokens"
    <*> v .: "total_tokens"

-- ════════════════════════════════════════════════════════════════
-- STREAMING TYPES
-- ════════════════════════════════════════════════════════════════

-- | Streaming chunk response
data StreamChunk = StreamChunk
  { scId      :: !Text
  , scModel   :: !Text
  , scChoices :: ![StreamChoice]
  } deriving (Eq, Show, Generic)

instance FromJSON StreamChunk where
  parseJSON = withObject "StreamChunk" $ \v -> StreamChunk
    <$> v .: "id"
    <*> v .: "model"
    <*> v .: "choices"

-- | Streaming choice
data StreamChoice = StreamChoice
  { streamChoiceIndex        :: !Int
  , streamChoiceDelta        :: !StreamDelta
  , streamChoiceFinishReason :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON StreamChoice where
  parseJSON = withObject "StreamChoice" $ \v -> StreamChoice
    <$> v .: "index"
    <*> v .: "delta"
    <*> v .:? "finish_reason"

-- | Streaming delta (partial content)
data StreamDelta = StreamDelta
  { deltaRole      :: !(Maybe Text)
  , deltaContent   :: !(Maybe Text)
  , deltaToolCalls :: !(Maybe [ToolCallDelta])  -- ^ Tool calls (may be partial)
  } deriving (Eq, Show, Generic)

instance FromJSON StreamDelta where
  parseJSON = withObject "StreamDelta" $ \v -> StreamDelta
    <$> v .:? "role"
    <*> v .:? "content"
    <*> v .:? "tool_calls"

-- ════════════════════════════════════════════════════════════════
-- TOOL CALL TYPES
-- ════════════════════════════════════════════════════════════════

-- | Tool call in a response (from the LLM)
data ToolCall = ToolCall
  { tcId       :: !Text           -- ^ Unique ID for this call
  , tcType     :: !Text           -- ^ Always "function"
  , tcFunction :: !FunctionCall   -- ^ The actual function call
  } deriving (Eq, Show, Generic)

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v -> ToolCall
    <$> v .: "id"
    <*> v .: "type"
    <*> v .: "function"

instance ToJSON ToolCall where
  toJSON ToolCall{..} = object
    [ "id"       .= tcId
    , "type"     .= tcType
    , "function" .= tcFunction
    ]

-- | Function call details
data FunctionCall = FunctionCall
  { fcName      :: !Text  -- ^ Name of the function called
  , fcArguments :: !Text  -- ^ JSON string of arguments
  } deriving (Eq, Show, Generic)

instance FromJSON FunctionCall where
  parseJSON = withObject "FunctionCall" $ \v -> FunctionCall
    <$> v .: "name"
    <*> v .: "arguments"

instance ToJSON FunctionCall where
  toJSON FunctionCall{..} = object
    [ "name"      .= fcName
    , "arguments" .= fcArguments
    ]

-- | Streaming tool call (may be partial)
data ToolCallDelta = ToolCallDelta
  { tcdIndex    :: !Int
  , tcdId       :: !(Maybe Text)
  , tcdType     :: !(Maybe Text)
  , tcdFunction :: !(Maybe FunctionCallDelta)
  } deriving (Eq, Show, Generic)

instance FromJSON ToolCallDelta where
  parseJSON = withObject "ToolCallDelta" $ \v -> ToolCallDelta
    <$> v .: "index"
    <*> v .:? "id"
    <*> v .:? "type"
    <*> v .:? "function"

-- | Streaming function call (arguments come in chunks)
data FunctionCallDelta = FunctionCallDelta
  { fcdName      :: !(Maybe Text)  -- ^ Present in first chunk
  , fcdArguments :: !(Maybe Text)  -- ^ Partial argument string
  } deriving (Eq, Show, Generic)

instance FromJSON FunctionCallDelta where
  parseJSON = withObject "FunctionCallDelta" $ \v -> FunctionCallDelta
    <$> v .:? "name"
    <*> v .:? "arguments"

-- ════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════

-- | API error response wrapper
data APIErrorResponse = APIErrorResponse
  { aerError :: !APIErrorDetail
  } deriving (Eq, Show, Generic)

instance FromJSON APIErrorResponse where
  parseJSON = withObject "APIErrorResponse" $ \v ->
    APIErrorResponse <$> v .: "error"

-- | API error details
data APIErrorDetail = APIErrorDetail
  { aedMessage :: !Text
  , aedType    :: !Text
  , aedCode    :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

instance FromJSON APIErrorDetail where
  parseJSON = withObject "APIErrorDetail" $ \v -> APIErrorDetail
    <$> v .: "message"
    <*> v .: "type"
    <*> v .:? "code"

-- ════════════════════════════════════════════════════════════════
-- STREAMING RESULT TYPES (shared across providers)
-- ════════════════════════════════════════════════════════════════

-- | Callback for streaming tokens
type StreamCallback = Text -> IO ()

-- | Result of a streaming completion
data StreamResult = StreamResult
  { srFullResponse :: !Text        -- ^ Complete concatenated response
  , srModel        :: !Text        -- ^ Model that generated the response
  , srTokensUsed   :: !(Maybe Int) -- ^ Token count if available
  } deriving (Eq, Show)
