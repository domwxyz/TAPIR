{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.Request
-- Description : Build LLM requests with appropriate tools and prompts
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.Request
  ( -- * Request Building
    buildRequest
  , buildRequestWithTools
  
    -- * Configuration
  , RequestConfig(..)
  , defaultRequestConfig
  ) where

import Data.Text (Text)

import Tapir.Types (Mode(..), Message, Role(..), messageContent, messageRole)
import Tapir.Types.Language (LanguageModule, languageInfo, learnerLevel)
import Tapir.Config.Types (AppConfig, configProvider)
import Tapir.Types.Provider (ProviderConfig, providerModel)
import Tapir.Config.Loader (getSystemPrompt)
import Tapir.Client.LLM.Types
import Tapir.Client.LLM.Tools
import Tapir.Core.Constants (maxHistoryMessages)

-- | Configuration for request building
data RequestConfig = RequestConfig
  { rcUseTools       :: !Bool   -- ^ Enable tool calling
  , rcForceTools     :: !Bool   -- ^ Force tool use (vs auto)
  , rcStream         :: !Bool   -- ^ Enable streaming
  , rcIncludeHistory :: !Bool   -- ^ Include conversation history
  , rcMaxHistory     :: !Int    -- ^ Max history messages to include
  } deriving (Eq, Show)

-- | Default request configuration
defaultRequestConfig :: RequestConfig
defaultRequestConfig = RequestConfig
  { rcUseTools       = True
  , rcForceTools     = True   -- We want guaranteed structure
  , rcStream         = False  -- Tool calls don't stream well
  , rcIncludeHistory = True
  , rcMaxHistory     = 20
  }

-- | Build a request with tools for structured output
buildRequestWithTools
  :: AppConfig
  -> LanguageModule
  -> Mode
  -> [Message]      -- ^ Conversation history
  -> Message        -- ^ Current user message
  -> ChatRequest
buildRequestWithTools config langMod mode history currentMsg =
  let -- Get system prompt for mode
      systemPrompt = getSystemPrompt langMod mode
      systemMsg = maybe [] (\p -> [ChatMessage "system" p]) systemPrompt
      
      -- Convert history to chat messages (limit to recent)
      historyMsgs = map toChatMessage (takeEnd maxHistoryMessages history)
      
      -- Current message
      currentChatMsg = toChatMessage currentMsg
      
      -- All messages in order
      allMsgs = systemMsg ++ historyMsgs ++ [currentChatMsg]
      
      -- Get tool for this mode
      tool = toolForMode mode
      toolName = toolNameForMode mode
      
      -- Build request
      baseReq = defaultChatRequest (getModel config) allMsgs
      
      -- Determine tool choice
      toolChoice = case (rcForceTools defaultRequestConfig, toolName) of
        (True, Just name) -> Just (ToolChoiceForced name)
        _ -> case (rcUseTools defaultRequestConfig, tool) of
          (True, _) -> Just ToolChoiceRequired
          _ -> Nothing
      
      -- Add tools if enabled
      tools = case (rcUseTools defaultRequestConfig, tool) of
        (True, Just t) -> Just [t]
        _ -> Nothing
      
  in baseReq
      { crTools = tools
      , crToolChoice = toolChoice
      , crStream = False  -- Disable streaming for tool calls
      }
  where
    toChatMessage :: Message -> ChatMessage
    toChatMessage msg = ChatMessage
      { cmRole = case messageRole msg of
          User      -> "user"
          Assistant -> "assistant"
          System    -> "system"
      , cmContent = messageContent msg
      }
    
    getModel :: AppConfig -> Text
    getModel cfg = providerModel (configProvider cfg)
    
    takeEnd :: Int -> [a] -> [a]
    takeEnd n xs = drop (max 0 (length xs - n)) xs

-- | Build a simple request without tools (for streaming conversation)
buildRequest
  :: AppConfig
  -> LanguageModule
  -> Mode
  -> [Message]
  -> Message
  -> ChatRequest
buildRequest config langMod mode history currentMsg =
  let req = buildRequestWithTools config langMod mode history currentMsg
  in req
      { crTools = Nothing
      , crToolChoice = Nothing
      , crStream = True
      }
