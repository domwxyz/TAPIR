{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Service.Message
-- Description : Message creation and processing logic
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- Pure functions for creating and processing messages, separated from
-- IO and UI concerns.

module Tapir.Service.Message
  ( -- * Message Creation
    mkAssistantMessage
  , mkUserMessage

    -- * Message Processing
  , AssistantMessageParams(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Tapir.Types (Message(..), Role(..), Mode)

-- | Parameters for creating an assistant message
data AssistantMessageParams = AssistantMessageParams
  { ampSessionId :: !Text
  , ampContent   :: !Text
  , ampMode      :: !Mode
  , ampTimestamp :: !UTCTime
  , ampModel     :: !(Maybe Text)
  , ampProvider  :: !(Maybe Text)
  , ampTokens    :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Create an assistant message from parameters (pure function)
mkAssistantMessage :: AssistantMessageParams -> Message
mkAssistantMessage AssistantMessageParams{..} = Message
  { messageId         = Nothing
  , messageSessionId  = ampSessionId
  , messageRole       = Assistant
  , messageContent    = ampContent
  , messageMode       = ampMode
  , messageTimestamp  = ampTimestamp
  , messageModel      = ampModel
  , messageProvider   = ampProvider
  , messageTokensUsed = ampTokens
  , messageError      = Nothing
  }

-- | Create a user message (pure function)
mkUserMessage :: Text -> Text -> Mode -> UTCTime -> Message
mkUserMessage sessionId content mode timestamp = Message
  { messageId         = Nothing
  , messageSessionId  = sessionId
  , messageRole       = User
  , messageContent    = content
  , messageMode       = mode
  , messageTimestamp  = timestamp
  , messageModel      = Nothing
  , messageProvider   = Nothing
  , messageTokensUsed = Nothing
  , messageError      = Nothing
  }
