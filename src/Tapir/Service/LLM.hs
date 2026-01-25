{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Service.LLM
-- Description : LLM interaction service layer
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module encapsulates all LLM interaction logic, keeping the UI layer
-- free from request construction, parsing, and provider details.

module Tapir.Service.LLM
  ( -- * Message Sending
    sendMessage
  , sendMessageAsync
  , SendMessageConfig(..)

    -- * Response Handling
  , LLMResult(..)
  ) where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Text (Text)

import Tapir.Types
import Tapir.Types.Response (StructuredResponse)
import Tapir.Config.Types (AppConfig)
import Tapir.Client.LLM (LLMClient, llmComplete)
import Tapir.Client.LLM.Request (buildRequestWithTools)
import Tapir.Client.LLM.Response (parseResponse, ParsedResponse(..), ParseError(..))
import Tapir.UI.Types (TapirEvent(..))

-- | Configuration for sending a message
data SendMessageConfig = SendMessageConfig
  { smcConfig     :: !AppConfig
  , smcLangModule :: !LanguageModule
  , smcClient     :: !LLMClient
  , smcMode       :: !Mode
  , smcHistory    :: ![Message]
  , smcUserMsg    :: !Message
  }

-- | Result of an LLM request
data LLMResult
  = LLMStructured !StructuredResponse
  | LLMRawText !Text
  | LLMError !TapirError
  deriving (Eq, Show)

-- | Send a message to the LLM and get a structured response
--
-- This is the main entry point for LLM interactions. It:
-- 1. Builds the appropriate request with tools for the mode
-- 2. Sends the request to the LLM
-- 3. Parses the response into structured format
--
-- All provider-specific logic is encapsulated here.
sendMessage :: SendMessageConfig -> IO LLMResult
sendMessage SendMessageConfig{..} = do
  let messages = smcHistory ++ [smcUserMsg]
      req = buildRequestWithTools smcConfig smcLangModule smcMode messages smcUserMsg

  result <- llmComplete smcClient req

  pure $ case result of
    Left err -> LLMError err
    Right resp ->
      case parseResponse smcMode resp of
        ParsedStructured sr -> LLMStructured sr
        ParsedRawText t -> LLMRawText t
        ParsedError pe -> LLMError (InternalError (peMessage pe))

-- | Send a message asynchronously, delivering results via BChan
--
-- This is the preferred method for UI integration. Results are delivered
-- as TapirEvents that the UI can handle.
sendMessageAsync
  :: SendMessageConfig
  -> BChan TapirEvent
  -> IO ()
sendMessageAsync cfg chan = void $ forkIO $ do
  result <- sendMessage cfg
  let event = case result of
        LLMStructured sr -> EvStructuredResponse sr
        LLMRawText t     -> EvStreamComplete t
        LLMError err     -> EvStreamError err
  writeBChan chan event
