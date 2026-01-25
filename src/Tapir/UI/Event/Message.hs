{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Event.Message
-- Description : Message sending event handlers
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Message
  ( handleSendMessage
  ) where

import Brick
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Lens.Micro.Mtl ((.=), (%=))

import Tapir.Types
import Tapir.UI.Types
import Tapir.UI.Input (getEditorContent, mkInputEditor)
import Tapir.Db.Repository (saveMessage, updateSessionTimestamp)
import Tapir.Service.LLM (sendMessageAsync, SendMessageConfig(..))

-- | Handle Enter key - send message
handleSendMessage :: EventM Name AppState ()
handleSendMessage = do
  st <- get
  let content = getEditorContent (_asInputEditor st)
  unless (T.null content) $ do
    unless (isRequesting (_asRequestState st)) $ do
      let sid = sessionId (_asSession st)
          mode = _asCurrentMode st
          conn = _asDbConnection st
      now <- liftIO getCurrentTime
      -- Create user message
      let msg = Message
            { messageId         = Nothing
            , messageSessionId  = sid
            , messageRole       = User
            , messageContent    = content
            , messageMode       = mode
            , messageTimestamp  = now
            , messageModel      = Nothing
            , messageProvider   = Nothing
            , messageTokensUsed = Nothing
            , messageError      = Nothing
            }

      -- Save message to database and get version with ID
      savedResult <- liftIO $ saveMessage conn msg
      let savedMsg = case savedResult of
            Right m -> m
            Left _  -> msg  -- On error, use original (will lose ID)

      asMessages %= (++ [savedMsg])
      asInputEditor .= mkInputEditor
      asRequestState .= Requesting
      asLastError .= Nothing
      asPendingStructured .= Nothing  -- Clear old structured response

      -- Update session timestamp
      _ <- liftIO $ updateSessionTimestamp conn sid

      -- Use service layer for LLM interaction
      let sendConfig = SendMessageConfig
            { smcConfig     = _asConfig st
            , smcLangModule = _asLangModule st
            , smcClient     = _asLlmClient st
            , smcMode       = mode
            , smcHistory    = _asMessages st
            , smcUserMsg    = savedMsg
            }

      liftIO $ sendMessageAsync sendConfig (_asEventChannel st)

      -- Scroll to bottom
      let vp = viewportScroll NameHistoryViewport
      vScrollToEnd vp
