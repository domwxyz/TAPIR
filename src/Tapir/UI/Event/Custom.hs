{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Event.Custom
-- Description : Handle custom TapirEvent events from BChan
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Custom
  ( handleCustomEvent
  ) where

import Brick
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Data.Text.Lazy.Builder as B
import Lens.Micro.Mtl ((.=), (%=))

import Tapir.Types
import Tapir.Types.Response (StructuredResponse(..), responseToText)
import Tapir.UI.Types
import Tapir.Db.Repository (saveMessage, saveCard, updateSessionTimestamp)
import Tapir.Service.Card (extractCardFromResponse, cardResponseToAnkiCard)
import Tapir.Core.Constants (providerNameOpenRouter)

-- | Handle custom events from BChan
handleCustomEvent :: TapirEvent -> EventM Name AppState ()
handleCustomEvent = \case
  EvStreamChunk chunk -> do
    asStreamingText %= (<> B.fromText chunk)
    asRequestState .= Streaming
    vScrollToEnd (viewportScroll NameHistoryViewport)

  EvStreamComplete fullResponse -> handleStreamComplete fullResponse

  EvStructuredResponse structured -> handleStructuredResponse structured

  EvStreamError err -> do
    asRequestState .= RequestFailed err
    asLastError .= Just err

  EvAnkiStatusUpdate connected ->
    asAnkiConnected .= connected

  EvCardPushResult result -> case result of
    Right _noteId -> do
      asPendingCard .= Nothing
      asModal .= NoModal
    Left err ->
      asLastError .= Just err

  EvSessionsLoaded summaries ->
    asModal .= SessionsModal summaries 0

  EvMessagesLoaded msgs ->
    asMessages .= msgs

  EvLanguageReloaded newLangMod ->
    asLangModule .= newLangMod

  EvConfigReloaded newConfig ->
    asConfig .= newConfig

  EvTick -> pure ()

-- | Handle stream completion
handleStreamComplete :: Text -> EventM Name AppState ()
handleStreamComplete fullResponse = do
  st <- get
  let mode = _asCurrentMode st
      sid = sessionId (_asSession st)
      conn = _asDbConnection st
  now <- liftIO getCurrentTime

  let msg = Message
        { messageId         = Nothing
        , messageSessionId  = sid
        , messageRole       = Assistant
        , messageContent    = fullResponse
        , messageMode       = mode
        , messageTimestamp  = now
        , messageModel      = Nothing
        , messageProvider   = Just providerNameOpenRouter
        , messageTokensUsed = Nothing
        , messageError      = Nothing
        }

  savedResult <- liftIO $ saveMessage conn msg
  let savedMsg = either (const msg) id savedResult

  asMessages %= (++ [savedMsg])
  asRequestState .= Idle
  asStreamingText .= mempty

  _ <- liftIO $ updateSessionTimestamp conn sid

  when (mode == CardGeneration) $ do
    let langMod = _asLangModule st
        langId = languageId (languageInfo langMod)
        msgId = messageId savedMsg
    case extractCardFromResponse langId sid msgId fullResponse now of
      Just card -> do
        savedCardResult <- liftIO $ saveCard conn card
        case savedCardResult of
          Right savedCard -> do
            asPendingCard .= Just savedCard
            asModal .= CardPreviewModal savedCard
          Left _ ->
            asPendingCard .= Just card
      Nothing -> pure ()

-- | Handle structured response from tool calls
handleStructuredResponse :: StructuredResponse -> EventM Name AppState ()
handleStructuredResponse structured = do
  st <- get
  let mode = _asCurrentMode st
      sid = sessionId (_asSession st)
      conn = _asDbConnection st
  now <- liftIO getCurrentTime

  let textContent = responseToText structured

  let msg = Message
        { messageId         = Nothing
        , messageSessionId  = sid
        , messageRole       = Assistant
        , messageContent    = textContent
        , messageMode       = mode
        , messageTimestamp  = now
        , messageModel      = Nothing
        , messageProvider   = Just providerNameOpenRouter
        , messageTokensUsed = Nothing
        , messageError      = Nothing
        }

  savedResult <- liftIO $ saveMessage conn msg
  let savedMsg = either (const msg) id savedResult

  asMessages %= (++ [savedMsg])
  asPendingStructured .= Just structured
  asRequestState .= Idle

  case structured of
    SRCard cardResp -> do
      let langId = languageId (languageInfo (_asLangModule st))
          card = cardResponseToAnkiCard cardResp sid langId now
      asPendingCard .= Just card
      asModal .= CardPreviewModal card
    _ -> pure ()

  vScrollToEnd (viewportScroll NameHistoryViewport)
