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
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text.Lazy.Builder as B
import Lens.Micro.Mtl ((.=), (%=))

import Tapir.Types
import Tapir.Types.Response (StructuredResponse(..), responseToText)
import Tapir.UI.Types
import qualified Tapir.Core.Selection as Sel
import Tapir.Db.Repository (saveMessage, saveCard, updateSessionTimestamp)
import Tapir.Service.Card (extractCardFromResponse, cardResponseToAnkiCard)
import Tapir.Service.Message (mkAssistantMessage, AssistantMessageParams(..))
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
    asModal .= SessionsModal (Sel.fromList summaries)

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
  now <- liftIO getCurrentTime

  -- Pure: Create the message using service
  let params = AssistantMessageParams
        { ampSessionId = sessionId (_asSession st)
        , ampContent   = fullResponse
        , ampMode      = _asCurrentMode st
        , ampTimestamp = now
        , ampModel     = Nothing
        , ampProvider  = Just providerNameOpenRouter
        , ampTokens    = Nothing
        }
      msg = mkAssistantMessage params

  -- IO: Persist the message
  let conn = _asDbConnection st
  savedResult <- liftIO $ saveMessage conn msg
  let savedMsg = either (const msg) id savedResult

  -- State: Update UI state
  updateStateAfterMessage savedMsg

  -- IO: Update session timestamp
  _ <- liftIO $ updateSessionTimestamp conn (sessionId (_asSession st))

  -- Conditional: Handle card generation mode
  handleCardGenerationIfNeeded savedMsg fullResponse

-- | Update state after receiving a message (extracted helper)
updateStateAfterMessage :: Message -> EventM Name AppState ()
updateStateAfterMessage msg = do
  asMessages %= (++ [msg])
  asRequestState .= Idle
  asStreamingText .= mempty

-- | Handle card extraction if in card generation mode (extracted helper)
handleCardGenerationIfNeeded :: Message -> Text -> EventM Name AppState ()
handleCardGenerationIfNeeded savedMsg responseText = do
  st <- get
  when (_asCurrentMode st == CardGeneration) $ do
    let langMod = _asLangModule st
        langId = languageId (languageInfo langMod)
        sid = sessionId (_asSession st)
        msgId = messageId savedMsg
    now <- liftIO getCurrentTime
    case extractCardFromResponse langId sid msgId responseText now of
      Just card -> do
        let conn = _asDbConnection st
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
  now <- liftIO getCurrentTime

  -- Pure: Create the message using service
  let textContent = responseToText structured
      params = AssistantMessageParams
        { ampSessionId = sessionId (_asSession st)
        , ampContent   = textContent
        , ampMode      = _asCurrentMode st
        , ampTimestamp = now
        , ampModel     = Nothing
        , ampProvider  = Just providerNameOpenRouter
        , ampTokens    = Nothing
        }
      msg = mkAssistantMessage params

  -- IO: Persist the message
  let conn = _asDbConnection st
  savedResult <- liftIO $ saveMessage conn msg
  let savedMsg = either (const msg) id savedResult

  -- State: Update UI state
  asMessages %= (++ [savedMsg])
  asPendingStructured .= Just structured
  asRequestState .= Idle

  -- Handle card response specifically
  handleStructuredCard structured now

  vScrollToEnd (viewportScroll NameHistoryViewport)

-- | Handle card extraction from structured response (extracted helper)
handleStructuredCard :: StructuredResponse -> UTCTime -> EventM Name AppState ()
handleStructuredCard (SRCard cardResp) now = do
  st <- get
  let langId = languageId (languageInfo (_asLangModule st))
      sid = sessionId (_asSession st)
      card = cardResponseToAnkiCard cardResp sid langId now
  asPendingCard .= Just card
  asModal .= CardPreviewModal card
handleStructuredCard _ _ = pure ()
