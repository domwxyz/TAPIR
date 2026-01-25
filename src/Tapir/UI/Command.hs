{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Command
-- Description : Command execution logic
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Command
  ( executeCommand
  ) where

import Brick
import Brick.BChan (writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Lens.Micro.Mtl ((.=))

import Tapir.Types
import Tapir.UI.Types
import Tapir.UI.Input (mkInputEditor)
import Tapir.Db.Repository (createSession, getRecentSessions)
import Tapir.UI.Event.Session (sessionToSummary)

-- | Execute a command from the command menu
executeCommand :: Command -> EventM Name AppState ()
executeCommand = \case
  CmdNewSession -> do
    st <- get
    let langMod = _asLangModule st
        conn = _asDbConnection st
    newSid <- liftIO $ UUID.toText <$> nextRandom
    now <- liftIO getCurrentTime
    let newSession = Session
          { sessionId          = newSid
          , sessionLanguageId  = languageId (languageInfo langMod)
          , sessionMode        = _asCurrentMode st
          , sessionLearnerLevel = learnerLevel langMod
          , sessionCreatedAt   = now
          , sessionUpdatedAt   = now
          , sessionTitle       = Nothing
          , sessionActive      = True
          }
    _ <- liftIO $ createSession conn newSession
    asSession .= newSession
    asMessages .= []
    asInputEditor .= mkInputEditor

  CmdSessionList -> do
    st <- get
    let conn = _asDbConnection st
        chan = _asEventChannel st
    liftIO $ void $ forkIO $ do
      result <- getRecentSessions conn 50
      case result of
        Right sessionsWithCount -> do
          let summaries = map sessionToSummary sessionsWithCount
          writeBChan chan (EvSessionsLoaded summaries)
        Left _ ->
          writeBChan chan (EvSessionsLoaded [])
    asModal .= SessionsModal [] 0

  CmdSettings ->
    asModal .= SettingsModal

  CmdShowCard -> do
    st <- get
    case _asPendingCard st of
      Just card -> asModal .= CardPreviewModal card
      Nothing   -> pure ()

  CmdHelp ->
    asModal .= HelpModal

  CmdQuit ->
    asModal .= ConfirmQuitModal
