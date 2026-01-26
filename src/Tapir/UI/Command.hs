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
import Tapir.Core.Selection (SelectionEmpty(..))
import Tapir.Db.Repository (createSession, getRecentSessions)
import Tapir.UI.Event.Session (sessionToSummary)
import Tapir.Service.Session (mkNewSessionWithMode)
import Tapir.Core.Constants (recentSessionsLimit)

-- | Execute a command from the command menu
executeCommand :: Command -> EventM Name AppState ()
executeCommand = \case
  CmdNewSession -> do
    st <- get
    let langMod = _asLangModule st
        conn = _asDbConnection st
        mode = _asCurrentMode st
    newSid <- liftIO $ UUID.toText <$> nextRandom
    now <- liftIO getCurrentTime
    let newSession = mkNewSessionWithMode mode newSid langMod now
    _ <- liftIO $ createSession conn newSession
    asSession .= newSession
    asMessages .= []
    asInputEditor .= mkInputEditor

  CmdSessionList -> do
    st <- get
    let conn = _asDbConnection st
        chan = _asEventChannel st
    liftIO $ void $ forkIO $ do
      result <- getRecentSessions conn recentSessionsLimit
      case result of
        Right sessionsWithCount -> do
          let summaries = map sessionToSummary sessionsWithCount
          writeBChan chan (EvSessionsLoaded summaries)
        Left _ ->
          writeBChan chan (EvSessionsLoaded [])
    asModal .= SessionsModal (Left SelectionEmpty)

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
