{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Event.Session
-- Description : Session management event handlers
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Session
  ( handleNewSession
  , handleLoadSessions
  , handleSessionSelect
  , handleSessionDelete
  , handleSessionSelectFromSelection
  , handleSessionDeleteFromSelection
  , handleSessionNew
  , sessionToSummary
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
import Tapir.Core.Selection (Selection, SelectionEmpty(..))
import qualified Tapir.Core.Selection as Sel
import Tapir.Db.Repository (createSession, getRecentSessions, getSessionMessages, deleteSession)
import Tapir.Service.Session (mkNewSessionWithMode, sessionFromSummary)
import Tapir.Core.Constants (recentSessionsLimit)

-- | Handle Ctrl+N - create new session from main view
handleNewSession :: EventM Name AppState ()
handleNewSession = do
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

-- | Handle Ctrl+S - load sessions list
handleLoadSessions :: EventM Name AppState ()
handleLoadSessions = do
  st <- get
  let conn = _asDbConnection st
      chan = _asEventChannel st
  -- Trigger async load of sessions
  liftIO $ void $ forkIO $ do
    result <- getRecentSessions conn recentSessionsLimit
    case result of
      Right sessionsWithCount -> do
        let summaries = map sessionToSummary sessionsWithCount
        writeBChan chan (EvSessionsLoaded summaries)
      Left _ ->
        -- On error, show empty list
        writeBChan chan (EvSessionsLoaded [])
  -- Show modal immediately with loading state (empty selection)
  asModal .= SessionsModal (Left SelectionEmpty)

-- | Handle session selection from sessions modal (using Selection)
handleSessionSelectFromSelection :: Selection SessionSummary -> EventM Name AppState ()
handleSessionSelectFromSelection sel = do
  let selectedSummary = Sel.selected sel
  st <- get
  let sid = summaryId selectedSummary
      conn = _asDbConnection st
      chan = _asEventChannel st

  -- Load messages for the selected session asynchronously
  liftIO $ void $ forkIO $ do
    result <- getSessionMessages conn sid
    case result of
      Right msgs -> writeBChan chan (EvMessagesLoaded msgs)
      Left _ -> writeBChan chan (EvMessagesLoaded [])

  -- Update session info in state immediately
  now <- liftIO getCurrentTime
  let level = learnerLevel (_asLangModule st)
      newSession = sessionFromSummary selectedSummary level now
  asSession .= newSession
  asCurrentMode .= summaryMode selectedSummary
  asMessages .= []
  asModal .= NoModal

-- | Handle session deletion from sessions modal (using Selection)
handleSessionDeleteFromSelection :: Selection SessionSummary -> EventM Name AppState ()
handleSessionDeleteFromSelection sel = do
  let selectedSummary = Sel.selected sel
  st <- get
  let sid = summaryId selectedSummary
      conn = _asDbConnection st
      chan = _asEventChannel st
  -- Delete session asynchronously
  liftIO $ void $ forkIO $ do
    _ <- deleteSession conn sid
    -- Reload sessions list
    result <- getRecentSessions conn recentSessionsLimit
    case result of
      Right sessionsWithCount ->
        writeBChan chan (EvSessionsLoaded (map sessionToSummary sessionsWithCount))
      Left _ ->
        writeBChan chan (EvSessionsLoaded [])
  asModal .= NoModal

-- | Handle session selection from sessions modal (legacy, delegates to Selection-based version)
handleSessionSelect :: [SessionSummary] -> Int -> EventM Name AppState ()
handleSessionSelect sums idx =
  case Sel.fromList sums of
    Left SelectionEmpty -> pure ()
    Right sel -> handleSessionSelectFromSelection (Sel.moveToIndex idx sel)

-- | Handle session deletion from sessions modal (legacy, delegates to Selection-based version)
handleSessionDelete :: [SessionSummary] -> Int -> EventM Name AppState ()
handleSessionDelete sums idx =
  case Sel.fromList sums of
    Left SelectionEmpty -> pure ()
    Right sel -> handleSessionDeleteFromSelection (Sel.moveToIndex idx sel)

-- | Handle new session creation from sessions modal
handleSessionNew :: EventM Name AppState ()
handleSessionNew = do
  st <- get
  let langMod = _asLangModule st
      conn = _asDbConnection st
      mode = _asCurrentMode st
  newSid <- liftIO $ UUID.toText <$> nextRandom
  now <- liftIO getCurrentTime
  let newSession = mkNewSessionWithMode mode newSid langMod now

  liftIO $ void $ createSession conn newSession

  asSession .= newSession
  asMessages .= []
  asInputEditor .= mkInputEditor
  asModal .= NoModal

-- | Convert a Session and message count to a SessionSummary
sessionToSummary :: (Session, Int) -> SessionSummary
sessionToSummary (sess, count) = SessionSummary
  { summaryId = sessionId sess
  , summaryTitle = case sessionTitle sess of
      Just t  -> t
      Nothing -> "Untitled Session"
  , summaryLanguageId = sessionLanguageId sess
  , summaryMode = sessionMode sess
  , summaryMessageCount = count
  , summaryLastActivity = sessionUpdatedAt sess
  }
