{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Service.Session
-- Description : Session lifecycle management
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- Pure functions for creating and managing sessions.

module Tapir.Service.Session
  ( -- * Session Creation
    mkNewSession
  , mkNewSessionWithMode
  , createInitialSession
  , sessionFromSummary

    -- * Session Defaults
  , defaultSessionMode
  ) where

import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)

import Tapir.Types
  ( Session(..)
  , Mode(..)
  , LanguageModule(..)
  , LanguageInfo(..)
  , LearnerLevel
  )
import Tapir.UI.Types (SessionSummary(..))
import Tapir.Db.Repository (createSession)
import Tapir.Db.Error (DbError)

-- | Default mode for new sessions
defaultSessionMode :: Mode
defaultSessionMode = Conversation

-- | Create a new session with the default mode (Conversation)
--
-- This is the primary session creation function. Use this when starting
-- the application or when the user wants a fresh session without
-- specifying a mode.
mkNewSession
  :: Text           -- ^ Session ID (UUID)
  -> LanguageModule -- ^ Active language module
  -> UTCTime        -- ^ Current timestamp
  -> Session
mkNewSession = mkNewSessionWithMode defaultSessionMode

-- | Create a new session with a specific mode
--
-- Use this when creating a session that should inherit the current mode
-- (e.g., from Ctrl+N in the main view).
mkNewSessionWithMode
  :: Mode           -- ^ Session mode
  -> Text           -- ^ Session ID (UUID)
  -> LanguageModule -- ^ Active language module
  -> UTCTime        -- ^ Current timestamp
  -> Session
mkNewSessionWithMode mode sessionId langMod now = Session
  { sessionId          = sessionId
  , sessionLanguageId  = languageId (languageInfo langMod)
  , sessionMode        = mode
  , sessionLearnerLevel = learnerLevel langMod
  , sessionCreatedAt   = now
  , sessionUpdatedAt   = now
  , sessionTitle       = Nothing
  , sessionActive      = True
  }

-- | Reconstruct a Session from a SessionSummary
--
-- Used when loading a previously saved session from the session list.
-- Note: Some fields (like createdAt) are approximated since SessionSummary
-- doesn't store them.
sessionFromSummary
  :: SessionSummary -- ^ Summary to convert
  -> LearnerLevel   -- ^ Current learner level (from language module)
  -> UTCTime        -- ^ Current timestamp (used for createdAt approximation)
  -> Session
sessionFromSummary summary level now = Session
  { sessionId          = summaryId summary
  , sessionLanguageId  = summaryLanguageId summary
  , sessionMode        = summaryMode summary
  , sessionLearnerLevel = level
  , sessionCreatedAt   = now  -- Approximation; original not in summary
  , sessionUpdatedAt   = summaryLastActivity summary
  , sessionTitle       = Just (summaryTitle summary)
  , sessionActive      = True
  }

-- | Create and persist a new session with fresh UUID
--
-- This is the main entry point for session creation at app startup.
-- It generates a UUID, creates a Session object, persists it to the database,
-- and returns the session.
createInitialSession
  :: Connection
  -> LanguageModule
  -> IO (Either DbError Session)
createInitialSession conn langMod = do
  sid <- UUID.toText <$> nextRandom
  now <- getCurrentTime
  let session = mkNewSession sid langMod now
  result <- createSession conn session
  pure $ case result of
    Left err -> Left err
    Right () -> Right session
