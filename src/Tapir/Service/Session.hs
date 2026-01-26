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
  , sessionFromSummary

    -- * Session Defaults
  , defaultSessionMode
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Tapir.Types
  ( Session(..)
  , Mode(..)
  , LanguageModule(..)
  , LanguageInfo(..)
  , LearnerLevel
  )
import Tapir.UI.Types (SessionSummary(..))

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
