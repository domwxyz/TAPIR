{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Tapir.UI.Types
-- Description : Types for brick TUI state and events
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the core types for the brick TUI:
-- - Resource names for widgets
-- - Focus management
-- - Modal dialogs
-- - Request state
-- - Main application state

module Tapir.UI.Types
  ( -- * Resource Names
    Name(..)

    -- * Focus
  , Focus(..)

    -- * Modals
  , Modal(..)

    -- * Commands
  , Command(..)
  , commandName
  , commandKeybind
  , commandDescription
  , allCommands
  , mkCommandSelection

    -- * Request State
  , RequestState(..)
  , isRequesting

    -- * Application State
  , AppState(..)

    -- * Lenses
  , asSession
  , asMessages
  , asInputEditor
  , asCurrentMode
  , asFocus
  , asModal
  , asRequestState
  , asPendingCard
  , asPendingStructured
  , asAnkiConnected
  , asLastError
  , asConfig
  , asLangModule
  , asLlmClient
  , asAnkiClient
  , asDbConnection
  , asEventChannel

    -- * Custom Events
  , TapirEvent(..)

    -- * Session Summary (for session list)
  , SessionSummary(..)
  ) where

import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection)
import Lens.Micro.TH (makeLenses)

import Data.List.NonEmpty (NonEmpty(..))

import Tapir.Types
import Tapir.Types.Response (StructuredResponse)
import Tapir.Config.Types (AppConfig)
import Tapir.Client.LLM (LLMClient)
import Tapir.Client.Anki (AnkiClient)
import Tapir.Core.Selection (Selection, SelectionEmpty(..))
import qualified Tapir.Core.Selection as Sel

-- ════════════════════════════════════════════════════════════════
-- RESOURCE NAMES
-- ════════════════════════════════════════════════════════════════

-- | Resource names for brick widgets
-- Used to identify widgets for focus, event handling, and rendering
data Name
  = NameHistoryViewport     -- ^ Chat history scrollable viewport
  | NameInputEditor         -- ^ Text input editor
  | NameSessionList         -- ^ Session selection list
  | NameCardPreviewDialog   -- ^ Card preview modal
  | NameSettingsDialog      -- ^ Settings modal
  | NameHelpDialog          -- ^ Help overlay
  | NameLanguageSelector    -- ^ Language dropdown in settings
  | NameLevelSelector       -- ^ Level dropdown in settings
  | NameModelSelector       -- ^ Model dropdown in settings
  deriving (Eq, Ord, Show)

-- ════════════════════════════════════════════════════════════════
-- FOCUS
-- ════════════════════════════════════════════════════════════════

-- | Which area of the UI has focus
data Focus
  = FocusInput    -- ^ Input editor has focus (typing)
  | FocusHistory  -- ^ History viewport has focus (scrolling)
  deriving (Eq, Show)

-- ════════════════════════════════════════════════════════════════
-- MODALS
-- ════════════════════════════════════════════════════════════════

-- | Modal dialog state
data Modal
  = NoModal                                                    -- ^ No modal visible
  | HelpModal                                                  -- ^ Help/keybindings overlay
  | CommandMenuModal !(Selection Command)                      -- ^ Command palette with selection
  | SessionsModal !(Either SelectionEmpty (Selection SessionSummary))  -- ^ Session list (may be empty during loading)
  | CardPreviewModal !AnkiCard                                 -- ^ Card preview/edit before push
  | SettingsModal                                              -- ^ Settings dialog
  | PromptPreviewModal !Text                                   -- ^ System prompt preview modal
  | ConfirmQuitModal                                           -- ^ Confirm quit dialog
  | ErrorModal !TapirError                                     -- ^ Error display modal
  deriving (Eq, Show)

-- ════════════════════════════════════════════════════════════════
-- COMMANDS
-- ════════════════════════════════════════════════════════════════

-- | Available commands in the command menu
data Command
  = CmdNewSession
  | CmdSessionList
  | CmdSettings
  | CmdShowCard
  | CmdHelp
  | CmdQuit
  deriving (Eq, Show, Enum, Bounded)

-- | Get display name for a command
commandName :: Command -> Text
commandName = \case
  CmdNewSession  -> "New Session"
  CmdSessionList -> "Session List"
  CmdSettings    -> "Settings"
  CmdShowCard    -> "Show Card"
  CmdHelp        -> "Help"
  CmdQuit        -> "Quit"

-- | Get keybinding for a command
commandKeybind :: Command -> Text
commandKeybind = \case
  CmdNewSession  -> "Ctrl+N"
  CmdSessionList -> "Ctrl+S"
  CmdSettings    -> "F2"
  CmdShowCard    -> "Ctrl+A"
  CmdHelp        -> "F1"
  CmdQuit        -> "Ctrl+Q"

-- | Get description for a command
commandDescription :: Command -> Text
commandDescription = \case
  CmdNewSession  -> "Start a fresh conversation"
  CmdSessionList -> "Browse and switch sessions"
  CmdSettings    -> "Open settings panel"
  CmdShowCard    -> "View pending Anki card"
  CmdHelp        -> "Show all keybindings"
  CmdQuit        -> "Exit TAPIR"

-- | All commands in menu order
allCommands :: [Command]
allCommands = [minBound .. maxBound]

-- | Create a command menu selection (guaranteed non-empty since commands are static)
mkCommandSelection :: Selection Command
mkCommandSelection = Sel.fromNonEmpty (CmdNewSession :| [CmdSessionList, CmdSettings, CmdShowCard, CmdHelp, CmdQuit])

-- ════════════════════════════════════════════════════════════════
-- REQUEST STATE
-- ════════════════════════════════════════════════════════════════

-- | State of an LLM request
data RequestState
  = Idle                    -- ^ No active request
  | Requesting              -- ^ Request sent, waiting for response
  | RequestFailed !TapirError  -- ^ Request failed with error
  deriving (Eq, Show)

-- | Check if request in progress
isRequesting :: RequestState -> Bool
isRequesting Idle             = False
isRequesting (RequestFailed _) = False
isRequesting _                = True

-- ════════════════════════════════════════════════════════════════
-- SESSION SUMMARY
-- ════════════════════════════════════════════════════════════════

-- | Summary of a session for the session list
data SessionSummary = SessionSummary
  { summaryId           :: !Text      -- ^ Session UUID
  , summaryTitle        :: !Text      -- ^ Session title/name
  , summaryLanguageId   :: !Text      -- ^ Language ID
  , summaryMode         :: !Mode      -- ^ Session mode
  , summaryMessageCount :: !Int       -- ^ Number of messages
  , summaryLastActivity :: !UTCTime   -- ^ Last update time
  } deriving (Eq, Show)

-- ════════════════════════════════════════════════════════════════
-- CUSTOM EVENTS
-- ════════════════════════════════════════════════════════════════

-- | Custom events for the brick app
-- These are sent via BChan for async operations
data TapirEvent
  = EvRequestError !TapirError      -- ^ Request error occurred
  | EvStructuredResponse !StructuredResponse  -- ^ Parsed structured response from tools
  | EvAnkiStatusUpdate !Bool        -- ^ Anki connection status changed
  | EvCardPushResult !(Either TapirError Integer)  -- ^ Card push result (note ID)
  | EvSessionsLoaded ![SessionSummary]  -- ^ Sessions loaded from DB
  | EvMessagesLoaded ![Message]     -- ^ Messages loaded for session
  | EvLanguageReloaded !LanguageModule  -- ^ Language config reloaded
  | EvConfigReloaded !AppConfig     -- ^ Main config reloaded
  | EvTick                          -- ^ Periodic tick for animations/status
  deriving (Show)

-- ════════════════════════════════════════════════════════════════
-- APPLICATION STATE
-- ════════════════════════════════════════════════════════════════

-- | Main application state
data AppState = AppState
  { _asSession            :: !Session              -- ^ Current active session
  , _asMessages           :: ![Message]            -- ^ Messages in current session
  , _asInputEditor        :: !(Editor Text Name)   -- ^ Input text editor
  , _asCurrentMode       :: !Mode                 -- ^ Current interaction mode
  , _asFocus             :: !Focus                -- ^ Current focus area
  , _asModal             :: !Modal                -- ^ Current modal state
  , _asRequestState      :: !RequestState         -- ^ LLM request state
  , _asPendingCard       :: !(Maybe AnkiCard)     -- ^ Card pending push to Anki
  , _asPendingStructured  :: !(Maybe StructuredResponse)  -- ^ Parsed structured response
  , _asAnkiConnected     :: !Bool                 -- ^ Anki connection status
  , _asLastError         :: !(Maybe TapirError)   -- ^ Last error for status display
  , _asConfig            :: !AppConfig            -- ^ Main configuration
  , _asLangModule        :: !LanguageModule       -- ^ Active language module
  , _asLlmClient         :: !LLMClient            -- ^ LLM client instance
  , _asAnkiClient        :: !AnkiClient           -- ^ Anki client instance
  , _asDbConnection      :: !Connection           -- ^ Database connection
  , _asEventChannel      :: !(BChan TapirEvent)   -- ^ Event channel for async ops
  }

-- Generate lenses for AppState
makeLenses ''AppState

-- Note: We can't derive Eq/Show for AppState because LLMClient and Connection
-- don't have those instances, which is fine for brick apps
