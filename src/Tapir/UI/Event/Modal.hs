{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Event.Modal
-- Description : Event handling for modal dialogs
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Modal
  ( handleModalEvent
  , handleHelpModal
  , handleCommandMenuModal
  , handleConfirmQuitModal
  , handleSettingsModal
  , handleCardPreviewModal
  , handleSessionsModal
  , handleDismissableModal
  ) where

import Brick
import Graphics.Vty.Input.Events
import Lens.Micro.Mtl ((.=))

import Tapir.Types (AnkiCard)
import Tapir.UI.Types
import Tapir.UI.Widgets (safeIndex)
import Tapir.UI.Command (executeCommand)
import Tapir.UI.Event.Session (handleSessionSelect, handleSessionDelete, handleSessionNew)
import Tapir.UI.Event.Card (handleCardPush, handleCardDiscard)
import Tapir.UI.Event.Settings (handleSettingsSave, handleSettingsReload, handleViewPrompt, cycleLearnerLevel)

-- | Handle keyboard events when a modal is open
handleModalEvent :: Event -> EventM Name AppState ()
handleModalEvent ev = do
  st <- get
  case _asModal st of
    HelpModal            -> handleHelpModal ev
    CommandMenuModal idx -> handleCommandMenuModal ev idx
    ConfirmQuitModal     -> handleConfirmQuitModal ev
    SettingsModal        -> handleSettingsModal ev
    CardPreviewModal card -> handleCardPreviewModal ev card
    SessionsModal sums idx -> handleSessionsModal ev sums idx
    ErrorModal _         -> handleDismissableModal ev
    PromptPreviewModal _ -> handleDismissableModal ev
    NoModal              -> pure ()

-- | Handle help modal - any key dismisses
handleHelpModal :: Event -> EventM Name AppState ()
handleHelpModal ev = case ev of
  EvKey _ _ -> asModal .= NoModal
  _         -> pure ()

-- | Handle command menu modal navigation and selection
handleCommandMenuModal :: Event -> Int -> EventM Name AppState ()
handleCommandMenuModal ev idx = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey (KChar 'j') [] -> asModal .= CommandMenuModal (min (idx + 1) (commandCount - 1))
  EvKey KDown []       -> asModal .= CommandMenuModal (min (idx + 1) (commandCount - 1))
  EvKey (KChar 'k') [] -> asModal .= CommandMenuModal (max (idx - 1) 0)
  EvKey KUp []         -> asModal .= CommandMenuModal (max (idx - 1) 0)
  EvKey KEnter []      -> do
    asModal .= NoModal
    case safeIndex allCommands idx of
      Just cmd -> executeCommand cmd
      Nothing  -> pure ()
  _                    -> pure ()

-- | Handle confirm quit modal
handleConfirmQuitModal :: Event -> EventM Name AppState ()
handleConfirmQuitModal ev = case ev of
  EvKey (KChar 'y') [] -> halt
  EvKey (KChar 'Y') [] -> halt
  EvKey (KChar 'n') [] -> asModal .= NoModal
  EvKey (KChar 'N') [] -> asModal .= NoModal
  EvKey KEsc []        -> asModal .= NoModal
  _                    -> pure ()

-- | Handle settings modal
handleSettingsModal :: Event -> EventM Name AppState ()
handleSettingsModal ev = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey (KChar 's') [] -> handleSettingsSave
  EvKey (KChar 'r') [] -> handleSettingsReload
  EvKey (KChar 'e') [] -> handleViewPrompt
  EvKey (KChar '+') [] -> cycleLearnerLevel 1
  EvKey (KChar '-') [] -> cycleLearnerLevel (-1)
  EvKey (KChar '=') [] -> cycleLearnerLevel 1
  _                    -> pure ()

-- | Handle card preview modal
handleCardPreviewModal :: Event -> AnkiCard -> EventM Name AppState ()
handleCardPreviewModal ev card = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey KEnter []      -> handleCardPush card
  EvKey (KChar 'd') [] -> handleCardDiscard
  _                    -> pure ()

-- | Handle sessions modal
handleSessionsModal :: Event -> [SessionSummary] -> Int -> EventM Name AppState ()
handleSessionsModal ev sums idx = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey (KChar 'j') [] -> asModal .= SessionsModal sums (min (idx + 1) (length sums - 1))
  EvKey KDown []       -> asModal .= SessionsModal sums (min (idx + 1) (length sums - 1))
  EvKey (KChar 'k') [] -> asModal .= SessionsModal sums (max (idx - 1) 0)
  EvKey KUp []         -> asModal .= SessionsModal sums (max (idx - 1) 0)
  EvKey KEnter []      -> handleSessionSelect sums idx
  EvKey (KChar 'd') [] -> handleSessionDelete sums idx
  EvKey (KChar 'n') [] -> handleSessionNew
  _                    -> pure ()

-- | Handle dismissable modals (any key closes)
handleDismissableModal :: Event -> EventM Name AppState ()
handleDismissableModal ev = case ev of
  EvKey _ _ -> asModal .= NoModal
  _         -> pure ()
