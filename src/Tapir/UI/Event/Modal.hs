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
import Tapir.Core.Selection (Selection, SelectionEmpty(..))
import qualified Tapir.Core.Selection as Sel
import Tapir.UI.Command (executeCommand)
import Tapir.UI.Event.Session (handleSessionSelectFromSelection, handleSessionDeleteFromSelection, handleSessionNew)
import Tapir.UI.Event.Card (handleCardPush, handleCardDiscard)
import Tapir.UI.Event.Settings (handleSettingsSave, handleSettingsReload, handleViewPrompt, cycleLearnerLevel)

-- | Handle keyboard events when a modal is open
handleModalEvent :: Event -> EventM Name AppState ()
handleModalEvent ev = do
  st <- get
  case _asModal st of
    HelpModal            -> handleHelpModal ev
    CommandMenuModal sel -> handleCommandMenuModal ev sel
    ConfirmQuitModal     -> handleConfirmQuitModal ev
    SettingsModal        -> handleSettingsModal ev
    CardPreviewModal card -> handleCardPreviewModal ev card
    SessionsModal eSel   -> handleSessionsModal ev eSel
    ErrorModal _         -> handleDismissableModal ev
    PromptPreviewModal _ -> handleDismissableModal ev
    NoModal              -> pure ()

-- | Handle help modal - any key dismisses
handleHelpModal :: Event -> EventM Name AppState ()
handleHelpModal ev = case ev of
  EvKey _ _ -> asModal .= NoModal
  _         -> pure ()

-- | Handle command menu modal navigation and selection
handleCommandMenuModal :: Event -> Selection Command -> EventM Name AppState ()
handleCommandMenuModal ev sel = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey (KChar 'j') [] -> asModal .= CommandMenuModal (Sel.moveNext sel)
  EvKey KDown []       -> asModal .= CommandMenuModal (Sel.moveNext sel)
  EvKey (KChar 'k') [] -> asModal .= CommandMenuModal (Sel.movePrev sel)
  EvKey KUp []         -> asModal .= CommandMenuModal (Sel.movePrev sel)
  EvKey KEnter []      -> do
    asModal .= NoModal
    executeCommand (Sel.selected sel)
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
handleSessionsModal :: Event -> Either SelectionEmpty (Selection SessionSummary) -> EventM Name AppState ()
handleSessionsModal ev eSel = case ev of
  EvKey KEsc []        -> asModal .= NoModal
  EvKey (KChar 'n') [] -> handleSessionNew
  _ -> case eSel of
    Left SelectionEmpty ->
      -- Empty list - only allow close or new
      pure ()
    Right sel -> handleSessionsModalWithSelection ev sel

-- | Handle sessions modal when we have items
handleSessionsModalWithSelection :: Event -> Selection SessionSummary -> EventM Name AppState ()
handleSessionsModalWithSelection ev sel = case ev of
  EvKey (KChar 'j') [] -> asModal .= SessionsModal (Right $ Sel.moveNext sel)
  EvKey KDown []       -> asModal .= SessionsModal (Right $ Sel.moveNext sel)
  EvKey (KChar 'k') [] -> asModal .= SessionsModal (Right $ Sel.movePrev sel)
  EvKey KUp []         -> asModal .= SessionsModal (Right $ Sel.movePrev sel)
  EvKey KEnter []      -> handleSessionSelectFromSelection sel
  EvKey (KChar 'd') [] -> handleSessionDeleteFromSelection sel
  _                    -> pure ()

-- | Handle dismissable modals (any key closes)
handleDismissableModal :: Event -> EventM Name AppState ()
handleDismissableModal ev = case ev of
  EvKey _ _ -> asModal .= NoModal
  _         -> pure ()
