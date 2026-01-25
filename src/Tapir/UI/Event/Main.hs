{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Event.Main
-- Description : Event handling for main (non-modal) view
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Main
  ( handleMainEvent
  , cycleMode
  ) where

import Brick
import Brick.Widgets.Edit (handleEditorEvent)
import Data.List (elemIndex)
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Lens.Micro.Mtl ((.=), use)

import Tapir.Types (Mode(..))
import Tapir.UI.Types
import Tapir.UI.Input (getEditorContent)
import Tapir.UI.Widgets (safeIndexWithDefault)
import Tapir.UI.Event.Session (handleNewSession, handleLoadSessions)
import Tapir.UI.Event.Message (handleSendMessage)

-- | Handle keyboard events in main view (no modal)
handleMainEvent :: Event -> EventM Name AppState ()
handleMainEvent ev = case ev of
  -- Quit
  EvKey (KChar 'q') [MCtrl] -> asModal .= ConfirmQuitModal
  EvKey (KChar 'c') [MCtrl] -> do
    st <- get
    case _asRequestState st of
      Requesting -> asRequestState .= Idle
      Streaming  -> asRequestState .= Idle
      _          -> asModal .= ConfirmQuitModal

  -- Command menu (Ctrl+P)
  EvKey (KChar 'p') [MCtrl] -> asModal .= CommandMenuModal 0

  -- Help (F1)
  EvKey (KFun 1) [] -> asModal .= HelpModal

  -- Settings (F2)
  EvKey (KFun 2) [] -> asModal .= SettingsModal

  -- Mode switching
  EvKey (KChar '\t') [] -> cycleMode 1
  EvKey KBackTab []     -> cycleMode (-1)

  -- New session
  EvKey (KChar 'n') [MCtrl] -> handleNewSession

  -- Sessions list
  EvKey (KChar 's') [MCtrl] -> handleLoadSessions

  -- Show pending card
  EvKey (KChar 'a') [MCtrl] -> do
    st <- get
    case _asPendingCard st of
      Just card -> asModal .= CardPreviewModal card
      Nothing   -> pure ()

  -- Focus switching and scrolling
  EvKey KPageUp [] -> do
    asFocus .= FocusHistory
    let vp = viewportScroll NameHistoryViewport
    vScrollPage vp Up

  EvKey KPageDown [] -> do
    asFocus .= FocusHistory
    let vp = viewportScroll NameHistoryViewport
    vScrollPage vp Down

  EvKey KUp [] -> handleUpArrow
  EvKey KDown [] -> handleDownArrow

  -- Escape returns focus to input
  EvKey KEsc [] -> asFocus .= FocusInput

  -- Send message
  EvKey KEnter [] -> handleSendMessage

  -- Editor events (fallback)
  _ -> do
    asFocus .= FocusInput
    zoom asInputEditor $ handleEditorEvent (VtyEvent ev)

-- | Handle up arrow - scroll or edit depending on context
handleUpArrow :: EventM Name AppState ()
handleUpArrow = do
  st <- get
  let editorEmpty = T.null (getEditorContent (_asInputEditor st))
  if _asFocus st == FocusHistory || editorEmpty
    then vScrollBy (viewportScroll NameHistoryViewport) (-1)
    else zoom asInputEditor $ handleEditorEvent (VtyEvent (EvKey KUp []))

-- | Handle down arrow - scroll or edit depending on context
handleDownArrow :: EventM Name AppState ()
handleDownArrow = do
  st <- get
  let editorEmpty = T.null (getEditorContent (_asInputEditor st))
  if _asFocus st == FocusHistory || editorEmpty
    then vScrollBy (viewportScroll NameHistoryViewport) 1
    else zoom asInputEditor $ handleEditorEvent (VtyEvent (EvKey KDown []))

-- | Cycle through modes
cycleMode :: Int -> EventM Name AppState ()
cycleMode delta = do
  current <- use asCurrentMode
  let modes = [Conversation, Correction, Translation, CardGeneration]
      currentIdx = maybe 0 id (elemIndex current modes)
      newIdx = (currentIdx + delta) `mod` length modes
      newMode = safeIndexWithDefault Conversation modes newIdx
  asCurrentMode .= newMode
