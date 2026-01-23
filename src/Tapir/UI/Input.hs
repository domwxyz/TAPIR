{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Input
-- Description : Text input editor widget
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module handles the text input editor for composing messages.

module Tapir.UI.Input
  ( -- * Input Widget
    renderInput

    -- * Editor Helpers
  , getEditorContent
  , clearEditor
  , mkInputEditor
  ) where

import Brick
import Brick.Widgets.Edit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper (textZipper)

import Tapir.Types (Mode(..))
import Tapir.UI.Types
import Tapir.UI.Attrs

-- ════════════════════════════════════════════════════════════════
-- INPUT WIDGET
-- ════════════════════════════════════════════════════════════════

-- | Render the input editor widget (compact, borderless design)
renderInput :: AppState -> Widget Name
renderInput st =
  let focused = _asFocus st == FocusInput
      editorWidget = renderEditor (txt . T.unlines) focused (_asInputEditor st)
      prompt = renderPrompt st
      -- Show placeholder if editor is empty, otherwise show editor
      content = if isEditorEmpty (_asInputEditor st)
        then renderPlaceholder st
        else editorWidget
      -- Compact input area: prompt > content (2-3 lines max)
  in vLimit 3 $ hBox
       [ prompt
       , padLeft (Pad 1) content
       ]

-- | Render the input prompt indicator (shows current mode)
renderPrompt :: AppState -> Widget Name
renderPrompt st =
  let promptChar = case _asCurrentMode st of
        Conversation   -> ">"
        Correction     -> "✓"
        Translation    -> "→"
        CardGeneration -> "+"
        CustomMode _   -> ">"
  in withAttr attrStatusModeActive $ txt promptChar

-- | Render placeholder text when editor is empty
renderPlaceholder :: AppState -> Widget Name
renderPlaceholder st =
  let modeHint = case _asCurrentMode st of
        Conversation   -> "Type to chat..."
        Correction     -> "Enter text to correct..."
        Translation    -> "Enter text to translate..."
        CardGeneration -> "Word or phrase for card..."
        CustomMode _   -> "Enter input..."
  in withAttr attrPlaceholder $ txt modeHint

-- | Default number of visible rows for the input editor
-- This value balances screen space with the need to see multi-line input
-- without scrolling. A value of 5 allows most short messages to be visible
-- while keeping the chat history area large enough for readability.
defaultEditorRows :: Int
defaultEditorRows = 5

-- ════════════════════════════════════════════════════════════════
-- EDITOR HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Get the content of the editor as a single text
getEditorContent :: Editor Text Name -> Text
getEditorContent ed =
  let lines' = getEditContents ed
  in T.strip $ T.unlines lines'

-- | Check if editor is empty
isEditorEmpty :: Editor Text Name -> Bool
isEditorEmpty ed =
  let content = getEditorContent ed
  in T.null content

-- | Clear the editor content
clearEditor :: Editor Text Name -> Editor Text Name
clearEditor = applyEdit (const $ textZipper [] Nothing)

-- | Create a new input editor
mkInputEditor :: Editor Text Name
mkInputEditor = editor NameInputEditor (Just defaultEditorRows) ""
