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
import Tapir.UI.Widgets (titledBoxFocused)

-- ════════════════════════════════════════════════════════════════
-- INPUT WIDGET
-- ════════════════════════════════════════════════════════════════

-- | Render the input editor widget
renderInput :: AppState -> Widget Name
renderInput st =
  let focused = _asFocus st == FocusInput
      editorWidget = renderEditor (txt . T.unlines) focused (_asInputEditor st)
      title = "Input"
      -- Show placeholder if editor is empty
      content = if isEditorEmpty (_asInputEditor st)
        then vLimit 3 $ renderPlaceholder st
        else vLimit 5 editorWidget
  in titledBoxFocused focused title content

-- | Render placeholder text when editor is empty
renderPlaceholder :: AppState -> Widget Name
renderPlaceholder st =
  let modeHint = case _asCurrentMode st of
        Conversation -> "Type a message to practice conversation..."
        Correction   -> "Enter text to correct..."
        Translation  -> "Enter text to translate..."
        CardGeneration -> "Enter a word or phrase to create a card..."
        CustomMode name -> "Enter input for " <> name <> "..."
  in withAttr attrPlaceholder $ txt modeHint

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
mkInputEditor = editor NameInputEditor (Just 5) ""
