{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Attrs
-- Description : Color and style attributes for the TUI
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the visual attributes (colors, styles) used
-- throughout the TAPIR TUI. All styling is centralized here for
-- easy theming and consistency.

module Tapir.UI.Attrs
  ( -- * Attribute Map
    tapirAttrMap
  , defaultAttrMap
  , lightAttrMap
  , getAttrMap

    -- * Attribute Names
    -- ** General
  , attrBorder
  , attrBorderFocus
  , attrTitle
  , attrTitleFocus
  , attrError
  , attrSuccess
  , attrWarning

    -- ** Chat
  , attrUserMessage
  , attrUserLabel
  , attrAssistantMessage
  , attrAssistantLabel
  , attrSystemMessage
  , attrTimestamp
  , attrStreaming

    -- ** Input
  , attrEditor
  , attrEditorFocus
  , attrPlaceholder

    -- ** Status Bar
  , attrStatusBar
  , attrStatusBarLeft
  , attrStatusBarRight
  , attrStatusMode
  , attrStatusModeActive
  , attrStatusLanguage
  , attrStatusLevel
  , attrStatusAnkiOn
  , attrStatusAnkiOff
  , attrStatusModel
  , attrStatusError
  , attrStatusHint

    -- ** Modals
  , attrModalBorder
  , attrModalTitle
  , attrModalKey
  , attrModalDescription

    -- ** Card Preview
  , attrCardFront
  , attrCardBack
  , attrCardTags
  , attrCardDeck

    -- ** Help
   , attrHelpKey
   , attrHelpDescription
   , attrHelpSection

     -- ** Structured Response Sections
   , attrSectionHeader
   , attrSectionDivider
   , attrCorrectionsSection
   , attrChangesSection
   , attrVocabSection
  ) where

import Brick (AttrMap, AttrName, attrMap, attrName, on, fg)
import Data.Text (Text)
import Graphics.Vty.Attributes

-- ════════════════════════════════════════════════════════════════
-- ATTRIBUTE NAMES
-- ════════════════════════════════════════════════════════════════

-- General
attrBorder, attrBorderFocus, attrTitle, attrTitleFocus :: AttrName
attrError, attrSuccess, attrWarning :: AttrName
attrBorder      = attrName "border"
attrBorderFocus = attrName "borderFocus"
attrTitle       = attrName "title"
attrTitleFocus  = attrName "titleFocus"
attrError       = attrName "error"
attrSuccess     = attrName "success"
attrWarning     = attrName "warning"

-- Chat
attrUserMessage, attrUserLabel, attrAssistantMessage :: AttrName
attrAssistantLabel, attrSystemMessage, attrTimestamp, attrStreaming :: AttrName
attrUserMessage      = attrName "chat" <> attrName "user" <> attrName "message"
attrUserLabel        = attrName "chat" <> attrName "user" <> attrName "label"
attrAssistantMessage = attrName "chat" <> attrName "assistant" <> attrName "message"
attrAssistantLabel   = attrName "chat" <> attrName "assistant" <> attrName "label"
attrSystemMessage    = attrName "chat" <> attrName "system"
attrTimestamp        = attrName "chat" <> attrName "timestamp"
attrStreaming        = attrName "chat" <> attrName "streaming"

-- Input
attrEditor, attrEditorFocus, attrPlaceholder :: AttrName
attrEditor      = attrName "editor"
attrEditorFocus = attrName "editorFocus"
attrPlaceholder = attrName "placeholder"

-- Status Bar
attrStatusBar, attrStatusBarLeft, attrStatusBarRight :: AttrName
attrStatusMode, attrStatusModeActive :: AttrName
attrStatusLanguage, attrStatusLevel, attrStatusAnkiOn :: AttrName
attrStatusAnkiOff, attrStatusModel, attrStatusError, attrStatusHint :: AttrName
attrStatusBar        = attrName "status" <> attrName "bar"
attrStatusBarLeft    = attrName "status" <> attrName "barLeft"
attrStatusBarRight   = attrName "status" <> attrName "barRight"
attrStatusMode       = attrName "status" <> attrName "mode"
attrStatusModeActive = attrName "status" <> attrName "modeActive"
attrStatusLanguage   = attrName "status" <> attrName "language"
attrStatusLevel      = attrName "status" <> attrName "level"
attrStatusAnkiOn     = attrName "status" <> attrName "ankiOn"
attrStatusAnkiOff    = attrName "status" <> attrName "ankiOff"
attrStatusModel      = attrName "status" <> attrName "model"
attrStatusError      = attrName "status" <> attrName "error"
attrStatusHint       = attrName "status" <> attrName "hint"

-- Modals
attrModalBorder, attrModalTitle, attrModalKey, attrModalDescription :: AttrName
attrModalBorder      = attrName "modal" <> attrName "border"
attrModalTitle       = attrName "modal" <> attrName "title"
attrModalKey         = attrName "modal" <> attrName "key"
attrModalDescription = attrName "modal" <> attrName "description"

-- Card Preview
attrCardFront, attrCardBack, attrCardTags, attrCardDeck :: AttrName
attrCardFront = attrName "card" <> attrName "front"
attrCardBack  = attrName "card" <> attrName "back"
attrCardTags  = attrName "card" <> attrName "tags"
attrCardDeck  = attrName "card" <> attrName "deck"

-- Help
attrHelpKey, attrHelpDescription, attrHelpSection :: AttrName
attrHelpKey         = attrName "help" <> attrName "key"
attrHelpDescription = attrName "help" <> attrName "description"
attrHelpSection     = attrName "help" <> attrName "section"

-- Structured Response Sections
attrSectionHeader, attrSectionDivider :: AttrName
attrCorrectionsSection, attrChangesSection, attrVocabSection :: AttrName
attrSectionHeader       = attrName "response" <> attrName "sectionHeader"
attrSectionDivider      = attrName "response" <> attrName "divider"
attrCorrectionsSection  = attrName "response" <> attrName "corrections"
attrChangesSection      = attrName "response" <> attrName "changes"
attrVocabSection        = attrName "response" <> attrName "vocab"

-- ════════════════════════════════════════════════════════════════
-- ATTRIBUTE MAP (Dark Theme)
-- ════════════════════════════════════════════════════════════════

-- | Default attribute map for TAPIR (dark theme)
tapirAttrMap :: AttrMap
tapirAttrMap = defaultAttrMap

-- | Dark theme attribute map
defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ -- General
    (attrBorder,       fg white)
  , (attrBorderFocus,  fg cyan)
  , (attrTitle,        withStyle (fg white) bold)
  , (attrTitleFocus,   withStyle (fg cyan) bold)
  , (attrError,        fg red)
  , (attrSuccess,      fg green)
  , (attrWarning,      fg yellow)

    -- Chat
  , (attrUserLabel,        withStyle (fg blue) bold)
  , (attrUserMessage,      fg white)
  , (attrAssistantLabel,   withStyle (fg green) bold)
  , (attrAssistantMessage, fg white)
  , (attrSystemMessage,    withStyle (fg brightBlack) italic)
  , (attrTimestamp,        fg brightBlack)
  , (attrStreaming,        withStyle (fg cyan) blink)

    -- Input
  , (attrEditor,      white `on` black)
  , (attrEditorFocus, white `on` brightBlack)
  , (attrPlaceholder, fg brightBlack)

    -- Status Bar (compact, dark theme)
  , (attrStatusBar,        fg brightBlack)
  , (attrStatusBarLeft,    fg brightBlack)
  , (attrStatusBarRight,   fg brightBlack)
  , (attrStatusMode,       fg brightBlack)
  , (attrStatusModeActive, withStyle (fg cyan) bold)
  , (attrStatusLanguage,   fg magenta)
  , (attrStatusLevel,      fg brightBlack)
  , (attrStatusAnkiOn,     fg green)
  , (attrStatusAnkiOff,    fg brightBlack)
  , (attrStatusModel,      fg brightBlack)
  , (attrStatusError,      fg red)
  , (attrStatusHint,       fg brightBlack)

    -- Modals
  , (attrModalBorder,      fg cyan)
  , (attrModalTitle,       withStyle (fg cyan) bold)
  , (attrModalKey,         withStyle (fg yellow) bold)
  , (attrModalDescription, fg white)

    -- Card Preview
  , (attrCardFront, withStyle (fg yellow) bold)
  , (attrCardBack,  fg white)
  , (attrCardTags,  fg cyan)
  , (attrCardDeck,  fg magenta)

    -- Help
   , (attrHelpKey,         withStyle (fg yellow) bold)
   , (attrHelpDescription, fg white)
   , (attrHelpSection,     withStyle (fg cyan) bold)

      -- Structured Response Sections
   , (attrSectionHeader,       withStyle (fg cyan) bold)
   , (attrSectionDivider,      fg blue)
   , (attrCorrectionsSection,  withStyle (fg magenta) bold)
   , (attrChangesSection,      withStyle (fg yellow) bold)
   , (attrVocabSection,        withStyle (fg magenta) bold)
   ]

-- ════════════════════════════════════════════════════════════════
-- ATTRIBUTE MAP (Light Theme)
-- ════════════════════════════════════════════════════════════════

-- | Light theme attribute map
lightAttrMap :: AttrMap
lightAttrMap = attrMap defAttr
  [ -- General
    (attrBorder,       fg black)
  , (attrBorderFocus,  fg blue)
  , (attrTitle,        withStyle (fg black) bold)
  , (attrTitleFocus,   withStyle (fg blue) bold)
  , (attrError,        fg red)
  , (attrSuccess,      fg green)
  , (attrWarning,      fg yellow)

    -- Chat
  , (attrUserLabel,        withStyle (fg blue) bold)
  , (attrUserMessage,      fg black)
  , (attrAssistantLabel,   withStyle (fg green) bold)
  , (attrAssistantMessage, fg black)
  , (attrSystemMessage,    withStyle (fg brightBlack) italic)
  , (attrTimestamp,        fg brightBlack)
  , (attrStreaming,        withStyle (fg blue) blink)

    -- Input
  , (attrEditor,      black `on` white)
  , (attrEditorFocus, black `on` rgbColor 240 240 240)
  , (attrPlaceholder, fg brightBlack)

    -- Status Bar (light theme)
  , (attrStatusBar,        fg brightBlack)
  , (attrStatusBarLeft,    fg brightBlack)
  , (attrStatusBarRight,   fg brightBlack)
  , (attrStatusMode,       fg brightBlack)
  , (attrStatusModeActive, withStyle (fg blue) bold)
  , (attrStatusLanguage,   fg magenta)
  , (attrStatusLevel,      fg brightBlack)
  , (attrStatusAnkiOn,     fg green)
  , (attrStatusAnkiOff,    fg brightBlack)
  , (attrStatusModel,      fg brightBlack)
  , (attrStatusError,      fg red)
  , (attrStatusHint,       fg brightBlack)

    -- Modals
  , (attrModalBorder,      fg blue)
  , (attrModalTitle,       withStyle (fg blue) bold)
  , (attrModalKey,         withStyle (fg brightBlack) bold)
  , (attrModalDescription, fg black)

    -- Card Preview
  , (attrCardFront, withStyle (fg brightBlack) bold)
  , (attrCardBack,  fg black)
  , (attrCardTags,  fg blue)
  , (attrCardDeck,  fg magenta)

    -- Help
   , (attrHelpKey,         withStyle (fg brightBlack) bold)
   , (attrHelpDescription, fg black)
   , (attrHelpSection,     withStyle (fg blue) bold)

      -- Structured Response Sections
   , (attrSectionHeader,       withStyle (fg blue) bold)
   , (attrSectionDivider,      fg brightBlack)
   , (attrCorrectionsSection,  withStyle (fg magenta) bold)
   , (attrChangesSection,      withStyle (fg brightBlack) bold)
   , (attrVocabSection,        withStyle (fg blue) bold)
   ]

-- | Get attr map by theme name ("default", "dark", or "light")
getAttrMap :: Text -> AttrMap
getAttrMap theme
  | theme == "light" = lightAttrMap
  | otherwise        = defaultAttrMap

