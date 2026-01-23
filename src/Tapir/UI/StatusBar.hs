{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.UI.StatusBar
-- Description : Bottom status bar widget
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module renders the status bar at the bottom of the TUI.

module Tapir.UI.StatusBar
  ( -- * Status Bar
    renderStatusBar
  , renderKeyHints

    -- * Mode Tabs
  , renderModeTabs
  ) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T

import Tapir.Types
import Tapir.Config.Types (AppConfig(..))
import Tapir.UI.Types
import Tapir.UI.Attrs

-- ════════════════════════════════════════════════════════════════
-- STATUS BAR
-- ════════════════════════════════════════════════════════════════

-- | Render the complete status bar (single compact line)
renderStatusBar :: AppState -> Widget Name
renderStatusBar st =
  vLimit 1 $
  hBox
    [ withAttr attrStatusBarLeft $ hBox
        [ txt " "
        , renderModeIndicator st
        , txt "  "
        , renderLanguageCompact st
        ]
    , fill ' '
    , withAttr attrStatusBarRight $ hBox
        [ renderErrorOrHints st
        , txt "  "
        , renderAnkiIndicator (_asAnkiConnected st)
        , txt "  "
        , renderModelCompact st
        , txt " "
        ]
    ]

-- | Compact mode indicator (just shows current mode)
renderModeIndicator :: AppState -> Widget Name
renderModeIndicator st =
  let mode = _asCurrentMode st
      modeName = case mode of
        Conversation   -> "chat"
        Correction     -> "correct"
        Translation    -> "translate"
        CardGeneration -> "card"
        CustomMode n   -> n
  in withAttr attrStatusModeActive $ txt modeName

-- | Compact language display
renderLanguageCompact :: AppState -> Widget Name
renderLanguageCompact st =
  let langMod = _asLangModule st
      langName' = languageNativeName (languageInfo langMod)
      level = learnerLevel langMod
      levelText = T.pack $ show level
  in withAttr attrStatusLanguage $ txt $ langName' <> " " <> levelText

-- | Compact model display (shows short model name from config)
renderModelCompact :: AppState -> Widget Name
renderModelCompact st =
  let cfg = _asConfig st
      fullModel = providerModel (configProvider cfg)
      -- Extract short name: "z-ai/glm-4.7" -> "glm-4.7"
      shortModel = case T.splitOn "/" fullModel of
        [_, name] -> name
        _         -> fullModel  -- Use full name if no slash
  in withAttr attrStatusModel $ txt shortModel

-- | Compact Anki indicator
renderAnkiIndicator :: Bool -> Widget Name
renderAnkiIndicator connected =
  if connected
    then withAttr attrStatusAnkiOn $ txt "anki"
    else withAttr attrStatusAnkiOff $ txt "anki"

-- | Show error if present, otherwise show abbreviated hints
renderErrorOrHints :: AppState -> Widget Name
renderErrorOrHints st =
  case _asLastError st of
    Just err -> withAttr attrStatusError $ txt $ shortError err
    Nothing  -> withAttr attrStatusHint $ txt "tab modes  ctrl+p menu"

-- ════════════════════════════════════════════════════════════════
-- LEGACY EXPORTS (for compatibility)
-- ════════════════════════════════════════════════════════════════

-- | Render key hints (legacy, now integrated into main bar)
renderKeyHints :: AppState -> Widget Name
renderKeyHints _ = emptyWidget

-- | Render mode tabs with current mode highlighted (legacy)
renderModeTabs :: AppState -> Widget Name
renderModeTabs st =
  let currentMode = _asCurrentMode st
      modes' = [ (Conversation, "chat")
               , (Correction, "correct")
               , (Translation, "translate")
               , (CardGeneration, "card")
               ]
  in hBox $ map (renderModeTab currentMode) modes'

-- | Render a single mode tab
renderModeTab :: Mode -> (Mode, Text) -> Widget Name
renderModeTab current (mode, label) =
  let isActive = current == mode
      attr = if isActive then attrStatusModeActive else attrStatusMode
  in withAttr attr $ txt $ " " <> label <> " "
