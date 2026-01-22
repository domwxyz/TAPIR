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
import Tapir.UI.Types
import Tapir.UI.Attrs

-- ════════════════════════════════════════════════════════════════
-- STATUS BAR
-- ════════════════════════════════════════════════════════════════

-- | Render the complete status bar (two lines)
renderStatusBar :: AppState -> Widget Name
renderStatusBar st =
  vBox
    [ renderStatusLine st
    , renderKeyHints st
    ]

-- | Render the main status line
renderStatusLine :: AppState -> Widget Name
renderStatusLine st =
  withAttr attrStatusBar $
  hBox
    [ renderModeTabs st
    , txt " │ "
    , renderAnkiStatus (_asAnkiConnected st)
    , txt " │ "
    , renderLanguageInfo st
    , fill ' '
    , renderModelInfo st
    , txt " "
    ]

-- | Render the key hints line
renderKeyHints :: AppState -> Widget Name
renderKeyHints st =
  let errorWidget = case _asLastError st of
        Just err -> withAttr attrStatusError $ txt $ " " <> shortError err <> " "
        Nothing  -> emptyWidget
  in withAttr attrStatusBar $
     hBox
       [ txt " "
       , renderHintText
       , fill ' '
       , errorWidget
       ]

-- ════════════════════════════════════════════════════════════════
-- MODE TABS
-- ════════════════════════════════════════════════════════════════

-- | Render mode tabs with current mode highlighted
renderModeTabs :: AppState -> Widget Name
renderModeTabs st =
  let currentMode = _asCurrentMode st
      modes' = [ (Conversation, "Chat", '1')
               , (Correction, "Correct", '2')
               , (Translation, "Translate", '3')
               , (CardGeneration, "Card", '4')
               ]
  in hBox $ map (renderModeTab currentMode) modes'

-- | Render a single mode tab
renderModeTab :: Mode -> (Mode, Text, Char) -> Widget Name
renderModeTab current (mode, label, _key) =
  let isActive = current == mode
      attr = if isActive then attrStatusModeActive else attrStatusMode
      prefix = if isActive then "[" else " "
      suffix = if isActive then "]" else " "
  in withAttr attr $ txt $ prefix <> label <> suffix

-- ════════════════════════════════════════════════════════════════
-- STATUS COMPONENTS
-- ════════════════════════════════════════════════════════════════

-- | Render Anki connection status
renderAnkiStatus :: Bool -> Widget Name
renderAnkiStatus connected =
  let (attr, symbol) = if connected
        then (attrStatusAnkiOn, "● Anki")
        else (attrStatusAnkiOff, "○ Anki")
  in withAttr attr $ txt symbol

-- | Render language and level info
renderLanguageInfo :: AppState -> Widget Name
renderLanguageInfo st =
  let langMod = _asLangModule st
      langName' = languageNativeName (languageInfo langMod)
      level = learnerLevel langMod
      levelText = T.pack $ show level
  in hBox
       [ withAttr attrStatusLanguage $ txt langName'
       , txt " ("
       , withAttr attrStatusLevel $ txt levelText
       , txt ")"
       ]

-- | Render model info
renderModelInfo :: AppState -> Widget Name
renderModelInfo _st =
  -- For now, show a placeholder. In full implementation,
  -- this would come from the provider config
  withAttr attrStatusModel $ txt "z-ai/glm-4.7"

-- | Render key hint text
renderHintText :: Widget Name
renderHintText =
  txt "F1:Help Tab:Mode ^,:Settings ^N:New ^S:Sessions ^A:Card ^Q:Quit"
