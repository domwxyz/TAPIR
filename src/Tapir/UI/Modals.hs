{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.UI.Modals
-- Description : Modal dialog widgets
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides modal dialog widgets for help, settings,
-- card preview, session list, and confirmations.

module Tapir.UI.Modals
  ( -- * Modal Rendering
    renderModal

    -- * Individual Modals
  , renderHelpModal
  , renderCommandMenuModal
  , renderSettingsModal
  , renderCardPreviewModal
  , renderSessionsModal
  , renderConfirmQuitModal
  , renderErrorModal
  , renderPromptPreviewModal
  ) where

import Brick
import Brick.Widgets.Border (hBorder, borderWithLabel)
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, defaultTimeLocale)

import Tapir.Types
import Tapir.UI.Types
import Tapir.UI.Attrs
import Tapir.UI.Widgets (keyHintRow)

-- ════════════════════════════════════════════════════════════════
-- MODAL DISPATCHER
-- ════════════════════════════════════════════════════════════════

-- | Render the current modal (returns emptyWidget if no modal)
renderModal :: AppState -> Widget Name
renderModal st = case _asModal st of
  NoModal              -> emptyWidget
  HelpModal            -> renderHelpModal
  CommandMenuModal idx -> renderCommandMenuModal idx
  SettingsModal        -> renderSettingsModal st
  PromptPreviewModal prompt -> renderPromptPreviewModal prompt
  CardPreviewModal card -> renderCardPreviewModal card
  SessionsModal sums idx -> renderSessionsModal sums idx
  ConfirmQuitModal     -> renderConfirmQuitModal
  ErrorModal err       -> renderErrorModal err

-- ════════════════════════════════════════════════════════════════
-- HELP MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render help/keybindings modal
renderHelpModal :: Widget Name
renderHelpModal =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " Help ") $
  padAll 1 $
  hLimit 60 $
  vBox
    [ withAttr attrHelpSection $ txt "NAVIGATION"
    , renderKeybind "Tab"       "Next mode"
    , renderKeybind "Shift+Tab" "Previous mode"
    , renderKeybind "1/2/3/4"   "Jump to mode"
    , renderKeybind "PageUp/Dn" "Scroll history"
    , txt " "
    , withAttr attrHelpSection $ txt "ACTIONS"
    , renderKeybind "Enter"     "Send message"
    , renderKeybind "Ctrl+N"    "New session"
    , renderKeybind "Ctrl+S"    "Session list"
    , renderKeybind "Ctrl+A"    "Show pending card"
    , txt " "
    , withAttr attrHelpSection $ txt "APPLICATION"
    , renderKeybind "Ctrl+P"    "Command menu"
    , renderKeybind "F1"        "This help"
    , renderKeybind "F2"        "Settings"
    , renderKeybind "Esc"       "Close modal"
    , renderKeybind "Ctrl+C"    "Cancel / Quit"
    , renderKeybind "Ctrl+Q"    "Quit"
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ txt "Press any key to close"
    ]

-- | Render a single keybinding line
renderKeybind :: Text -> Text -> Widget Name
renderKeybind key desc =
  hBox
    [ hLimit 14 $ withAttr attrHelpKey $ txt $ "  " <> key
    , withAttr attrHelpDescription $ txt $ " " <> desc
    ]

-- ════════════════════════════════════════════════════════════════
-- COMMAND MENU MODAL
-- ════════════════════════════════════════════════════════════════

-- | Command definition with name, keybind, and description
data Command = Command
  { cmdName    :: !Text
  , cmdKeybind :: !Text
  , cmdDesc    :: !Text
  }

-- | All available commands in the menu
commands :: [Command]
commands =
  [ Command "New Session"    "Ctrl+N" "Start a fresh conversation"
  , Command "Session List"   "Ctrl+S" "Browse and switch sessions"
  , Command "Settings"       "F2"     "Open settings panel"
  , Command "Show Card"      "Ctrl+A" "View pending Anki card"
  , Command "Help"           "F1"     "Show all keybindings"
  , Command "Quit"           "Ctrl+Q" "Exit TAPIR"
  ]

-- | Render command menu modal
renderCommandMenuModal :: Int -> Widget Name
renderCommandMenuModal selectedIdx =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " Commands ") $
  padAll 1 $
  hLimit 40 $
  vBox
    [ vBox $ zipWith (renderCommandRow selectedIdx) [0..] commands
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ keyHintRow
        [ ("Enter", "Execute")
        , ("j/k", "Navigate")
        , ("Esc", "Close")
        ]
    ]

-- | Render a single command row
renderCommandRow :: Int -> Int -> Command -> Widget Name
renderCommandRow selectedIdx idx Command{..} =
  let isSelected = selectedIdx == idx
      attr = if isSelected then attrStatusModeActive else attrModalDescription
      prefix = if isSelected then "> " else "  "
  in withAttr attr $
     hBox
       [ txt prefix
       , txt cmdName
       , padLeft Max $ withAttr attrHelpKey $ txt cmdKeybind
       ]

-- ════════════════════════════════════════════════════════════════
-- SETTINGS MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render settings modal
renderSettingsModal :: AppState -> Widget Name
renderSettingsModal st =
  let langMod = _asLangModule st
      info = languageInfo langMod
      level = learnerLevel langMod
  in centerLayer $
     withAttr attrModalBorder $
     withBorderStyle unicodeRounded $
     borderWithLabel (withAttr attrModalTitle $ txt " Settings ") $
     padAll 1 $
     hLimit 50 $
     vBox
       [ withAttr attrHelpSection $ txt "LANGUAGE"
       , hBorder
       , settingRow "Active:" (languageName info)
       , hBox
           [ hLimit 14 $ withAttr attrModalKey $ txt $ "  Level:"
           , txt " "
           , txt $ T.pack (show level) <> " (" <> learnerLevelDescription level <> ") "
           , withAttr attrHelpKey $ txt "[+/-]"
           ]
       , settingRow "Variant:" (maybe "neutral" id (languageVariant info))
       , txt " "
       , withAttr attrHelpSection $ txt "PROVIDER"
       , hBorder
       , settingRow "Provider:" "OpenRouter"
       , settingRow "Model:" "z-ai/glm-4.7"
       , settingRow "Max tokens:" "4096"
       , txt " "
       , withAttr attrHelpSection $ txt "ANKI"
       , hBorder
       , settingRow "Deck:" (ankiDefaultDeck (ankiConfig langMod))
       , settingRow "Status:" (if _asAnkiConnected st then "Connected" else "Disconnected")
       , txt " "
      , hBorder
      , padTop (Pad 1) $ hCenter $ keyHintRow
          [ ("S", "Save")
          , ("R", "Reload")
          , ("E", "View prompt")
          , ("Esc", "Close")
          ]
       ]

-- | Render a settings row
settingRow :: Text -> Text -> Widget Name
settingRow label value =
  hBox
    [ hLimit 14 $ withAttr attrModalKey $ txt $ "  " <> label
    , txt " "
    , txt value
    ]

-- ════════════════════════════════════════════════════════════════
-- CARD PREVIEW MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render card preview modal
renderCardPreviewModal :: AnkiCard -> Widget Name
renderCardPreviewModal AnkiCard{..} =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " Card Preview ") $
  padAll 1 $
  hLimit 60 $
  vBox
    [ withAttr attrHelpSection $ txt "FRONT"
    , hBorder
    , padAll 1 $ withAttr attrCardFront $ txtWrap cardFront
    , txt " "
    , withAttr attrHelpSection $ txt "BACK"
    , hBorder
    , padAll 1 $ withAttr attrCardBack $ txtWrap cardBack
    , txt " "
    , hBox
        [ withAttr attrModalKey $ txt "Deck: "
        , withAttr attrCardDeck $ txt cardDeck
        ]
    , hBox
        [ withAttr attrModalKey $ txt "Tags: "
        , withAttr attrCardTags $ txt $ T.intercalate ", " cardTags
        ]
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ keyHintRow
        [ ("Enter", "Push to Anki")
        , ("E", "Edit")
        , ("D", "Discard")
        , ("Esc", "Close")
        ]
    ]

-- ════════════════════════════════════════════════════════════════
-- SESSIONS MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render sessions list modal
renderSessionsModal :: [SessionSummary] -> Int -> Widget Name
renderSessionsModal summaries selectedIdx =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " Sessions ") $
  padAll 1 $
  hLimit 70 $
  vLimit 20 $
  vBox
    [ vBox $ zipWith (renderSessionRow selectedIdx) [0..] summaries
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ keyHintRow
        [ ("Enter", "Select")
        , ("j/k", "Navigate")
        , ("n", "New")
        , ("d", "Delete")
        , ("Esc", "Close")
        ]
    ]

-- | Render a single session row
renderSessionRow :: Int -> Int -> SessionSummary -> Widget Name
renderSessionRow selectedIdx idx SessionSummary{..} =
  let isSelected = selectedIdx == idx
      attr = if isSelected then attrStatusModeActive else attrModalDescription
      prefix = if isSelected then "> " else "  "
      timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" summaryLastActivity
      countStr = T.pack $ show summaryMessageCount
  in withAttr attr $
     hBox
       [ txt prefix
       , hLimit 30 $ txt $ T.take 28 summaryTitle <> if T.length summaryTitle > 28 then ".." else ""
       , txt " "
       , hLimit 12 $ txt summaryLanguageId
       , txt " "
       , hLimit 5 $ txt $ countStr <> " msgs"
       , txt " "
       , txt timeStr
       ]

-- ════════════════════════════════════════════════════════════════
-- CONFIRM QUIT MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render confirm quit modal
renderConfirmQuitModal :: Widget Name
renderConfirmQuitModal =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " Quit ") $
  padAll 2 $
  vBox
    [ txt "Are you sure you want to quit?"
    , txt " "
    , hCenter $ keyHintRow
        [ ("Y", "Quit")
        , ("N/Esc", "Cancel")
        ]
    ]

-- ════════════════════════════════════════════════════════════════
-- ERROR MODAL
-- ════════════════════════════════════════════════════════════════

-- | Render error display modal
renderErrorModal :: TapirError -> Widget Name
renderErrorModal err =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrError $ txt " Error ") $
  padAll 1 $
  hLimit 60 $
  vBox
    [ padAll 1 $ withAttr attrError $ txtWrap $ displayError err
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ txt "Press any key to close"
    ]

-- | Render system prompt preview modal
renderPromptPreviewModal :: Text -> Widget Name
renderPromptPreviewModal promptText =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt " System Prompt ") $
  padAll 1 $
  hLimit 70 $
  vLimit 20 $
  vBox
    [ txtWrap promptText
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ txt "Press any key to close"
    ]
