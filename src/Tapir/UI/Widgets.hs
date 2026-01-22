{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Widgets
-- Description : Reusable brick widgets for TAPIR
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides reusable widget primitives used across the TUI.

module Tapir.UI.Widgets
  ( -- * Box Drawing
    titledBox
  , titledBoxFocused
  , simpleBox

    -- * Text Rendering
  , wrappedText
  , centeredText
  , paddedText
  , labeledRow

    -- * Separators
  , horizontalSeparator
  , verticalSeparator

    -- * Status Indicators
  , statusDot
  , loadingSpinner

    -- * Key Hints
  , keyHint
  , keyHintRow

    -- * Modals
  , modalBox
  , dialogBox
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Text (Text)
import qualified Data.Text as T
import Text.Wrap (wrapText, defaultWrapSettings, WrapSettings(..))

import Tapir.UI.Attrs
import Tapir.UI.Types (Name)

-- ════════════════════════════════════════════════════════════════
-- BOX DRAWING
-- ════════════════════════════════════════════════════════════════

-- | Box with a title in the top border
titledBox :: Text -> Widget Name -> Widget Name
titledBox title content =
  withBorderStyle unicode $
  borderWithLabel (withAttr attrTitle $ txt $ " " <> title <> " ") $
  content

-- | Box with a title, highlighted when focused
titledBoxFocused :: Bool -> Text -> Widget Name -> Widget Name
titledBoxFocused focused title content =
  let borderAttr = if focused then attrBorderFocus else attrBorder
      titleAttr  = if focused then attrTitleFocus else attrTitle
  in withAttr borderAttr $
     withBorderStyle unicode $
     borderWithLabel (withAttr titleAttr $ txt $ " " <> title <> " ") $
     content

-- | Simple box without title
simpleBox :: Widget Name -> Widget Name
simpleBox content =
  withBorderStyle unicode $
  border content

-- ════════════════════════════════════════════════════════════════
-- TEXT RENDERING
-- ════════════════════════════════════════════════════════════════

-- | Text widget that wraps at word boundaries
wrappedText :: Int -> Text -> Widget Name
wrappedText width t =
  let wrapped = wrapText wrapSettings width t
      wrapSettings = defaultWrapSettings
        { preserveIndentation = False
        , breakLongWords = True
        }
  in txtWrap wrapped

-- | Center text horizontally
centeredText :: Text -> Widget Name
centeredText = hCenter . txt

-- | Text with padding
paddedText :: Int -> Int -> Text -> Widget Name
paddedText h v t = padLeftRight h $ padTopBottom v $ txt t

-- | A labeled row: "Label: value"
labeledRow :: Text -> Widget Name -> Widget Name
labeledRow label value =
  hBox
    [ withAttr attrTitle $ txt (label <> ": ")
    , value
    ]

-- ════════════════════════════════════════════════════════════════
-- SEPARATORS
-- ════════════════════════════════════════════════════════════════

-- | Horizontal line separator
horizontalSeparator :: Widget Name
horizontalSeparator = vLimit 1 $ fill '-'

-- | Vertical line separator
verticalSeparator :: Widget Name
verticalSeparator = hLimit 1 $ fill '|'

-- ════════════════════════════════════════════════════════════════
-- STATUS INDICATORS
-- ════════════════════════════════════════════════════════════════

-- | Colored status dot
-- True = green (connected/success), False = red (disconnected/error)
statusDot :: Bool -> Widget Name
statusDot connected =
  let attr = if connected then attrStatusAnkiOn else attrStatusAnkiOff
  in withAttr attr $ txt "●"

-- | Simple loading spinner (rotates through characters)
-- Takes a tick count and displays appropriate frame
loadingSpinner :: Int -> Widget Name
loadingSpinner tick =
  let frames = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
      idx = tick `mod` length frames
      char = frames !! idx
  in withAttr attrStreaming $ txt (T.singleton char)

-- ════════════════════════════════════════════════════════════════
-- KEY HINTS
-- ════════════════════════════════════════════════════════════════

-- | Single key hint: [Key]:Action
keyHint :: Text -> Text -> Widget Name
keyHint key action =
  hBox
    [ withAttr attrHelpKey $ txt key
    , txt ":"
    , withAttr attrHelpDescription $ txt action
    ]

-- | Row of key hints separated by spaces
keyHintRow :: [(Text, Text)] -> Widget Name
keyHintRow hints =
  hBox $ intersperse (txt "  ") $ map (uncurry keyHint) hints
  where
    intersperse :: a -> [a] -> [a]
    intersperse _ []     = []
    intersperse _ [x]    = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

-- ════════════════════════════════════════════════════════════════
-- MODALS
-- ════════════════════════════════════════════════════════════════

-- | Modal box with title, centered on screen
modalBox :: Text -> Widget Name -> Widget Name
modalBox title content =
  centerLayer $
  withAttr attrModalBorder $
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr attrModalTitle $ txt $ " " <> title <> " ") $
  padAll 1 content

-- | Dialog box with title and action hints at bottom
dialogBox :: Text -> Widget Name -> [(Text, Text)] -> Widget Name
dialogBox title content actions =
  modalBox title $
  vBox
    [ content
    , txt " "
    , hBorder
    , padTop (Pad 1) $ hCenter $ keyHintRow actions
    ]
