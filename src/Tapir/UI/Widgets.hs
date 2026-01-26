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

    -- * Text Wrapping
  , wrapTextToWidth
  , wrapTextDynamic

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

    -- * Safe List Operations
  , safeIndex
  , safeIndexWithDefault
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.))
import Text.Wrap (wrapText, defaultWrapSettings, WrapSettings(..))

import Tapir.UI.Attrs
import Tapir.UI.Types (Name)
import Tapir.Core.Constants (minTextWrapWidth, textWrapHorizontalPadding)

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
  let borderStyle = if focused then attrBorderFocus else attrBorder
      titleStyle  = if focused then attrTitleFocus else attrTitle
  in withAttr borderStyle $
     withBorderStyle unicode $
     borderWithLabel (withAttr titleStyle $ txt $ " " <> title <> " ") $
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
      -- Safe: idx is always valid due to mod, but we use safe access anyway
      char = safeIndexWithDefault '⠋' frames idx
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

-- ════════════════════════════════════════════════════════════════
-- TEXT WRAPPING
-- ════════════════════════════════════════════════════════════════

-- | Wrap text to a given width, returning multiple lines.
-- Handles word boundaries and preserves existing newlines.
wrapTextToWidth :: Int -> Text -> [Text]
wrapTextToWidth width content
  | width <= 0 = [content]
  | T.null content = [""]
  | otherwise  = concatMap (wrapLine width) (T.lines content)
  where
    wrapLine :: Int -> Text -> [Text]
    wrapLine w line
      | T.length line <= w = [line]
      | otherwise = wrapWords w (T.words line) [] ""

    wrapWords :: Int -> [Text] -> [Text] -> Text -> [Text]
    wrapWords _ [] acc current =
      if T.null current then acc else acc ++ [current]
    wrapWords w (word:rest) acc current
      | T.null current =
          if T.length word > w
            then let (pre, post) = T.splitAt w word
                 in wrapWords w (post:rest) (acc ++ [pre]) ""
            else wrapWords w rest acc word
      | T.length current + 1 + T.length word <= w =
          wrapWords w rest acc (current <> " " <> word)
      | otherwise =
          if T.length word > w
            then let (pre, post) = T.splitAt w word
                 in wrapWords w (post:rest) (acc ++ [current, pre]) ""
            else wrapWords w rest (acc ++ [current]) word

-- | Wrap text dynamically based on available viewport width.
-- Must be Greedy horizontally to properly fill available space before wrapping.
wrapTextDynamic :: Text -> Widget Name
wrapTextDynamic t = Widget Greedy Fixed $ do
    ctx <- getContext
    let availWidth = max minTextWrapWidth (ctx ^. availWidthL - textWrapHorizontalPadding)
        wrapped = wrapTextToWidth availWidth t
    render $ vBox $ map txt wrapped

-- ════════════════════════════════════════════════════════════════
-- SAFE LIST OPERATIONS
-- ════════════════════════════════════════════════════════════════

-- | Safe list indexing, returns Nothing for out-of-bounds
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)

-- | Safe list indexing with default value
safeIndexWithDefault :: a -> [a] -> Int -> a
safeIndexWithDefault def xs n =
  case safeIndex xs n of
    Just x  -> x
    Nothing -> def
