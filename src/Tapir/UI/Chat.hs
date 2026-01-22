{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.UI.Chat
-- Description : Chat history display widget
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module renders the chat history in a scrollable viewport.

module Tapir.UI.Chat
  ( -- * Chat Widget
    renderChat
  , renderChatHistory
  , renderMessage
  , renderStreamingMessage

    -- * Helpers
  , formatTimestamp
  , roleLabel
  ) where

import Brick
import Brick.Widgets.Center (hCenter)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Lens.Micro ((^.))

import Tapir.Types
import Tapir.UI.Types
import Tapir.UI.Attrs

-- | Wrap text to a given width, returning multiple lines
-- Handles word boundaries and preserves existing newlines
wrapTextToWidth :: Int -> Text -> [Text]
wrapTextToWidth width content
  | width <= 0 = [content]
  | T.null content = [""]
  | otherwise  = concatMap (wrapLine width) (T.lines content)
  where
    -- Wrap a single line (no newlines) to the given width
    wrapLine :: Int -> Text -> [Text]
    wrapLine w line
      | T.length line <= w = [line]
      | otherwise = wrapWords w (T.words line) [] ""

    -- Wrap words accumulating into lines
    wrapWords :: Int -> [Text] -> [Text] -> Text -> [Text]
    wrapWords _ [] acc current =
      if T.null current then acc else acc ++ [current]
    wrapWords w (word:rest) acc current
      | T.null current =
          -- Starting a new line
          if T.length word > w
            -- Word is longer than width, break it
            then let (pre, post) = T.splitAt w word
                 in wrapWords w (post:rest) (acc ++ [pre]) ""
            else wrapWords w rest acc word
      | T.length current + 1 + T.length word <= w =
          -- Word fits on current line (with space)
          wrapWords w rest acc (current <> " " <> word)
      | otherwise =
          -- Word doesn't fit, start new line
          if T.length word > w
            then let (pre, post) = T.splitAt w word
                 in wrapWords w (post:rest) (acc ++ [current, pre]) ""
            else wrapWords w rest (acc ++ [current]) word

-- | Helper to wrap text in a viewport-safe way using Widget monad
-- Uses the available context width to wrap text dynamically
wrapText :: Text -> Widget n
wrapText t = Widget Fixed Fixed $ do
    ctx <- getContext
    -- Account for borders and padding (subtract 4 for safety margin)
    let availWidth = max 20 (availWidth' ctx - 4)
        wrapped = wrapTextToWidth availWidth t
    render $ vBox $ map txt wrapped
  where
    availWidth' ctx = ctx ^. availWidthL

-- ════════════════════════════════════════════════════════════════
-- MAIN CHAT WIDGET
-- ════════════════════════════════════════════════════════════════

-- | Render the complete chat widget with history viewport (no border)
renderChat :: AppState -> Widget Name
renderChat st =
  let content = renderChatHistory st
  -- Chat area takes all available space above input
  in padLeftRight 1 $
     viewport NameHistoryViewport Vertical content

-- | Render chat history (all messages + streaming if active)
renderChatHistory :: AppState -> Widget Name
renderChatHistory st =
  let messages = _asMessages st
      messageWidgets = map renderMessage messages
      streamingWidget = case _asRequestState st of
        Streaming -> [renderStreamingMessage (_asStreamingText st)]
        Requesting -> [renderRequestingIndicator]
        _ -> []
  in if null messages && null streamingWidget
     then renderEmptyChat
     else vBox (messageWidgets ++ streamingWidget)

-- ════════════════════════════════════════════════════════════════
-- MESSAGE RENDERING
-- ════════════════════════════════════════════════════════════════

-- | Render a single message
renderMessage :: Message -> Widget Name
renderMessage Message{..} =
  let (labelAttr, msgAttr) = roleAttrs messageRole
      label = roleLabel messageRole
      timestamp = formatTimestamp messageTimestamp
  in padBottom (Pad 1) $
     vBox
       [ hBox
           [ withAttr labelAttr $ txt label
           , txt " "
           , withAttr attrTimestamp $ txt $ "[" <> timestamp <> "]"
           ]
       , padLeft (Pad 2) $
         withAttr msgAttr $
         wrapText messageContent
       ]

-- | Render streaming message in progress
renderStreamingMessage :: Text -> Widget Name
renderStreamingMessage content =
  padBottom (Pad 1) $
  vBox
    [ hBox
        [ withAttr attrAssistantLabel $ txt "TAPIR"
        , txt " "
        , withAttr attrStreaming $ txt "[streaming...]"
        ]
    , padLeft (Pad 2) $
      hBox
        [ withAttr attrAssistantMessage $ wrapText content
        , withAttr attrStreaming $ txt "▌"  -- Cursor
        ]
    ]

-- | Render requesting indicator (waiting for response)
renderRequestingIndicator :: Widget Name
renderRequestingIndicator =
  padBottom (Pad 1) $
  vBox
    [ hBox
        [ withAttr attrAssistantLabel $ txt "TAPIR"
        , txt " "
        , withAttr attrStreaming $ txt "[thinking...]"
        ]
    , padLeft (Pad 2) $
      withAttr attrStreaming $ txt "..."
    ]

-- | Render empty chat placeholder
renderEmptyChat :: Widget Name
renderEmptyChat =
  padTopBottom 2 $
  hCenter $
  vBox
    [ withAttr attrSystemMessage $ txt "Start a conversation!"
    , txt " "
    , withAttr attrTimestamp $ txt "Type a message and press Enter to send."
    , withAttr attrTimestamp $ txt "Use Tab to switch modes, F1 for help."
    ]

-- ════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Get attribute names for a role
roleAttrs :: Role -> (AttrName, AttrName)
roleAttrs User      = (attrUserLabel, attrUserMessage)
roleAttrs Assistant = (attrAssistantLabel, attrAssistantMessage)
roleAttrs System    = (attrSystemMessage, attrSystemMessage)

-- | Get display label for a role
roleLabel :: Role -> Text
roleLabel User      = "You"
roleLabel Assistant = "TAPIR"
roleLabel System    = "System"

-- | Format timestamp for display (HH:MM format)
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%H:%M"
