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

import Tapir.Types
import Tapir.UI.Types
import Tapir.UI.Attrs
import Tapir.UI.Widgets (titledBoxFocused)

-- | Helper to wrap text in a viewport-safe way
-- txtWrap is infinite-height, so we use txt with manual line breaks
wrapText :: Text -> Widget n
wrapText t = txt t

-- ════════════════════════════════════════════════════════════════
-- MAIN CHAT WIDGET
-- ════════════════════════════════════════════════════════════════

-- | Render the complete chat widget with history viewport
renderChat :: AppState -> Widget Name
renderChat st =
  let focused = _asFocus st == FocusHistory
      title = "History"
      content = renderChatHistory st
  in titledBoxFocused focused title $
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
