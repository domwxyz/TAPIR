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
  , renderThinkingIndicator

    -- * Helpers
  , formatTimestamp
  , roleLabel
  ) where

import Brick
import Brick.Widgets.Center (hCenter)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)

import Tapir.Types (Role(..), Role, Message(..), messageRole)
import Tapir.UI.Types
import Tapir.UI.Attrs
import Tapir.UI.Structured (renderStructuredResponse)
import Tapir.UI.Widgets (wrapTextDynamic)

-- | Safe unsnoc - decompose list into init and last
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

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
      pendingStructured = _asPendingStructured st

      -- Check if the last message should be rendered as structured
      -- (when we have pendingStructured and last message is from Assistant)
      (messagesToRender, lastStructured) =
        case (unsnoc messages, pendingStructured) of
          (Just (initMsgs, lastMsg), Just struct)
            | messageRole lastMsg == Assistant ->
              (initMsgs, Just struct)
          _ -> (messages, Nothing)

      messageWidgets = map renderMessage messagesToRender

      -- Last message rendered as structured (with colors/sections)
      lastWidget = case lastStructured of
        Just structured -> renderStructuredResponse structured
        Nothing -> emptyWidget

      -- Show "Thinking..." indicator while waiting for response
      thinkingWidget = case _asRequestState st of
        Requesting -> [renderThinkingIndicator]
        _ -> []

  in if null messages && null thinkingWidget && isNothing lastStructured
     then renderEmptyChat
     else vBox (messageWidgets ++ [lastWidget] ++ thinkingWidget)

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
         wrapTextDynamic messageContent
       ]

-- | Render "Thinking..." indicator while waiting for response
renderThinkingIndicator :: Widget Name
renderThinkingIndicator =
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
    , withAttr attrTimestamp $ txt "Use Tab to switch modes, Ctrl+P for commands."
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
