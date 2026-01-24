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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Lens.Micro ((^.))

import Tapir.Types (Role(..), Role, Message(..), messageRole)
import Tapir.UI.Types
import Tapir.UI.Attrs
import Tapir.UI.Structured (renderStructuredResponse)

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
-- NOTE: Must be Greedy horizontally to properly fill available space before wrapping
wrapText :: Text -> Widget n
wrapText t = Widget Greedy Fixed $ do
    ctx <- getContext
    -- Account for borders and padding (subtract 2 for safety margin)
    let availWidth = max 20 (availWidth' ctx - 2)
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
      pendingStructured = _asPendingStructured st

      -- Check if the last message should be rendered as structured
      -- (when we have pendingStructured and last message is from Assistant)
      (messagesToRender, lastStructured) =
        case (safeLast messages, pendingStructured) of
          (Just lastMsg, Just struct)
            | messageRole lastMsg == Assistant ->
              (safeInit messages, Just struct)
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
  where
    isNothing Nothing = True
    isNothing _       = False

    -- Safe last element
    safeLast :: [a] -> Maybe a
    safeLast []     = Nothing
    safeLast [x]    = Just x
    safeLast (_:xs) = safeLast xs

    -- Safe init (all but last)
    safeInit :: [a] -> [a]
    safeInit []     = []
    safeInit [_]    = []
    safeInit (x:xs) = x : safeInit xs

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
