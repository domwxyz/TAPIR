{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Draw
-- Description : Main UI drawing function
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Draw
  ( drawUI
  ) where

import Brick

import Tapir.UI.Types (Name, AppState(..))
import Tapir.UI.Attrs (attrBorder)
import Tapir.UI.Chat (renderChat)
import Tapir.UI.Input (renderInput)
import Tapir.UI.StatusBar (renderStatusBar)
import Tapir.UI.Modals (renderModal)

-- | Draw the entire UI
drawUI :: AppState -> [Widget Name]
drawUI st =
  let -- Chat takes most of the screen
      chatArea = renderChat st
      -- Thin separator line
      separator = withAttr attrBorder $ vLimit 1 $ fill '─'
      -- Compact input area (2-3 lines)
      inputArea = renderInput st
      -- Single-line status bar at bottom
      statusBar = renderStatusBar st
      -- Main layout: chat fills remaining space, input and status at bottom
      mainUI = vBox
        [ chatArea
        , separator
        , inputArea
        , statusBar
        ]
      modalOverlay = renderModal st
  in [modalOverlay, mainUI]
