{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Event
-- Description : Main event dispatcher for brick events
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event
  ( handleEvent
  ) where

import Brick

import Tapir.UI.Types
import Tapir.UI.Event.Main (handleMainEvent)
import Tapir.UI.Event.Modal (handleModalEvent)
import Tapir.UI.Event.Custom (handleCustomEvent)

-- | Main event handler - dispatches to appropriate sub-handler
handleEvent :: BrickEvent Name TapirEvent -> EventM Name AppState ()
handleEvent = \case
  AppEvent ev -> handleCustomEvent ev
  VtyEvent vtyEv -> do
    st <- get
    case _asModal st of
      NoModal -> handleMainEvent vtyEv
      _       -> handleModalEvent vtyEv
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()
