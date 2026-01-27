{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.UI.App
-- Description : Main brick application definition
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the brick application core:
-- - Main app definition
-- - Application runner
-- - Initial state construction

module Tapir.UI.App
  ( -- * Application
    tapirApp
  , runTapir

    -- * State Initialization
  , mkInitialState
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Database.SQLite.Simple (Connection)
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

import Tapir.Types
import Tapir.Config.Types (AppConfig(..), UIConfig(..))
import Tapir.Client.LLM (LLMClient)
import Tapir.Client.Anki (mkAnkiClientWithConfig, checkConnection)
import Tapir.Core.Constants (eventChannelBufferSize)
import Tapir.UI.Types
import Tapir.UI.Attrs (getAttrMap)
import Tapir.UI.Draw (drawUI)
import Tapir.UI.Event (handleEvent)
import Tapir.UI.Input (mkInputEditor)

-- ════════════════════════════════════════════════════════════════
-- APPLICATION DEFINITION
-- ════════════════════════════════════════════════════════════════

-- | The main brick application
tapirApp :: App AppState TapirEvent Name
tapirApp = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = \st -> getAttrMap (uiTheme (configUI (_asConfig st)))
  }

-- | Run the TAPIR application
runTapir :: AppConfig -> LanguageModule -> LLMClient -> Connection -> Session -> IO ()
runTapir config langMod client conn session = do
  chan <- newBChan eventChannelBufferSize
  initialState <- mkInitialState config langMod client conn chan session

  let buildVty = mkVty defaultConfig

  result <- try $ do
    initialVty <- buildVty
    customMain initialVty buildVty (Just chan) tapirApp initialState

  case result of
    Left (err :: SomeException) -> do
      putStrLn ""
      putStrLn "ERROR: Terminal UI failed."
      putStrLn $ "  " ++ show err
      putStrLn ""
      putStrLn "This usually happens when running in Git Bash or mintty."
      putStrLn "Please try one of these alternatives:"
      putStrLn "  1. Run in Windows Terminal, PowerShell, or Command Prompt"
      putStrLn "  2. Use: winpty cabal run tapir"
      putStrLn ""
    Right _ -> pure ()

-- ════════════════════════════════════════════════════════════════
-- STATE INITIALIZATION
-- ════════════════════════════════════════════════════════════════

-- | Create the initial application state
mkInitialState
  :: AppConfig
  -> LanguageModule
  -> LLMClient
  -> Connection
  -> BChan TapirEvent
  -> Session
  -> IO AppState
mkInitialState config langMod client conn chan session = do
  let ankiCfg = configAnki config
  ankiClient <- mkAnkiClientWithConfig ankiCfg

  _ <- forkIO $ do
    result <- checkConnection ankiClient
    let connected = case result of
          Right True -> True
          _          -> False
    writeBChan chan (EvAnkiStatusUpdate connected)

  pure AppState
    { _asSession        = session
    , _asMessages       = []
    , _asInputEditor    = mkInputEditor
    , _asCurrentMode    = sessionMode session
    , _asFocus          = FocusInput
    , _asModal          = NoModal
    , _asRequestState   = Idle
    , _asStreamingText  = mempty
    , _asPendingCard    = Nothing
    , _asPendingStructured = Nothing
    , _asAnkiConnected  = False
    , _asLastError      = Nothing
    , _asConfig         = config
    , _asLangModule     = langMod
    , _asLlmClient      = client
    , _asAnkiClient     = ankiClient
    , _asDbConnection   = conn
    , _asEventChannel   = chan
    }
