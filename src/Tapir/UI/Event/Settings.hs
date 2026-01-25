{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Event.Settings
-- Description : Settings modal event handlers
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Settings
  ( handleSettingsSave
  , handleSettingsReload
  , handleViewPrompt
  , cycleLearnerLevel
  ) where

import Brick
import Brick.BChan (writeBChan)
import Control.Monad.IO.Class (liftIO)
import Lens.Micro.Mtl ((.=))

import Tapir.Types (LanguageModule(..), LanguageInfo(..), LearnerLevel(..))
import Tapir.UI.Types
import Tapir.UI.Widgets (safeIndexWithDefault)
import Tapir.Config.Loader (getSystemPrompt, loadLanguageModule, loadConfig)

-- | Handle 's' key in settings modal - save settings
handleSettingsSave :: EventM Name AppState ()
handleSettingsSave = do
  appState <- get
  let langMod = _asLangModule appState
  -- Reload language module with new level
  liftIO $ do
    result <- loadLanguageModule (languageId (languageInfo langMod))
    case result of
      Right newLangMod -> writeBChan (_asEventChannel appState) (EvLanguageReloaded newLangMod)
      Left _ -> pure ()
  asModal .= NoModal

-- | Handle 'r' key in settings modal - reload config
handleSettingsReload :: EventM Name AppState ()
handleSettingsReload = do
  appState <- get
  liftIO $ do
    result <- loadConfig
    case result of
      Right newConfig -> writeBChan (_asEventChannel appState) (EvConfigReloaded newConfig)
      Left _ -> pure ()
  asModal .= NoModal

-- | Handle 'e' key in settings modal - view system prompt
handleViewPrompt :: EventM Name AppState ()
handleViewPrompt = do
  settingsState <- get
  let langMod = _asLangModule settingsState
      mode = _asCurrentMode settingsState
  case getSystemPrompt langMod mode of
    Just prompt -> asModal .= PromptPreviewModal prompt
    Nothing -> pure ()

-- | Cycle through learner levels with +/- keys
cycleLearnerLevel :: Int -> EventM Name AppState ()
cycleLearnerLevel delta = do
  st <- get
  let langMod = _asLangModule st
      levels = [A1, A2, B1, B2, C1, C2]
      currentLevel = learnerLevel langMod
      currentIdx = case currentLevel of
        A1 -> 0; A2 -> 1; B1 -> 2; B2 -> 3; C1 -> 4; C2 -> 5
      newIdx = (currentIdx + delta) `mod` length levels
      newLevel = safeIndexWithDefault A1 levels newIdx
      newLangMod = langMod { learnerLevel = newLevel }
  asLangModule .= newLangMod
