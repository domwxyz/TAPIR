{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Description : TAPIR entry point
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Main (main) where

import Control.Exception (bracket)
import qualified Data.Text as T
import Database.SQLite.Simple (open, close)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory)

import Tapir.Types
import Tapir.Config.Types
import Tapir.Config.Loader (loadConfig, loadLanguageModule, expandPath)
import Tapir.Config.Defaults (defaultConfig)
import Tapir.Db.Schema (initializeDatabase)
import Tapir.Db.Error (displayDbError)
import Tapir.Client.LLM (mkLLMClient, LLMClient(..))
import Tapir.Service.Session (createInitialSession)
import Tapir.UI.App (runTapir)

main :: IO ()
main = do
  putStrLn "=========================================================="
  putStrLn "         TAPIR - Language Learning Assistant"
  putStrLn "                    Version 1.0.0"
  putStrLn "=========================================================="
  putStrLn ""

  -- Load configuration
  putStrLn "[...] Loading configuration..."
  configResult <- loadConfig
  config <- case configResult of
    Left err -> do
      putStrLn $ "[WARN] Config not found, using defaults: " <> show err
      pure defaultConfig
    Right cfg -> do
      putStrLn "[OK] Configuration loaded"
      pure cfg

  -- Load language module
  let langName = configActiveLanguage config
  putStrLn $ "[...] Loading language module: " <> T.unpack langName
  langResult <- loadLanguageModule langName
  langMod <- case langResult of
    Left err -> do
      putStrLn $ "[ERROR] Failed to load language module: " <> show err
      exitFailure
    Right lm -> do
      let info = languageInfo lm
      putStrLn $ "[OK] Language loaded: " <> T.unpack (languageName info)
      pure lm

  -- Initialize database
  putStrLn "[...] Initializing database..."
  dbPathExpanded <- expandPath (dbPath (configDatabase config))
  createDirectoryIfMissing True (takeDirectory dbPathExpanded)
  bracket (open dbPathExpanded) close $ \conn -> do
    dbResult <- initializeDatabase conn
    case dbResult of
      Left err -> do
        putStrLn $ "[ERROR] Database initialization failed: " <> T.unpack (displayDbError err)
        exitFailure
      Right () -> putStrLn "[OK] Database initialized"

    -- Create LLM client
    putStrLn "[...] Creating LLM client..."
    client <- mkLLMClient (configProvider config)
    putStrLn "[OK] LLM client created"

    -- Check for unimplemented providers
    case llmProviderName client of
      "Anthropic" -> putStrLn "[WARN] Anthropic provider is not yet implemented"
      _ -> pure ()

    -- Check API key
    isConfigured <- llmIsConfigured client
    if isConfigured
      then putStrLn "[OK] API key configured"
      else putStrLn "[WARN] API key not configured (set OPENROUTER_API_KEY)"

    -- Check Anki (optional)
    putStrLn "[INFO] Anki integration will be checked at runtime"

    -- Create initial session
    putStrLn "[...] Creating session..."
    sessionResult <- createInitialSession conn langMod
    case sessionResult of
      Left err -> do
        putStrLn $ "[ERROR] Failed to create session: " <> T.unpack (displayDbError err)
        exitFailure
      Right session -> do
        putStrLn "[OK] Session created"

        putStrLn ""
        putStrLn "[OK] All systems ready!"
        putStrLn ""
        putStrLn "Starting TUI... (Press Ctrl+Q to quit)"
        putStrLn ""

        -- Run the TUI
        runTapir config langMod client conn session

  putStrLn ""
  putStrLn "Goodbye!"
  exitSuccess
