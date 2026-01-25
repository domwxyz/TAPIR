{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Core.Logging
-- Description : Logging infrastructure (stub)
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- __Status: STUB - NOT YET IMPLEMENTED__
--
-- This module provides a placeholder logging interface.
-- Currently all functions are no-ops or use 'putStrLn'.
--
-- TODO: Implement with co-log, katip, or similar library

module Tapir.Core.Logging
  ( -- * Logging Interface
    LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError

    -- * Status
  , loggingImplemented
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- | Whether logging is actually implemented
-- Check this before relying on logging behavior
loggingImplemented :: Bool
loggingImplemented = False

-- | Log levels
data LogLevel = Debug | Info | Warn | Error
  deriving (Eq, Ord, Show)

-- | Log at debug level (currently no-op)
logDebug :: Text -> IO ()
logDebug _ = pure ()

-- | Log at info level (currently prints to stdout)
logInfo :: Text -> IO ()
logInfo msg = TIO.putStrLn $ "[INFO] " <> msg

-- | Log at warn level (currently prints to stdout)
logWarn :: Text -> IO ()
logWarn msg = TIO.putStrLn $ "[WARN] " <> msg

-- | Log at error level (currently prints to stderr)
logError :: Text -> IO ()
logError msg = TIO.hPutStrLn stderr $ "[ERROR] " <> msg
