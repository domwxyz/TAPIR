{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Model.Error
-- Description : Application-level error type
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Model.Error
  ( TapirError(..)
  , displayError
  , shortError
  ) where

import Data.Text (Text)

import Tapir.Client.LLM.Error (LLMError, displayLLMError, shortLLMError)
import Tapir.Db.Error (DbError, displayDbError, shortDbError)
import Tapir.Client.Anki.Error (AnkiError, displayAnkiError, shortAnkiError)
import Tapir.Config.Error (ConfigError, displayConfigError, shortConfigError)

-- | Top-level application error type
-- Wraps domain-specific errors for unified handling at application boundaries
data TapirError
  = AppLLMError !LLMError
  | AppDbError !DbError
  | AppAnkiError !AnkiError
  | AppConfigError !ConfigError
  | AppInternalError !Text
  deriving (Eq, Show)

-- | User-friendly error messages
displayError :: TapirError -> Text
displayError = \case
  AppLLMError e      -> displayLLMError e
  AppDbError e       -> displayDbError e
  AppAnkiError e     -> displayAnkiError e
  AppConfigError e   -> displayConfigError e
  AppInternalError msg -> "Internal error: " <> msg <> ". This is a bug."

-- | Short error messages for status bar
shortError :: TapirError -> Text
shortError = \case
  AppLLMError e      -> shortLLMError e
  AppDbError e       -> shortDbError e
  AppAnkiError e     -> shortAnkiError e
  AppConfigError e   -> shortConfigError e
  AppInternalError _ -> "Internal error"
