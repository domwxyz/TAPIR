{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Client.Anki.Error
-- Description : Anki client error types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.Anki.Error
  ( AnkiError(..)
  , displayAnkiError
  , shortAnkiError
  ) where

import Data.Text (Text)

-- | Errors specific to Anki operations
data AnkiError
  = AnkiNotRunning
  | AnkiConnectionError !Text
  | AnkiDeckNotFound !Text
  | AnkiCardParseError !Text
  deriving (Eq, Show)

-- | User-friendly error messages
displayAnkiError :: AnkiError -> Text
displayAnkiError = \case
  AnkiNotRunning ->
    "Anki is not running or AnkiConnect is not installed."
  AnkiConnectionError msg ->
    "Failed to connect to Anki: " <> msg
  AnkiDeckNotFound deck ->
    "Anki deck '" <> deck <> "' not found."
  AnkiCardParseError msg ->
    "Failed to parse card from response: " <> msg

-- | Short error messages for status bar
shortAnkiError :: AnkiError -> Text
shortAnkiError = \case
  AnkiNotRunning        -> "Anki not running"
  AnkiConnectionError _ -> "Anki connection error"
  AnkiDeckNotFound _    -> "Deck not found"
  AnkiCardParseError _  -> "Card parse error"
