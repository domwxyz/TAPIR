{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Config.Error
-- Description : Configuration error types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Config.Error
  ( ConfigError(..)
  , displayConfigError
  , shortConfigError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Errors specific to configuration loading
data ConfigError
  = ConfigNotFound !FilePath
  | ConfigParseError !FilePath !Text
  | LanguageNotFound !Text
  | InvalidLanguageConfig !Text !Text  -- ^ Language and reason
  | PromptTemplateError !Text
  | MissingPromptVariable !Text
  deriving (Eq, Show)

-- | User-friendly error messages
displayConfigError :: ConfigError -> Text
displayConfigError = \case
  ConfigNotFound path ->
    "Configuration file not found: " <> T.pack path
  ConfigParseError path reason ->
    "Failed to parse config at " <> T.pack path <> ": " <> reason
  LanguageNotFound lang ->
    "Language '" <> lang <> "' not found. Run 'tapir --list-languages' to see available."
  InvalidLanguageConfig lang reason ->
    "Invalid config for language '" <> lang <> "': " <> reason
  PromptTemplateError msg ->
    "Prompt template error: " <> msg
  MissingPromptVariable var ->
    "Missing variable in prompt template: " <> var

-- | Short error messages for status bar
shortConfigError :: ConfigError -> Text
shortConfigError = \case
  ConfigNotFound _          -> "Config not found"
  ConfigParseError _ _      -> "Config parse error"
  LanguageNotFound _        -> "Language not found"
  InvalidLanguageConfig _ _ -> "Invalid language config"
  PromptTemplateError _     -> "Prompt error"
  MissingPromptVariable _   -> "Missing variable"
