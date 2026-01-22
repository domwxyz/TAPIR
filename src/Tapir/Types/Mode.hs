{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapir.Types.Mode
  ( Mode(..)
  , ModeConfig(..)
  , modeToText
  , textToMode
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Learning mode
data Mode
  = Conversation
  | Correction
  | Translation
  | CardGeneration
  | CustomMode !Text  -- For user-defined modes
  deriving (Eq, Show, Generic)

instance ToJSON Mode where
  toJSON = \case
    Conversation -> "conversation"
    Correction -> "correction"
    Translation -> "translation"
    CardGeneration -> "card_generation"
    CustomMode name -> toJSON name

instance FromJSON Mode where
  parseJSON = withText "Mode" $ \case
    "conversation" -> pure Conversation
    "correction" -> pure Correction
    "translation" -> pure Translation
    "card_generation" -> pure CardGeneration
    other -> pure (CustomMode other)

-- | Mode configuration from language module
data ModeConfig = ModeConfig
  { modeLabel :: !Text
  , modeDescription :: !Text
  , modeSystemPrompt :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON ModeConfig where
  parseJSON = withObject "ModeConfig" $ \v -> ModeConfig
    <$> v .: "label"
    <*> v .: "description"
    <*> v .: "system_prompt"

instance ToJSON ModeConfig

-- | Convert mode to text identifier
modeToText :: Mode -> Text
modeToText = \case
  Conversation -> "conversation"
  Correction -> "correction"
  Translation -> "translation"
  CardGeneration -> "card_generation"
  CustomMode name -> name

-- | Parse mode from text
textToMode :: Text -> Mode
textToMode = \case
  "conversation" -> Conversation
  "correction" -> Correction
  "translation" -> Translation
  "card_generation" -> CardGeneration
  other -> CustomMode other
