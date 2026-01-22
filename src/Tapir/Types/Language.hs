{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapir.Types.Language
  ( LearnerLevel(..)
  , learnerLevelDescription
  , LanguageInfo(..)
  , AnkiConfig(..)
  , LanguageModule(..)
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Tapir.Types.Mode (Mode, ModeConfig)

-- | CEFR learner level
data LearnerLevel
  = A1 | A2 | B1 | B2 | C1 | C2
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON LearnerLevel where
  toJSON = toJSON . show

instance FromJSON LearnerLevel where
  parseJSON = withText "LearnerLevel" $ \case
    "A1" -> pure A1
    "A2" -> pure A2
    "B1" -> pure B1
    "B2" -> pure B2
    "C1" -> pure C1
    "C2" -> pure C2
    other -> fail $ "Unknown level: " ++ show other

-- | Human-readable description of learner level
learnerLevelDescription :: LearnerLevel -> Text
learnerLevelDescription = \case
  A1 -> "beginner"
  A2 -> "elementary"
  B1 -> "intermediate"
  B2 -> "upper-intermediate"
  C1 -> "advanced"
  C2 -> "proficient"

-- | Language metadata
data LanguageInfo = LanguageInfo
  { languageId :: !Text
  , languageName :: !Text
  , languageNativeName :: !Text
  , languageCode :: !Text
  , languageNativeLanguage :: !Text
  , languageNativeLanguageCode :: !Text
  , languageNativeLanguageName :: !Text
  , languageVariant :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON LanguageInfo where
  parseJSON = withObject "LanguageInfo" $ \v -> LanguageInfo
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "native_name"
    <*> v .: "code"
    <*> v .: "native_language"
    <*> v .: "native_language_code"
    <*> v .:? "native_language_name" .!= "English"
    <*> v .:? "variant"

instance ToJSON LanguageInfo

-- | Anki configuration for language
data AnkiConfig = AnkiConfig
  { ankiDefaultDeck :: !Text
  , ankiDefaultTags :: ![Text]
  , ankiNoteType :: !Text
  , ankiFields :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

instance FromJSON AnkiConfig where
  parseJSON = withObject "AnkiConfig" $ \v -> AnkiConfig
    <$> v .:? "default_deck" .!= "Vocab"
    <*> v .:? "default_tags" .!= []
    <*> v .:? "note_type" .!= "Basic"
    <*> v .:? "fields" .!= Map.fromList [("front", "Front"), ("back", "Back")]

instance ToJSON AnkiConfig

-- | Complete language module
data LanguageModule = LanguageModule
  { languageInfo :: !LanguageInfo
  , learnerLevel :: !LearnerLevel
  , ankiConfig :: !AnkiConfig
  , modes :: !(Map Text ModeConfig)
  , customModes :: !(Map Text ModeConfig)
  } deriving (Eq, Show, Generic)

instance FromJSON LanguageModule where
  parseJSON = withObject "LanguageModule" $ \v -> LanguageModule
    <$> v .: "language"
    <*> v .:? "learner_level" .!= A1
    <*> v .:? "anki" .!= AnkiConfig "Vocab" [] "Basic" (Map.fromList [("front", "Front"), ("back", "Back")])
    <*> v .:? "modes" .!= Map.empty
    <*> v .:? "custom_modes" .!= Map.empty

instance ToJSON LanguageModule
