{-# LANGUAGE OverloadedStrings #-}

module Tapir.Config.LoaderSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Tapir.Config.Loader
import Tapir.Types

-- | Mock language module for testing
mockLanguageModule :: LanguageModule
mockLanguageModule = LanguageModule
  { languageInfo = LanguageInfo
      { languageId = "spanish"
      , languageName = "Spanish"
      , languageNativeName = "Español"
      , languageCode = "es"
      , languageNativeLanguage = "English"
      , languageNativeLanguageCode = "en"
      , languageNativeLanguageName = "English"
      , languageVariant = Just "latam"
      }
  , learnerLevel = B1
  , ankiConfig = AnkiConfig "Vocab" [] "Basic" Map.empty
  , modes = Map.fromList
      [ ("conversation", ModeConfig
          { modeLabel = "Chat"
          , modeDescription = "Practice conversation"
          , modeSystemPrompt = "You are a {{language}} tutor helping a {{level}} ({{level_description}}) learner."
          })
      ]
  , customModes = Map.empty
  }

spec :: Spec
spec = do
  describe "interpolatePrompt" $ do
    it "replaces language variable" $ do
      let vars = PromptVars "Spanish" "English" "Latin American" "A1" "beginner"
          template = "You are a {{language}} tutor"
      interpolatePrompt vars template `shouldBe` "You are a Spanish tutor"

    it "replaces all variables" $ do
      let vars = PromptVars "French" "English" "Parisian" "B2" "upper-intermediate"
          template = "{{language}} ({{variant}}) for {{level}} learners"
      interpolatePrompt vars template `shouldBe` "French (Parisian) for B2 learners"

  describe "promptVarsFromModule" $ do
    it "extracts variables from language module" $ do
      let vars = promptVarsFromModule mockLanguageModule
      pvLanguage vars `shouldBe` "Spanish"
      pvNativeLanguage vars `shouldBe` "English"
      pvVariant vars `shouldBe` "Latin American"
      pvLevel vars `shouldBe` "B1"
      pvLevelDesc vars `shouldBe` "intermediate"

  describe "getSystemPrompt" $ do
    it "interpolates prompt for mode" $ do
      case getSystemPrompt mockLanguageModule Conversation of
        Just prompt -> prompt `shouldBe` "You are a Spanish tutor helping a B1 (intermediate) learner."
        Nothing -> expectationFailure "Expected Just prompt, got Nothing"

    it "returns Nothing for undefined mode" $ do
      getSystemPrompt mockLanguageModule Translation `shouldBe` Nothing
