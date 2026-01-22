{-# LANGUAGE OverloadedStrings #-}

module Tapir.Config.LoaderSpec (spec) where

import Test.Hspec
import Tapir.Config.Loader
import Tapir.Config.Types
import Tapir.Types

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
    it "extracts variables from language module" $ pending

  describe "getSystemPrompt" $ do
    it "interpolates prompt for mode" $ pending
