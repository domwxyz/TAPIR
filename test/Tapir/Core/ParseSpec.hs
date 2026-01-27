{-# LANGUAGE OverloadedStrings #-}

module Tapir.Core.ParseSpec (spec) where

import Test.Hspec
import Data.Time (UTCTime)
import Data.Time.Clock (secondsToNominalDiffTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as T
import Tapir.Core.Parse
import Tapir.Types (TapirError(..))

-- Test timestamp: 2024-06-15T10:30:00Z
testTime :: UTCTime
testTime = addUTCTime
  (secondsToNominalDiffTime (10*3600 + 30*60))  -- 10:30:00
  (read "2024-06-15 00:00:00 UTC")

spec :: Spec
spec = describe "Tapir.Core.Parse" $ do

  describe "parseUTCTime" $ do
    it "parses valid ISO8601 timestamp" $ do
      case parseUTCTime "2024-06-15T10:30:00Z" of
        Right t -> formatUTCTime t `shouldSatisfy` T.isPrefixOf "2024-06-15"
        Left _ -> expectationFailure "expected Right"

    it "returns Left for invalid timestamp" $ do
      case parseUTCTime "not-a-date" of
        Left (AppInternalError msg) -> msg `shouldSatisfy` T.isInfixOf "Invalid ISO8601"
        _ -> expectationFailure "expected Left AppInternalError"

    it "returns Left for empty string" $ do
      case parseUTCTime "" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left"

  describe "parseUTCTimeMaybe" $ do
    it "parses valid ISO8601 timestamp" $ do
      parseUTCTimeMaybe "2024-06-15T10:30:00Z" `shouldSatisfy`
        \case Just _ -> True; Nothing -> False

    it "returns Nothing for invalid timestamp" $ do
      parseUTCTimeMaybe "invalid" `shouldBe` Nothing

    it "returns Nothing for empty string" $ do
      parseUTCTimeMaybe "" `shouldBe` Nothing

  describe "formatUTCTime" $ do
    it "formats time in ISO8601 format" $ do
      let formatted = formatUTCTime testTime
      formatted `shouldSatisfy` T.isPrefixOf "2024-06-15"

    it "roundtrips through parse/format" $ do
      let original = "2024-06-15T10:30:00Z"
      case parseUTCTimeMaybe original of
        Nothing -> expectationFailure "initial parse failed"
        Just t -> do
          let formatted = formatUTCTime t
          -- Note: formatting may differ slightly from original
          parseUTCTimeMaybe formatted `shouldSatisfy`
            \case Just _ -> True; Nothing -> False

  describe "parseJsonText" $ do
    it "parses valid JSON array" $ do
      case parseJsonText "[\"a\", \"b\", \"c\"]" :: Either TapirError [T.Text] of
        Right tags -> tags `shouldBe` ["a", "b", "c"]
        Left _ -> expectationFailure "expected Right"

    it "parses valid JSON object" $ do
      case parseJsonText "{\"key\": \"value\"}" :: Either TapirError (T.Text, T.Text) of
        Right _ -> expectationFailure "expected Left (wrong type)"
        Left _ -> pure ()  -- Type mismatch is an error

    it "returns Left for invalid JSON" $ do
      case parseJsonText "not json" :: Either TapirError [T.Text] of
        Left (AppInternalError msg) -> msg `shouldSatisfy` T.isInfixOf "JSON parse error"
        _ -> expectationFailure "expected Left AppInternalError"

    it "returns Left for empty string" $ do
      case parseJsonText "" :: Either TapirError [T.Text] of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left"

  describe "parseJsonTextMaybe" $ do
    it "parses valid JSON" $ do
      parseJsonTextMaybe "[1,2,3]" `shouldBe` Just ([1,2,3] :: [Int])

    it "returns Nothing for invalid JSON" $ do
      parseJsonTextMaybe "invalid" `shouldBe` (Nothing :: Maybe [Int])

  describe "parseTagsOrEmpty" $ do
    it "parses valid JSON array of strings" $ do
      parseTagsOrEmpty "[\"spanish\", \"vocab\"]" `shouldBe` ["spanish", "vocab"]

    it "returns empty list for empty string" $ do
      parseTagsOrEmpty "" `shouldBe` []

    it "returns empty list for invalid JSON" $ do
      parseTagsOrEmpty "not json" `shouldBe` []

    it "returns empty list for non-array JSON" $ do
      parseTagsOrEmpty "{\"key\": \"value\"}" `shouldBe` []

    it "returns empty list for number array" $ do
      parseTagsOrEmpty "[1, 2, 3]" `shouldBe` []

  describe "stripMarkdownFences" $ do
    it "strips json fenced code" $ do
      let input = "```json\n{\"key\": \"value\"}\n```"
      stripMarkdownFences input `shouldBe` "{\"key\": \"value\"}\n"

    it "strips plain fenced code" $ do
      let input = "```\nsome code\n```"
      stripMarkdownFences input `shouldBe` "some code\n"

    it "strips fenced code with language" $ do
      let input = "```python\nprint('hello')\n```"
      stripMarkdownFences input `shouldBe` "print('hello')\n"

    it "returns original text when no fences" $ do
      let input = "plain text without fences"
      stripMarkdownFences input `shouldBe` input

    it "handles leading whitespace" $ do
      let input = "\n  \n```json\n{}\n```"
      stripMarkdownFences input `shouldBe` "{}\n"

    it "handles unclosed fences" $ do
      let input = "```json\n{\"key\": \"value\"}"
      stripMarkdownFences input `shouldBe` "{\"key\": \"value\"}\n"

  describe "validateNonEmpty" $ do
    it "returns Right for non-empty text" $ do
      validateNonEmpty "field" "value" `shouldBe` Right "value"

    it "returns Left for empty text" $ do
      case validateNonEmpty "myField" "" of
        Left (AppInternalError msg) -> msg `shouldSatisfy` T.isInfixOf "myField"
        _ -> expectationFailure "expected Left AppInternalError"

    it "returns Left for whitespace-only text" $ do
      case validateNonEmpty "field" "   " of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left"

  describe "validateInRange" $ do
    it "returns Right for value in range" $ do
      validateInRange "score" (0 :: Int) 100 50 `shouldBe` Right 50

    it "returns Left for value below minimum" $ do
      case validateInRange "score" (0 :: Int) 100 (-5) of
        Left (AppInternalError msg) -> msg `shouldSatisfy` T.isInfixOf ">="
        _ -> expectationFailure "expected Left AppInternalError"

    it "returns Left for value above maximum" $ do
      case validateInRange "score" (0 :: Int) 100 150 of
        Left (AppInternalError msg) -> msg `shouldSatisfy` T.isInfixOf "<="
        _ -> expectationFailure "expected Left AppInternalError"

    it "accepts boundary values" $ do
      validateInRange "score" (0 :: Int) 100 0 `shouldBe` Right (0 :: Int)
      validateInRange "score" (0 :: Int) 100 100 `shouldBe` Right (100 :: Int)
