{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Client.LLM.ToolsSpec
-- Description : Tests for tool/function schema definitions
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.ToolsSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import Tapir.Client.LLM.Tools
import Tapir.Types.Mode (Mode(..))

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Tool Selection (toolForMode)" $ do
    it "returns Just conversationTool for Conversation mode" $ do
      toolForMode Conversation `shouldBe` Just conversationTool

    it "returns Just correctionTool for Correction mode" $ do
      toolForMode Correction `shouldBe` Just correctionTool

    it "returns Just translationTool for Translation mode" $ do
      toolForMode Translation `shouldBe` Just translationTool

    it "returns Just cardGenerationTool for CardGeneration mode" $ do
      toolForMode CardGeneration `shouldBe` Just cardGenerationTool

    it "returns Nothing for CustomMode" $ do
      toolForMode (CustomMode "test") `shouldBe` Nothing

  describe "Tool Names (toolNameForMode)" $ do
    it "returns Just \"send_conversation_reply\" for Conversation mode" $ do
      toolNameForMode Conversation `shouldBe` Just "send_conversation_reply"

    it "returns Just \"submit_correction\" for Correction mode" $ do
      toolNameForMode Correction `shouldBe` Just "submit_correction"

    it "returns Just \"submit_translation\" for Translation mode" $ do
      toolNameForMode Translation `shouldBe` Just "submit_translation"

    it "returns Just \"create_flashcard\" for CardGeneration mode" $ do
      toolNameForMode CardGeneration `shouldBe` Just "create_flashcard"

    it "returns Nothing for CustomMode" $ do
      toolNameForMode (CustomMode "test") `shouldBe` Nothing

  describe "Tool Serialization" $ do
    it "serializes conversationTool to valid JSON" $ do
      let json = encode conversationTool
      json `shouldInclude` "\"type\":\"function\""
      json `shouldInclude` "\"name\":\"send_conversation_reply\""

    it "serializes correctionTool to valid JSON" $ do
      let json = encode correctionTool
      json `shouldInclude` "\"type\":\"function\""
      json `shouldInclude` "\"name\":\"submit_correction\""

    it "serializes translationTool to valid JSON" $ do
      let json = encode translationTool
      json `shouldInclude` "\"type\":\"function\""
      json `shouldInclude` "\"name\":\"submit_translation\""

    it "serializes cardGenerationTool to valid JSON" $ do
      let json = encode cardGenerationTool
      json `shouldInclude` "\"type\":\"function\""
      json `shouldInclude` "\"name\":\"create_flashcard\""

    it "conversationTool can be decoded back successfully" $ do
      let json = encode conversationTool
      decode json `shouldBe` Just conversationTool

    it "correctionTool can be decoded back successfully" $ do
      let json = encode correctionTool
      decode json `shouldBe` Just correctionTool

    it "translationTool can be decoded back successfully" $ do
      let json = encode translationTool
      decode json `shouldBe` Just translationTool

    it "cardGenerationTool can be decoded back successfully" $ do
      let json = encode cardGenerationTool
      decode json `shouldBe` Just cardGenerationTool

  describe "Tool Structure Validation" $ do
    it "Tool type field is \"function\"" $ do
      toolType conversationTool `shouldBe` "function"
      toolType correctionTool `shouldBe` "function"
      toolType translationTool `shouldBe` "function"
      toolType cardGenerationTool `shouldBe` "function"

    it "Tool function has name (non-empty)" $ do
      tfName (toolFunction conversationTool) `shouldSatisfy` (not . T.null)
      tfName (toolFunction correctionTool) `shouldSatisfy` (not . T.null)
      tfName (toolFunction translationTool) `shouldSatisfy` (not . T.null)
      tfName (toolFunction cardGenerationTool) `shouldSatisfy` (not . T.null)

    it "Tool function has description (non-empty)" $ do
      tfDescription (toolFunction conversationTool) `shouldSatisfy` (not . T.null)
      tfDescription (toolFunction correctionTool) `shouldSatisfy` (not . T.null)
      tfDescription (toolFunction translationTool) `shouldSatisfy` (not . T.null)
      tfDescription (toolFunction cardGenerationTool) `shouldSatisfy` (not . T.null)

    it "Tool function has parameters" $ do
      let convParams = tfParameters (toolFunction conversationTool)
      let corrParams = tfParameters (toolFunction correctionTool)
      let trParams = tfParameters (toolFunction translationTool)
      let cardParams = tfParameters (toolFunction cardGenerationTool)
      encode convParams `shouldInclude` "\"type\":\"object\""
      encode corrParams `shouldInclude` "\"type\":\"object\""
      encode trParams `shouldInclude` "\"type\":\"object\""
      encode cardParams `shouldInclude` "\"type\":\"object\""

    it "Tool function has strict flag set to true" $ do
      tfStrict (toolFunction conversationTool) `shouldBe` True
      tfStrict (toolFunction correctionTool) `shouldBe` True
      tfStrict (toolFunction translationTool) `shouldBe` True
      tfStrict (toolFunction cardGenerationTool) `shouldBe` True

    it "conversationTool JSON contains required field names" $ do
      let json = encode conversationTool
      json `shouldInclude` "\"reply\""
      json `shouldInclude` "\"vocab_highlights\""

    it "correctionTool JSON contains required field names" $ do
      let json = encode correctionTool
      json `shouldInclude` "\"original\""
      json `shouldInclude` "\"corrected\""
      json `shouldInclude` "\"is_correct\""
      json `shouldInclude` "\"corrections\""

    it "translationTool JSON contains required field names" $ do
      let json = encode translationTool
      json `shouldInclude` "\"source_text\""
      json `shouldInclude` "\"source_lang\""
      json `shouldInclude` "\"target_text\""
      json `shouldInclude` "\"target_lang\""

    it "cardGenerationTool JSON contains required field names" $ do
      let json = encode cardGenerationTool
      json `shouldInclude` "\"front\""
      json `shouldInclude` "\"back\""
      json `shouldInclude` "\"tags\""

  describe "Tool Collection (allTools)" $ do
    it "has 4 tools" $ do
      length allTools `shouldBe` 4

    it "includes conversationTool" $ do
      conversationTool `shouldSatisfy` (`elem` allTools)

    it "includes correctionTool" $ do
      correctionTool `shouldSatisfy` (`elem` allTools)

    it "includes translationTool" $ do
      translationTool `shouldSatisfy` (`elem` allTools)

    it "includes cardGenerationTool" $ do
      cardGenerationTool `shouldSatisfy` (`elem` allTools)

  describe "ToolChoice Serialization" $ do
    it "ToolChoiceAuto serializes to \"auto\"" $ do
      encode ToolChoiceAuto `shouldBe` "\"auto\""

    it "ToolChoiceNone serializes to \"none\"" $ do
      encode ToolChoiceNone `shouldBe` "\"none\""

    it "ToolChoiceRequired serializes to \"required\"" $ do
      encode ToolChoiceRequired `shouldBe` "\"required\""

    it "ToolChoiceForced serializes to object with type and function.name" $ do
      let json = encode (ToolChoiceForced "test_func")
      json `shouldInclude` "\"type\":\"function\""
      json `shouldInclude` "\"name\":\"test_func\""

-- Helper: check if lazy bytestring contains text (using char8 search)
shouldInclude :: BL.ByteString -> BL.ByteString -> Expectation
shouldInclude haystack needle =
  let haystackStr = BL8.unpack haystack
      needleStr = BL8.unpack needle
  in if needleStr `isInfixOfList` haystackStr
     then pure ()
     else expectationFailure $
       "Expected " ++ show haystack ++ " to contain " ++ show needle
  where
    isInfixOfList :: Eq a => [a] -> [a] -> Bool
    isInfixOfList needle' haystack' = any (needle' `isPrefixOfList`) (tails haystack')

    isPrefixOfList :: Eq a => [a] -> [a] -> Bool
    isPrefixOfList [] _ = True
    isPrefixOfList _ [] = False
    isPrefixOfList (x:xs) (y:ys) = x == y && isPrefixOfList xs ys

    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
