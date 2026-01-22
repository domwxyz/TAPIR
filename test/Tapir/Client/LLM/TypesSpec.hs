{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Client.LLM.TypesSpec
-- Description : Tests for LLM API types JSON serialization
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import Tapir.Client.LLM.Types

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "ChatMessage" $ do
    it "serializes to correct JSON format" $ do
      let msg = ChatMessage "user" "Hello, how are you?"
      let json = encode msg
      json `shouldInclude` "\"role\":\"user\""
      json `shouldInclude` "\"content\":\"Hello, how are you?\""

    it "roundtrips through JSON" $ do
      let msg = ChatMessage "assistant" "I'm doing well, thank you!"
      decode (encode msg) `shouldBe` Just msg

  describe "ChatRequest" $ do
    it "serializes with all required fields" $ do
      let req = defaultChatRequest "z-ai/glm-4.7"
            [ChatMessage "user" "Hi"]
      let json = encode req
      json `shouldInclude` "\"model\":\"z-ai/glm-4.7\""
      json `shouldInclude` "\"messages\":"
      json `shouldInclude` "\"stream\":true"

    it "includes optional fields when set" $ do
      let req = (defaultChatRequest "test-model" [])
            { crTemperature = Just 0.5
            , crMaxTokens = Just 1000
            }
      let json = encode req
      json `shouldInclude` "\"temperature\":0.5"
      json `shouldInclude` "\"max_tokens\":1000"

  describe "ChatResponse" $ do
    it "parses a valid response" $ do
      let json = BL.concat
            [ "{\"id\":\"gen-123\","
            , "\"model\":\"test/model\","
            , "\"choices\":[{"
            , "\"index\":0,"
            , "\"message\":{\"role\":\"assistant\",\"content\":\"Hello!\"},"
            , "\"finish_reason\":\"stop\""
            , "}],"
            , "\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5,\"total_tokens\":15}}"
            ]
      case eitherDecode json of
        Right (resp :: ChatResponse) -> do
          respId resp `shouldBe` "gen-123"
          respModel resp `shouldBe` "test/model"
          length (respChoices resp) `shouldBe` 1
          case respUsage resp of
            Just usage -> usageTotalTokens usage `shouldBe` 15
            Nothing -> expectationFailure "Expected usage to be present"
        Left err -> expectationFailure $ "Failed to parse response: " <> err

    it "parses response without usage" $ do
      let json = BL.concat
            [ "{\"id\":\"gen-456\","
            , "\"model\":\"test/model\","
            , "\"choices\":[{"
            , "\"index\":0,"
            , "\"message\":{\"role\":\"assistant\",\"content\":\"Hi!\"},"
            , "\"finish_reason\":null"
            , "}]}"
            ]
      case eitherDecode json of
        Right (resp :: ChatResponse) -> do
          respUsage resp `shouldBe` Nothing
        Left err -> expectationFailure $ "Failed to parse response: " <> err

  describe "StreamChunk" $ do
    it "parses a streaming chunk with content" $ do
      let json = BL.concat
            [ "{\"id\":\"gen-789\","
            , "\"model\":\"test/model\","
            , "\"choices\":[{"
            , "\"index\":0,"
            , "\"delta\":{\"content\":\"Hello\"},"
            , "\"finish_reason\":null"
            , "}]}"
            ]
      case eitherDecode json of
        Right (chunk :: StreamChunk) -> do
          scId chunk `shouldBe` "gen-789"
          case scChoices chunk of
            [choice] -> do
              deltaContent (streamChoiceDelta choice) `shouldBe` Just "Hello"
            _ -> expectationFailure "Expected one choice"
        Left err -> expectationFailure $ "Failed to parse chunk: " <> err

    it "parses initial chunk with role" $ do
      let json = BL.concat
            [ "{\"id\":\"gen-abc\","
            , "\"model\":\"test/model\","
            , "\"choices\":[{"
            , "\"index\":0,"
            , "\"delta\":{\"role\":\"assistant\"},"
            , "\"finish_reason\":null"
            , "}]}"
            ]
      case eitherDecode json of
        Right (chunk :: StreamChunk) -> do
          case scChoices chunk of
            [choice] -> do
              deltaRole (streamChoiceDelta choice) `shouldBe` Just "assistant"
              deltaContent (streamChoiceDelta choice) `shouldBe` Nothing
            _ -> expectationFailure "Expected one choice"
        Left err -> expectationFailure $ "Failed to parse chunk: " <> err

    it "parses final chunk with finish_reason" $ do
      let json = BL.concat
            [ "{\"id\":\"gen-end\","
            , "\"model\":\"test/model\","
            , "\"choices\":[{"
            , "\"index\":0,"
            , "\"delta\":{},"
            , "\"finish_reason\":\"stop\""
            , "}]}"
            ]
      case eitherDecode json of
        Right (chunk :: StreamChunk) -> do
          case scChoices chunk of
            [choice] -> do
              streamChoiceFinishReason choice `shouldBe` Just "stop"
            _ -> expectationFailure "Expected one choice"
        Left err -> expectationFailure $ "Failed to parse chunk: " <> err

  describe "APIErrorResponse" $ do
    it "parses an error response" $ do
      let json = BL.concat
            [ "{\"error\":{"
            , "\"message\":\"Invalid API key\","
            , "\"type\":\"authentication_error\","
            , "\"code\":401"
            , "}}"
            ]
      case eitherDecode json of
        Right (errResp :: APIErrorResponse) -> do
          aedMessage (aerError errResp) `shouldBe` "Invalid API key"
          aedType (aerError errResp) `shouldBe` "authentication_error"
          aedCode (aerError errResp) `shouldBe` Just 401
        Left err -> expectationFailure $ "Failed to parse error: " <> err

    it "parses error without code" $ do
      let json = BL.concat
            [ "{\"error\":{"
            , "\"message\":\"Rate limit exceeded\","
            , "\"type\":\"rate_limit_error\""
            , "}}"
            ]
      case eitherDecode json of
        Right (errResp :: APIErrorResponse) -> do
          aedCode (aerError errResp) `shouldBe` Nothing
        Left err -> expectationFailure $ "Failed to parse error: " <> err

-- Helper: check if lazy bytestring contains text (using char8 search)
shouldInclude :: BL.ByteString -> BL.ByteString -> Expectation
shouldInclude haystack needle =
  let haystackStr = BL8.unpack haystack
      needleStr = BL8.unpack needle
  in if needleStr `isInfixOfList` haystackStr
       then pure ()
       else expectationFailure $
         "Expected " <> show haystack <> " to contain " <> show needle
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
