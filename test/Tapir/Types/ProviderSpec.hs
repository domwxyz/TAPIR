{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Types.ProviderSpec
-- Description : Tests for provider types and configuration
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Types.ProviderSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (isJust, isNothing)

import Tapir.Types.Provider

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "ProviderType" $ do
    it "serializes OpenRouter correctly" $ do
      encode OpenRouter `shouldBe` "\"openrouter\""

    it "serializes OpenAI correctly" $ do
      encode OpenAI `shouldBe` "\"openai\""

    it "serializes Ollama correctly" $ do
      encode Ollama `shouldBe` "\"ollama\""

    it "serializes Anthropic correctly" $ do
      encode Anthropic `shouldBe` "\"anthropic\""

    it "parses openrouter" $ do
      decode "\"openrouter\"" `shouldBe` Just OpenRouter

    it "parses openai" $ do
      decode "\"openai\"" `shouldBe` Just OpenAI

    it "parses ollama" $ do
      decode "\"ollama\"" `shouldBe` Just Ollama

    it "parses anthropic" $ do
      decode "\"anthropic\"" `shouldBe` Just Anthropic

    it "rejects invalid provider type" $ do
      (decode "\"invalid\"" :: Maybe ProviderType) `shouldBe` Nothing

  describe "ProviderConfig" $ do
    describe "JSON parsing" $ do
      it "parses minimal OpenRouter config" $ do
        let json = "{\"type\": \"openrouter\"}"
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerType cfg `shouldBe` OpenRouter
            providerModel cfg `shouldBe` "z-ai/glm-4.7"  -- default
            providerBaseUrl cfg `shouldBe` Nothing
          Left err -> expectationFailure $ "Failed to parse: " <> err

      it "parses OpenAI config with api_key" $ do
        let json = BL.concat
              [ "{\"type\": \"openai\","
              , "\"api_key\": \"sk-test-key\","
              , "\"model\": \"gpt-4o\"}"
              ]
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerType cfg `shouldBe` OpenAI
            providerApiKey cfg `shouldBe` Just "sk-test-key"
            providerModel cfg `shouldBe` "gpt-4o"
          Left err -> expectationFailure $ "Failed to parse: " <> err

      it "parses Ollama config with base_url" $ do
        let json = BL.concat
              [ "{\"type\": \"ollama\","
              , "\"base_url\": \"http://192.168.1.100:11434\","
              , "\"model\": \"llama3.2\"}"
              ]
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerType cfg `shouldBe` Ollama
            providerBaseUrl cfg `shouldBe` Just "http://192.168.1.100:11434"
            providerModel cfg `shouldBe` "llama3.2"
          Left err -> expectationFailure $ "Failed to parse: " <> err

      it "parses config with custom api_key_env" $ do
        let json = BL.concat
              [ "{\"type\": \"openai\","
              , "\"api_key_env\": \"MY_CUSTOM_API_KEY\"}"
              ]
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerApiKeyEnv cfg `shouldBe` "MY_CUSTOM_API_KEY"
          Left err -> expectationFailure $ "Failed to parse: " <> err

      it "uses correct defaults" $ do
        let json = "{\"type\": \"openrouter\"}"
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerApiKeyEnv cfg `shouldBe` "OPENROUTER_API_KEY"
            providerApiKey cfg `shouldBe` Nothing
            providerBaseUrl cfg `shouldBe` Nothing
            providerTemperature cfg `shouldBe` 0.7
            providerMaxTokens cfg `shouldBe` 2000
            providerTopP cfg `shouldBe` 1.0
            providerStream cfg `shouldBe` True
            providerTimeoutSeconds cfg `shouldBe` 30
            providerConnectTimeoutSeconds cfg `shouldBe` 10
          Left err -> expectationFailure $ "Failed to parse: " <> err

      it "parses full config with all fields" $ do
        let json = BL.concat
              [ "{\"type\": \"openai\","
              , "\"api_key_env\": \"OPENAI_API_KEY\","
              , "\"api_key\": \"sk-key\","
              , "\"base_url\": \"https://custom.openai.com\","
              , "\"model\": \"gpt-4o-mini\","
              , "\"temperature\": 0.5,"
              , "\"max_tokens\": 1000,"
              , "\"top_p\": 0.9,"
              , "\"stream\": false,"
              , "\"rate_limit\": {\"requests_per_minute\": 100, \"retry_after_seconds\": 30},"
              , "\"timeout_seconds\": 60,"
              , "\"connect_timeout_seconds\": 15}"
              ]
        case eitherDecode json of
          Right (cfg :: ProviderConfig) -> do
            providerType cfg `shouldBe` OpenAI
            providerApiKeyEnv cfg `shouldBe` "OPENAI_API_KEY"
            providerApiKey cfg `shouldBe` Just "sk-key"
            providerBaseUrl cfg `shouldBe` Just "https://custom.openai.com"
            providerModel cfg `shouldBe` "gpt-4o-mini"
            providerTemperature cfg `shouldBe` 0.5
            providerMaxTokens cfg `shouldBe` 1000
            providerTopP cfg `shouldBe` 0.9
            providerStream cfg `shouldBe` False
            rateLimitRequestsPerMinute (providerRateLimit cfg) `shouldBe` 100
            rateLimitRetryAfterSeconds (providerRateLimit cfg) `shouldBe` 30
            providerTimeoutSeconds cfg `shouldBe` 60
            providerConnectTimeoutSeconds cfg `shouldBe` 15
          Left err -> expectationFailure $ "Failed to parse: " <> err

  describe "RateLimit" $ do
    it "parses rate limit config" $ do
      let json = "{\"requests_per_minute\": 50, \"retry_after_seconds\": 120}"
      case eitherDecode json of
        Right (rl :: RateLimit) -> do
          rateLimitRequestsPerMinute rl `shouldBe` 50
          rateLimitRetryAfterSeconds rl `shouldBe` 120
        Left err -> expectationFailure $ "Failed to parse: " <> err

    it "serializes rate limit correctly" $ do
      let rl = RateLimit 60 30
      let json = encode rl
      json `shouldIncludeBS` "\"requests_per_minute\":60"
      json `shouldIncludeBS` "\"retry_after_seconds\":30"

-- Helper: check if lazy bytestring contains text
shouldIncludeBS :: BL.ByteString -> BL.ByteString -> Expectation
shouldIncludeBS haystack needle =
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
