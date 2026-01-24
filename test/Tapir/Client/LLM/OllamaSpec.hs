{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Client.LLM.OllamaSpec
-- Description : Tests for Ollama API client
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.OllamaSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Tapir.Types.Provider
import Tapir.Client.LLM.Ollama

-- ════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Create a test provider config for Ollama
testOllamaConfig :: Maybe Text -> ProviderConfig
testOllamaConfig mBaseUrl = ProviderConfig
  { providerType = Ollama
  , providerApiKeyEnv = "OLLAMA_API_KEY"  -- Not used but required
  , providerApiKey = Nothing  -- Ollama doesn't need API key
  , providerBaseUrl = mBaseUrl
  , providerModel = "llama3.2"
  , providerTemperature = 0.7
  , providerMaxTokens = 2000
  , providerTopP = 1.0
  , providerStream = True
  , providerRateLimit = RateLimit 60 60
  , providerTimeoutSeconds = 60  -- Ollama can be slower
  , providerConnectTimeoutSeconds = 10
  }

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Ollama Client" $ do
    describe "mkClient" $ do
      it "creates a client with default URL" $ do
        let cfg = testOllamaConfig Nothing
        client <- mkClient cfg
        olcConfig client `shouldBe` cfg
        olcBaseUrl client `shouldBe` "http://localhost:11434/v1/chat/completions"

      it "creates a client with custom base URL" $ do
        let cfg = testOllamaConfig (Just "http://192.168.1.100:11434")
        client <- mkClient cfg
        olcBaseUrl client `shouldBe` "http://192.168.1.100:11434/v1/chat/completions"

    describe "checkConfigured" $ do
      it "always returns True (no API key required)" $ do
        let cfg = testOllamaConfig Nothing
        result <- checkConfigured cfg
        result `shouldBe` True

      it "returns True even without any configuration" $ do
        let cfg = testOllamaConfig Nothing
        result <- checkConfigured cfg
        result `shouldBe` True

    describe "getBaseUrl" $ do
      it "returns default URL when base_url not set" $ do
        let cfg = testOllamaConfig Nothing
        getBaseUrl cfg `shouldBe` "http://localhost:11434/v1/chat/completions"

      it "appends endpoint to custom base URL" $ do
        let cfg = testOllamaConfig (Just "http://myserver:8080")
        getBaseUrl cfg `shouldBe` "http://myserver:8080/v1/chat/completions"

      it "handles base URL without trailing slash" $ do
        let cfg = testOllamaConfig (Just "http://localhost:11434")
        getBaseUrl cfg `shouldBe` "http://localhost:11434/v1/chat/completions"

    describe "StreamResult" $ do
      it "correctly represents streaming result" $ do
        let result = StreamResult
              { srFullResponse = "Ollama response"
              , srModel = "llama3.2"
              , srTokensUsed = Nothing
              }
        srFullResponse result `shouldBe` "Ollama response"
        srModel result `shouldBe` "llama3.2"
        srTokensUsed result `shouldBe` Nothing

  describe "Ollama Configuration" $ do
    it "uses correct default model" $ do
      let cfg = testOllamaConfig Nothing
      providerModel cfg `shouldBe` "llama3.2"

    it "supports longer timeout for local inference" $ do
      let cfg = testOllamaConfig Nothing
      providerTimeoutSeconds cfg `shouldBe` 60

    it "does not require API key" $ do
      let cfg = testOllamaConfig Nothing
      providerApiKey cfg `shouldBe` Nothing

  describe "Ollama Local Server" $ do
    it "uses HTTP (not HTTPS) for local server" $ do
      let cfg = testOllamaConfig Nothing
      client <- mkClient cfg
      -- Default URL should use http for local
      olcBaseUrl client `shouldSatisfy` T.isPrefixOf "http://" . T.pack

    it "supports custom remote Ollama servers" $ do
      let cfg = testOllamaConfig (Just "https://ollama.example.com")
      client <- mkClient cfg
      olcBaseUrl client `shouldBe` "https://ollama.example.com/v1/chat/completions"
