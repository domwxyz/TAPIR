{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Client.LLM.RequestSpec
-- Description : Tests for request building functions
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.RequestSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Tapir.Client.LLM.Request
import Tapir.Client.LLM.Types
import Tapir.Types (Mode(..), Message(..), Role(..), ModeConfig(..))
import Tapir.Types.Language (LanguageModule(..), LanguageInfo(..), AnkiConfig(..))
import Tapir.Types.Provider (ProviderConfig(..), ProviderType(..), RateLimit(..))
import Tapir.Types.Language (LearnerLevel(..))
import Tapir.Config.Types (AppConfig(..), UIConfig(..), ChatUIConfig(..), DatabaseConfig(..), LoggingConfig(..), AnkiClientConfig(..))
import qualified Data.Map.Strict as Map

-- ════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════

mockModeConfig :: Text -> ModeConfig
mockModeConfig modeName = ModeConfig
  { modeLabel = modeName
  , modeDescription = "Test mode for " <> modeName
  , modeSystemPrompt = "You are a test assistant for " <> modeName
  }

mockLanguageModule :: LanguageModule
mockLanguageModule = LanguageModule
  { languageInfo = LanguageInfo
    { languageId = "spanish"
    , languageName = "Spanish"
    , languageNativeName = "Español"
    , languageCode = "es"
    , languageNativeLanguage = "english"
    , languageNativeLanguageCode = "en"
    , languageNativeLanguageName = "English"
    , languageVariant = Just "latam"
    }
  , learnerLevel = A1
  , ankiConfig = AnkiConfig "Vocab" [] "Basic" Map.empty
  , modes = Map.fromList
      [ ("conversation", mockModeConfig "conversation")
      , ("correction", mockModeConfig "correction")
      , ("translation", mockModeConfig "translation")
      , ("card_generation", mockModeConfig "card_generation")
      ]
  , customModes = Map.empty
  }

mockProviderConfig :: ProviderConfig
mockProviderConfig = ProviderConfig
  { providerType = OpenRouter
  , providerApiKeyEnv = "OPENROUTER_API_KEY"
  , providerApiKey = Just "test-key"
  , providerBaseUrl = Nothing
  , providerModel = "test-model"
  , providerTemperature = 0.7
  , providerMaxTokens = 2000
  , providerTopP = 1.0
  , providerStream = True
  , providerRateLimit = RateLimit 20 60
  , providerTimeoutSeconds = 30
  , providerConnectTimeoutSeconds = 10
  }

mockUIConfig :: UIConfig
mockUIConfig = UIConfig
  { uiTheme = "default"
  , uiChat = ChatUIConfig True "%H:%M" 1000 True
  }

mockDatabaseConfig :: DatabaseConfig
mockDatabaseConfig = DatabaseConfig
  { dbPath = "/tmp/test.db"
  , dbAutoBackup = False
  , dbBackupDir = "/tmp/backups"
  , dbMaxBackups = 5
  }

mockLoggingConfig :: LoggingConfig
mockLoggingConfig = LoggingConfig
  { logEnabled = False
  , logLevel = "error"
  , logFile = "/tmp/test.log"
  , logMaxSizeMb = 1
  , logRotate = False
  }

mockAnkiClientConfig :: AnkiClientConfig
mockAnkiClientConfig = AnkiClientConfig
  { ankiHost = "localhost"
  , ankiPort = 8765
  , ankiTimeoutSeconds = 5
  }

mockAppConfig :: AppConfig
mockAppConfig = AppConfig
  { configActiveLanguage = "spanish"
  , configProvider = mockProviderConfig
  , configUI = mockUIConfig
  , configDatabase = mockDatabaseConfig
  , configLogging = mockLoggingConfig
  , configAnki = mockAnkiClientConfig
  }

mockMessage :: Role -> Text -> Message
mockMessage role content = Message
  { messageId = Nothing
  , messageSessionId = "test-session"
  , messageRole = role
  , messageContent = content
  , messageMode = Conversation
  , messageTimestamp = mockTime
  , messageModel = Nothing
  , messageProvider = Nothing
  , messageTokensUsed = Nothing
  , messageError = Nothing
  }

-- Mock time for testing
mockTime :: UTCTime
mockTime = read "2024-01-01 00:00:00 UTC"

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ══════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Default Config (defaultRequestConfig)" $ do
    it "rcUseTools = True" $ do
      rcUseTools defaultRequestConfig `shouldBe` True

    it "rcForceTools = True" $ do
      rcForceTools defaultRequestConfig `shouldBe` True

    it "rcStream = False (tools don't stream)" $ do
      rcStream defaultRequestConfig `shouldBe` False

    it "rcIncludeHistory = True" $ do
      rcIncludeHistory defaultRequestConfig `shouldBe` True

    it "rcMaxHistory = 20" $ do
      rcMaxHistory defaultRequestConfig `shouldBe` 20

  describe "Request Building (buildRequest - No Tools)" $ do
    it "returns ChatRequest with crTools = Nothing" $ do
      let history = [mockMessage Assistant "Hi", mockMessage Assistant "Hello"]
      let current = mockMessage User "How are you?"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      crTools req `shouldBe` Nothing

    it "returns ChatRequest with crToolChoice = Nothing" $ do
      let history = [mockMessage Assistant "Hi", mockMessage Assistant "Hello"]
      let current = mockMessage User "How are you?"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      crToolChoice req `shouldBe` Nothing

    it "returns ChatRequest with crStream = True" $ do
      let history = [mockMessage Assistant "Hi", mockMessage Assistant "Hello"]
      let current = mockMessage User "How are you?"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      crStream req `shouldBe` True

    it "includes system prompt" $ do
      let history = []
      let current = mockMessage User "Hello"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      case crMessages req of
        (ChatMessage "system" _ : _) -> pure ()
        _ -> expectationFailure "Expected system message"

    it "includes user message" $ do
      let history = []
      let current = mockMessage User "Hello"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      let userMsgs = filter (\m -> cmRole m == "user") (crMessages req)
      length userMsgs `shouldBe` 1

    it "uses model from config" $ do
      let history = []
      let current = mockMessage User "Hello"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      crModel req `shouldBe` "test-model"

  describe "Request Building (buildRequestWithTools - With Tools)" $ do
    it "includes tools for Correction mode" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Correction history current
      crTools req `shouldSatisfy` (not . null)

    it "includes tools for Translation mode" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Translation history current
      crTools req `shouldSatisfy` (not . null)

    it "includes tools for CardGeneration mode" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule CardGeneration history current
      crTools req `shouldSatisfy` (not . null)

    it "crStream = False for tool requests" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Correction history current
      crStream req `shouldBe` False

    it "includes system prompt" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Translation history current
      case crMessages req of
        (ChatMessage "system" _ : _) -> pure ()
        _ -> expectationFailure "Expected system message"

  describe "History Management" $ do
    it "empty history + user message = 2 messages (system + user)" $ do
      let history = []
      let current = mockMessage User "Hello"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Conversation history current
      length (crMessages req) `shouldBe` 2

    it "2 history + user message = 4 messages (system + 2 history + user)" $ do
      let history = [mockMessage Assistant "Hi", mockMessage Assistant "Hello"]
      let current = mockMessage User "How are you?"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Conversation history current
      length (crMessages req) `shouldBe` 4

    it "25 history + user message = 22 messages (system + 20 history + user)" $ do
      let history = map (mockMessage Assistant) (replicate 25 "msg")
      let current = mockMessage User "Test"
      let req = buildRequestWithTools mockAppConfig mockLanguageModule Conversation history current
      -- 1 system + 20 (limited history) + 1 user = 22
      length (crMessages req) `shouldBe` 22

  describe "Request Serialization" $ do
    it "serializes to valid JSON" $ do
      let history = []
      let current = mockMessage User "Hello"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      let json = encode req
      json `shouldInclude` "\"model\""
      json `shouldInclude` "\"messages\""

  describe "Tool Selection Logic" $ do
    it "Conversation mode: no tools (uses streaming instead)" $ do
      let history = []
      let current = mockMessage User "Test"
      let req = buildRequest mockAppConfig mockLanguageModule Conversation history current
      crTools req `shouldBe` Nothing

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
