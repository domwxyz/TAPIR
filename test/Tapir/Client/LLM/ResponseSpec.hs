{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Client.LLM.ResponseSpec
-- Description : Tests for LLM response parsing
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.ResponseSpec (spec) where

import Test.Hspec
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text, isInfixOf)

import Tapir.Client.LLM.Response
import Tapir.Types (Mode(..))
import qualified Tapir.Types.Response as R (StructuredResponse(..), ConversationResponse(..), CorrectionResponse(..), TranslationResponse(..), CardResponse(..), ConversationResponse, convReply, convVocab, CorrectionResponse, crOriginal, crCorrected, crIsCorrect, crCorrections, TranslationResponse, trSourceText, trTargetText, trSourceLang, trTargetLang, CardResponse, cardRespFront, cardRespBack, cardRespTags)
import qualified Tapir.Types.Mode as M (Mode(..))
import Tapir.Client.LLM.Types

-- ════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════

-- Mock helper for creating tool call choices
toolCallChoice :: Text -> Text -> Choice
toolCallChoice funcName args = Choice
  { choiceIndex = 0
  , choiceMessage = ResponseMessage
      { rmRole = "assistant"
      , rmContent = Nothing
      , rmToolCalls = Just [ToolCall "call-123" "function" (FunctionCall funcName args)]
      }
  , choiceFinishReason = Just "stop"
  }

-- Mock helper for creating plain text choices
plainTextChoice :: Text -> Choice
plainTextChoice content = Choice
  { choiceIndex = 0
  , choiceMessage = ResponseMessage
      { rmRole = "assistant"
      , rmContent = Just content
      , rmToolCalls = Nothing
      }
  , choiceFinishReason = Just "stop"
  }

-- Valid JSON arguments for each tool
validConversationArgs :: Text
validConversationArgs = "{\"reply\":\"Hello!\",\"vocab_highlights\":[]}"

validCorrectionArgs :: Text
validCorrectionArgs = "{\"original\":\"test\",\"corrected\":\"test\",\"is_correct\":true,\"corrections\":[]}"

validTranslationArgs :: Text
validTranslationArgs = "{\"source_text\":\"Hi\",\"source_lang\":\"en\",\"target_text\":\"Hola\",\"target_lang\":\"es\"}"

validCardArgs :: Text
validCardArgs = "{\"front\":\"Hola\",\"back\":\"Hello\",\"tags\":[]}"

-- Mock responses
mockConversationResponse :: ChatResponse
mockConversationResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [toolCallChoice "send_conversation_reply" validConversationArgs]
  , respUsage = Nothing
  }

mockCorrectionResponse :: ChatResponse
mockCorrectionResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [toolCallChoice "submit_correction" validCorrectionArgs]
  , respUsage = Nothing
  }

mockTranslationResponse :: ChatResponse
mockTranslationResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [toolCallChoice "submit_translation" validTranslationArgs]
  , respUsage = Nothing
  }

mockCardResponse :: ChatResponse
mockCardResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [toolCallChoice "create_flashcard" validCardArgs]
  , respUsage = Nothing
  }

mockPlainTextResponse :: ChatResponse
mockPlainTextResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [plainTextChoice "plain response"]
  , respUsage = Nothing
  }

mockErrorResponse :: ChatResponse
mockErrorResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = []
  , respUsage = Nothing
  }

mockNoContentResponse :: ChatResponse
mockNoContentResponse = ChatResponse
  { respId = "test-id"
  , respModel = "test-model"
  , respChoices = [Choice 0 (ResponseMessage "assistant" Nothing Nothing) Nothing]
  , respUsage = Nothing
  }

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Main Parsing (parseResponse)" $ do
    it "extracts tool call from ChatResponse with tool_calls" $ do
      let result = parseResponse M.Conversation mockConversationResponse
      case result of
        ParsedStructured (R.SRConversation _) -> pure ()
        _ -> expectationFailure "Expected ParsedStructured SRConversation"

    it "returns ParsedStructured when tool call found" $ do
      let result = parseResponse M.Correction mockCorrectionResponse
      case result of
        ParsedStructured (R.SRCorrection _) -> pure ()
        _ -> expectationFailure "Expected ParsedStructured SRCorrection"

    it "falls back to ParsedRawText when no tool_call but has content" $ do
      let result = parseResponse M.Conversation mockPlainTextResponse
      case result of
        ParsedRawText "plain response" -> pure ()
        _ -> expectationFailure "Expected ParsedRawText"

    it "returns ParsedError when no choices" $ do
      let result = parseResponse M.Conversation mockErrorResponse
      case result of
        ParsedError _ -> pure ()
        _ -> expectationFailure "Expected ParsedError"

    it "returns ParsedError when no content and no tool_calls" $ do
      let result = parseResponse M.Conversation mockNoContentResponse
      case result of
        ParsedError _ -> pure ()
        _ -> expectationFailure "Expected ParsedError"

  describe "Tool Call Parsing (parseToolCallResponse)" $ do
    it "parses valid conversation tool call -> ParsedStructured SRConversation" $ do
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" validConversationArgs)
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedStructured (R.SRConversation conv) -> R.convReply conv `shouldBe` "Hello!"
        _ -> expectationFailure "Expected ParsedStructured SRConversation"

    it "parses valid correction tool call -> ParsedStructured SRCorrection" $ do
      let tc = ToolCall "call-123" "function" (FunctionCall "submit_correction" validCorrectionArgs)
      let result = parseToolCallResponse M.Correction tc
      case result of
        ParsedStructured (R.SRCorrection corr) -> R.crOriginal corr `shouldBe` "test"
        _ -> expectationFailure "Expected ParsedStructured SRCorrection"

    it "parses valid translation tool call -> ParsedStructured SRTranslation" $ do
      let tc = ToolCall "call-123" "function" (FunctionCall "submit_translation" validTranslationArgs)
      let result = parseToolCallResponse M.Translation tc
      case result of
        ParsedStructured (R.SRTranslation tr) -> R.trTargetText tr `shouldBe` "Hola"
        _ -> expectationFailure "Expected ParsedStructured SRTranslation"

    it "parses valid card tool call -> ParsedStructured SRCard" $ do
      let tc = ToolCall "call-123" "function" (FunctionCall "create_flashcard" validCardArgs)
      let result = parseToolCallResponse M.CardGeneration tc
      case result of
        ParsedStructured (R.SRCard card) -> R.cardRespFront card `shouldBe` "Hola"
        _ -> expectationFailure "Expected ParsedStructured SRCard"

    it "returns ParsedError for invalid JSON arguments" $ do
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" "invalid json {")
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedError err -> do
          let msg = peMessage err
          msg `shouldSatisfy` ("Failed to parse" `isInfixOf`)
        _ -> expectationFailure "Expected ParsedError"

    it "includes raw content in ParseError" $ do
      let badArgs = "invalid"
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" badArgs)
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedError err -> peRawContent err `shouldBe` Just badArgs
        _ -> expectationFailure "Expected ParsedError with raw content"

  describe "Content Extraction (extractContent)" $ do
    it "extracts content from response with plain text" $ do
      let result = extractContent mockPlainTextResponse
      result `shouldBe` Just "plain response"

    it "returns Nothing when content is null" $ do
      let resp = ChatResponse
            { respId = "test-id"
            , respModel = "test-model"
            , respChoices = [plainTextChoice ""]
            , respUsage = Nothing
            }
      let result = extractContent resp
      case result of
        Just "" -> pure ()
        Nothing -> pure ()
        _ -> expectationFailure "Expected Nothing or empty"

    it "returns Nothing when no choices" $ do
      let result = extractContent mockErrorResponse
      result `shouldBe` Nothing

  describe "Full Workflow Tests" $ do
    it "conversation mode full parse cycle" $ do
      let result = parseResponse M.Conversation mockConversationResponse
      case result of
        ParsedStructured (R.SRConversation conv) -> do
          R.convReply conv `shouldBe` "Hello!"
          R.convVocab conv `shouldBe` []
        _ -> expectationFailure "Expected SRConversation"

    it "correction mode full parse cycle" $ do
      let result = parseResponse M.Correction mockCorrectionResponse
      case result of
        ParsedStructured (R.SRCorrection corr) -> do
          R.crOriginal corr `shouldBe` "test"
          R.crCorrected corr `shouldBe` "test"
          R.crIsCorrect corr `shouldBe` True
        _ -> expectationFailure "Expected SRCorrection"

    it "translation mode full parse cycle" $ do
      let result = parseResponse M.Translation mockTranslationResponse
      case result of
        ParsedStructured (R.SRTranslation tr) -> do
          R.trSourceText tr `shouldBe` "Hi"
          R.trTargetText tr `shouldBe` "Hola"
          R.trSourceLang tr `shouldBe` "en"
          R.trTargetLang tr `shouldBe` "es"
        _ -> expectationFailure "Expected SRTranslation"

    it "card generation mode full parse cycle" $ do
      let result = parseResponse M.CardGeneration mockCardResponse
      case result of
        ParsedStructured (R.SRCard card) -> do
          R.cardRespFront card `shouldBe` "Hola"
          R.cardRespBack card `shouldBe` "Hello"
          R.cardRespTags card `shouldBe` []
        _ -> expectationFailure "Expected SRCard"

  describe "Error Handling" $ do
    it "handles missing required field in tool arguments" $ do
      let badArgs = "{\"vocab_highlights\":[]}"
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" badArgs)
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedError _ -> pure ()
        _ -> expectationFailure "Expected ParsedError"

    it "handles wrong type for required field" $ do
      let badArgs = "{\"reply\":123,\"vocab_highlights\":[]}"
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" badArgs)
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedError _ -> pure ()
        _ -> expectationFailure "Expected ParsedError"

    it "handles additional properties gracefully" $ do
      let extraArgs = "{\"reply\":\"Hello\",\"vocab_highlights\":[],\"extra_field\":\"ignored\"}"
      let tc = ToolCall "call-123" "function" (FunctionCall "send_conversation_reply" extraArgs)
      let result = parseToolCallResponse M.Conversation tc
      case result of
        ParsedStructured (R.SRConversation conv) -> R.convReply conv `shouldBe` "Hello"
        _ -> expectationFailure "Expected ParsedStructured"
