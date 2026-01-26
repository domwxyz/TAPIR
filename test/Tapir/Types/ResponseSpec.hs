{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Types.ResponseSpec
-- Description : Tests for structured response types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Types.ResponseSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL

import Tapir.Types.Response
import qualified Tapir.Types.Mode as M (Mode(..))

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "ConversationResponse" $ do
    it "parses minimal JSON with only required fields" $ do
      let json = BL.concat
            [ "{\"reply\":\"Hello friend\","
            , "\"vocab_highlights\":[]}"
            ]
      case eitherDecode json of
        Right (resp :: ConversationResponse) -> do
          convReply resp `shouldBe` "Hello friend"
          convVocab resp `shouldBe` []
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses full JSON with all optional fields" $ do
      let json = BL.concat
            [ "{\"reply\":\"Hola\","
            , "\"corrections\":[{\"original\":\"yo comer\",\"fixed\":\"yo como\",\"explanation\":\"conjugation\"}],"
            , "\"vocab_highlights\":[{\"word\":\"amigo\",\"translation\":\"friend\"}],"
            , "\"grammar_tip\":\"Noun-adjective agreement\"}"
            ]
      case eitherDecode json of
        Right (resp :: ConversationResponse) -> do
          convReply resp `shouldBe` "Hola"
          convGrammarTip resp `shouldBe` Just "Noun-adjective agreement"
          length (convCorrections resp) `shouldBe` 1
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses vocab_highlights array" $ do
      let json = BL.concat
            [ "{\"reply\":\"Test\","
            , "\"vocab_highlights\":["
            , "{\"word\":\"casa\",\"translation\":\"house\",\"part_of_speech\":\"noun\"},"
            , "{\"word\":\"grande\",\"translation\":\"big\",\"part_of_speech\":\"adjective\"}"
            , "]}"
            ]
      case eitherDecode json of
        Right (resp :: ConversationResponse) -> do
          length (convVocab resp) `shouldBe` 2
          vhWord (head (convVocab resp)) `shouldBe` "casa"
          vhTranslation (head (convVocab resp)) `shouldBe` "house"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses empty vocab_highlights array" $ do
      let json = BL.concat
            [ "{\"reply\":\"Test\","
            , "\"vocab_highlights\":[]}"
            ]
      case eitherDecode json of
        Right (resp :: ConversationResponse) -> do
          convVocab resp `shouldBe` []
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses VocabHighlight with all fields" $ do
      let json = BL.concat
            [ "{\"word\":\"libro\","
            , "\"translation\":\"book\","
            , "\"part_of_speech\":\"noun\","
            , "\"gender\":\"masculine\","
            , "\"plural\":\"libros\"}"
            ]
      case eitherDecode json of
        Right (vh :: VocabHighlight) -> do
          vhWord vh `shouldBe` "libro"
          vhTranslation vh `shouldBe` "book"
          vhPartOfSpeech vh `shouldBe` Just "noun"
          vhGender vh `shouldBe` Just "masculine"
          vhPlural vh `shouldBe` Just "libros"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses VocabHighlight with only required fields" $ do
      let json = BL.concat
            [ "{\"word\":\"agua\","
            , "\"translation\":\"water\"}"
            ]
      case eitherDecode json of
        Right (vh :: VocabHighlight) -> do
          vhWord vh `shouldBe` "agua"
          vhTranslation vh `shouldBe` "water"
          vhPartOfSpeech vh `shouldBe` Nothing
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "roundtrips through JSON encode/decode" $ do
      let resp = ConversationResponse
            { convReply = "Testing reply"
            , convCorrections = []
            , convVocab = [VocabHighlight "test" "testing" (Just "noun") Nothing Nothing]
            , convGrammarTip = Just "Tip"
            }
      decode (encode resp) `shouldBe` Just resp

  describe "CorrectionResponse" $ do
    it "parses perfect response (is_correct=true, empty corrections)" $ do
      let json = BL.concat
            [ "{\"original\":\"Test text\","
            , "\"corrected\":\"Test text\","
            , "\"is_correct\":true,"
            , "\"corrections\":[]}"
            ]
      case eitherDecode json of
        Right (resp :: CorrectionResponse) -> do
          crIsCorrect resp `shouldBe` True
          crCorrections resp `shouldBe` []
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses response with one or more corrections" $ do
      let json = BL.concat
            [ "{\"original\":\"yo comer\","
            , "\"corrected\":\"yo como\","
            , "\"is_correct\":false,"
            , "\"corrections\":[{\"original\":\"comer\",\"fixed\":\"como\","
            , "\"explanation\":\"Conjugate for first person\"}]}"
            ]
      case eitherDecode json of
        Right (resp :: CorrectionResponse) -> do
          length (crCorrections resp) `shouldBe` 1
          corrOriginal (head (crCorrections resp)) `shouldBe` "comer"
          corrFixed (head (crCorrections resp)) `shouldBe` "como"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses Correction with all fields" $ do
      let json = BL.concat
            [ "{\"original\":\"el casa\","
            , "\"fixed\":\"la casa\","
            , "\"explanation\":\"Casa is feminine\","
            , "\"category\":\"gender\","
            , "\"severity\":\"moderate\"}"
            ]
      case eitherDecode json of
        Right (corr :: Correction) -> do
          corrOriginal corr `shouldBe` "el casa"
          corrFixed corr `shouldBe` "la casa"
          corrCategory corr `shouldBe` Just CatGender
          corrSeverity corr `shouldBe` Just "moderate"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses Correction with only required fields" $ do
      let json = BL.concat
            [ "{\"original\":\"bad\","
            , "\"fixed\":\"good\","
            , "\"explanation\":\"Use better word\"}"
            ]
      case eitherDecode json of
        Right (corr :: Correction) -> do
          corrOriginal corr `shouldBe` "bad"
          corrFixed corr `shouldBe` "good"
          corrCategory corr `shouldBe` Nothing
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses all CorrectionCategory values" $ do
      let corr1 = Correction "a" "b" "test" (Just CatGrammar) Nothing
      let corr2 = Correction "a" "b" "test" (Just CatSpelling) Nothing
      let corr3 = Correction "a" "b" "test" (Just CatWordChoice) Nothing
      let corr4 = Correction "a" "b" "test" (Just CatPunctuation) Nothing
      let corr5 = Correction "a" "b" "test" (Just CatStyle) Nothing
      let corr6 = Correction "a" "b" "test" (Just CatGender) Nothing
      let corr7 = Correction "a" "b" "test" (Just CatConjugation) Nothing
      let corr8 = Correction "a" "b" "test" (Just CatAccent) Nothing
      let corr9 = Correction "a" "b" "test" (Just CatOther) Nothing
      
      let getCat c = case corrCategory <$> decode (encode c) of
            Just (Just cat) -> Just cat
            _ -> Nothing
      
      getCat corr1 `shouldBe` Just CatGrammar
      getCat corr2 `shouldBe` Just CatSpelling
      getCat corr3 `shouldBe` Just CatWordChoice
      getCat corr4 `shouldBe` Just CatPunctuation
      getCat corr5 `shouldBe` Just CatStyle
      getCat corr6 `shouldBe` Just CatGender
      getCat corr7 `shouldBe` Just CatConjugation
      getCat corr8 `shouldBe` Just CatAccent
      getCat corr9 `shouldBe` Just CatOther

    it "parses encouragement and overall_note fields" $ do
      let resp = CorrectionResponse "Test" "Test" True [] (Just "Great job!") (Just "Keep practicing")
      let json = encode resp
      case decode json of
        Just r -> do
          crEncouragement r `shouldBe` Just "Great job!"
          crOverallNote r `shouldBe` Just "Keep practicing"
        Nothing -> expectationFailure "Failed to decode"

    it "parses encouragement and overall_note fields" $ do
      let json = BL.concat
            [ "{\"original\":\"Test\","
            , "\"corrected\":\"Test\","
            , "\"is_correct\":true,"
            , "\"corrections\":[],"
            , "\"encouragement\":\"Great job!\","
            , "\"overall_note\":\"Keep practicing\"}"
            ]
      case eitherDecode json of
        Right (resp :: CorrectionResponse) -> do
          crEncouragement resp `shouldBe` Just "Great job!"
          crOverallNote resp `shouldBe` Just "Keep practicing"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

    it "roundtrips through JSON encode/decode" $ do
      let resp = CorrectionResponse
            { crOriginal = "Test original"
            , crCorrected = "Test corrected"
            , crIsCorrect = False
            , crCorrections = []
            , crEncouragement = Just "Good"
            , crOverallNote = Nothing
            }
      decode (encode resp) `shouldBe` Just resp

  describe "TranslationResponse" $ do
    it "parses minimal JSON with only required fields" $ do
      let resp = TranslationResponse "Hello" "English" "Hola" "Spanish" [] [] Nothing Nothing
      let json = encode resp
      case decode json of
        Just r -> do
          trSourceText r `shouldBe` "Hello"
          trTargetText r `shouldBe` "Hola"
        Nothing -> expectationFailure "Failed to decode"

      let note1 = TranslationNote "Hola" "Standard greeting" Nothing
      let resp = TranslationResponse "Hello" "English" "Hola" "Spanish" [note1] ["Buenos días"] (Just Formal) (Just "Word for word: Hola")
      let json = encode resp
      case decode json of
        Just r -> do
          trFormality r `shouldBe` Just Formal
          trLiteralMeaning r `shouldBe` Just "Word for word: Hola"
          length (trNotes r) `shouldBe` 1
        Nothing -> expectationFailure "Failed to decode"

    it "parses TranslationNote array" $ do
      let note1 = TranslationNote "phrase1" "explanation1" Nothing
      let note2 = TranslationNote "phrase2" "explanation2" Nothing
      let resp = TranslationResponse "Test" "en" "Prueba" "es" [note1, note2] [] Nothing Nothing
      let json = encode resp
      case decode json of
        Just r -> do
          length (trNotes r) `shouldBe` 2
          tnPhrase (head (trNotes r)) `shouldBe` "phrase1"
        Nothing -> expectationFailure "Failed to decode"

    it "parses TranslationNote with cultural_context" $ do
      let note = TranslationNote "phrase" "explanation" (Just "context")
      let resp = TranslationResponse "Test" "en" "Prueba" "es" [note] [] Nothing Nothing
      let json = encode resp
      case decode json of
        Just r -> do
          let n = head (trNotes r)
          tnCultural n `shouldBe` Just "context"
        Nothing -> expectationFailure "Failed to decode"

    it "parses alternatives array" $ do
      let resp = TranslationResponse "Hi" "English" "Hola" "Spanish" [] ["Buenos días", "Buenas tardes"] Nothing Nothing
      case decode (encode resp) of
        Just r -> trAlternatives r `shouldBe` ["Buenos días", "Buenas tardes"]
        Nothing -> expectationFailure "Failed to decode"

    it "parses TranslationNote without cultural_context" $ do
      let note = TranslationNote "phrase" "explanation" Nothing
      let resp = TranslationResponse "Test" "en" "Prueba" "es" [note] [] Nothing Nothing
      case decode (encode resp) of
        Just r -> do
          let n = head (trNotes r)
          tnCultural n `shouldBe` Nothing
        Nothing -> expectationFailure "Failed to decode"

    it "parses all Formality values" $ do
      let resp1 = TranslationResponse "a" "en" "b" "es" [] [] (Just Formal) Nothing
      let resp2 = TranslationResponse "a" "en" "b" "es" [] [] (Just Informal) Nothing
      let resp3 = TranslationResponse "a" "en" "b" "es" [] [] (Just Neutral) Nothing
      
      let getForm r = case trFormality <$> decode (encode r) of
            Just (Just f) -> Just f
            _ -> Nothing
      
      getForm resp1 `shouldBe` Just Formal
      getForm resp2 `shouldBe` Just Informal
      getForm resp3 `shouldBe` Just Neutral

    it "parses alternatives array" $ do
      let resp = TranslationResponse "Hi" "English" "Hola" "Spanish" [] ["Buenos días", "Buenas tardes"] Nothing Nothing
      let json = encode resp
      case decode json of
        Just r -> trAlternatives r `shouldBe` ["Buenos días", "Buenas tardes"]
        Nothing -> expectationFailure "Failed to decode"

    it "roundtrips through JSON encode/decode" $ do
      let resp = TranslationResponse
            { trSourceText = "Hello"
            , trSourceLang = "English"
            , trTargetText = "Hola"
            , trTargetLang = "Spanish"
            , trNotes = []
            , trAlternatives = []
            , trFormality = Nothing
            , trLiteralMeaning = Nothing
            }
      decode (encode resp) `shouldBe` Just resp

  describe "CardResponse" $ do
    it "parses minimal JSON with only required fields" $ do
      let resp = CardResponse "Hola" "Hello" [] Nothing Nothing Nothing Nothing Nothing [] Nothing
      let json = encode resp
      case decode json of
        Just r -> do
          cardRespFront r `shouldBe` "Hola"
          cardRespBack r `shouldBe` "Hello"
          cardRespTags r `shouldBe` []
        Nothing -> expectationFailure "Failed to decode"

    it "parses full JSON with all optional fields" $ do
      let resp = CardResponse
            "Hola" "Hello (greeting)" ["greeting", "basic"]
            (Just "Hola, ¿cómo estás?") (Just "Hello, how are you?")
            (Just "/ˈola/") (Just "https://example.com/hola.mp3")
            (Just "Common greeting")
            ["buenos días", "adiós"] (Just "Think of 'hula' dance")
      let json = encode resp
      case decode json of
        Just r -> do
          cardRespExample r `shouldBe` Just "Hola, ¿cómo estás?"
          cardRespPronunciation r `shouldBe` Just "/ˈola/"
          cardRespMnemonic r `shouldBe` Just "Think of 'hula' dance"
        Nothing -> expectationFailure "Failed to decode"

    it "parses empty tags array" $ do
      let resp = CardResponse {cardRespFront = "Test", cardRespBack = "Testing", cardRespTags = [], cardRespExample = Nothing, cardRespExampleTrans = Nothing, cardRespPronunciation = Nothing, cardRespAudio = Nothing, cardRespNotes = Nothing, cardRespRelated = [], cardRespMnemonic = Nothing}
      let json = encode resp
      case decode json of
        Just r -> cardRespTags r `shouldBe` []
        Nothing -> expectationFailure "Failed to decode"

    it "parses related_words array" $ do
      let resp = CardResponse {cardRespFront = "big", cardRespBack = "grande", cardRespTags = [], cardRespExample = Nothing, cardRespExampleTrans = Nothing, cardRespPronunciation = Nothing, cardRespAudio = Nothing, cardRespNotes = Nothing, cardRespRelated = ["pequeño", "mediano", "enorme"], cardRespMnemonic = Nothing}
      let json = encode resp
      case decode json of
        Just r -> cardRespRelated r `shouldBe` ["pequeño", "mediano", "enorme"]
        Nothing -> expectationFailure "Failed to decode"

    it "parses related_words array (positional)" $ do
      let resp = CardResponse "big" "grande" [] Nothing Nothing Nothing Nothing Nothing ["pequeño", "mediano", "enorme"] Nothing
      let json = encode resp
      case decode json of
        Just r -> cardRespRelated r `shouldBe` ["pequeño", "mediano", "enorme"]
        Nothing -> expectationFailure "Failed to decode"

    it "roundtrips through JSON encode/decode" $ do
      let resp = CardResponse
            "Test" "Testing" ["tag1", "tag2"] Nothing Nothing Nothing Nothing Nothing [] Nothing
      let json = encode resp
      decode (encode resp) `shouldBe` Just resp

  describe "parseResponseForMode" $ do
    it "parses valid JSON for Conversation mode -> SRConversation" $ do
      let jsonText = "{\"reply\":\"Hola\",\"vocab_highlights\":[]}"
      case parseResponseForMode M.Conversation jsonText of
        Right (SRConversation conv) -> convReply conv `shouldBe` "Hola"
        _ -> expectationFailure "Expected SRConversation"

    it "parses valid JSON for Correction mode -> SRCorrection" $ do
      let jsonText = "{\"original\":\"test\",\"corrected\":\"test\",\"is_correct\":true,\"corrections\":[]}"
      case parseResponseForMode M.Correction jsonText of
        Right (SRCorrection corr) -> crOriginal corr `shouldBe` "test"
        _ -> expectationFailure "Expected SRCorrection"

    it "parses valid JSON for Translation mode -> SRTranslation" $ do
      let jsonText = "{\"source_text\":\"Hi\",\"source_lang\":\"en\",\"target_text\":\"Hola\",\"target_lang\":\"es\"}"
      case parseResponseForMode M.Translation jsonText of
        Right (SRTranslation tr) -> trTargetText tr `shouldBe` "Hola"
        _ -> expectationFailure "Expected SRTranslation"

    it "parses valid JSON for CardGeneration mode -> SRCard" $ do
      let jsonText = "{\"front\":\"test\",\"back\":\"testing\",\"tags\":[]}"
      case parseResponseForMode M.CardGeneration jsonText of
        Right (SRCard card) -> cardRespFront card `shouldBe` "test"
        _ -> expectationFailure "Expected SRCard"

    it "returns SRRawText for CustomMode" $ do
      let jsonText = "{\"invalid\":\"json\"}"
      case parseResponseForMode (M.CustomMode "test") jsonText of
        Right (SRRawText t) -> t `shouldBe` jsonText
        _ -> expectationFailure "Expected SRRawText"

    it "returns Left error for invalid JSON" $ do
      let jsonText = "invalid json {"
      case parseResponseForMode M.Conversation jsonText of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected Left error"

  describe "responseToText" $ do
    it "extracts convReply from SRConversation" $ do
      let conv = ConversationResponse "Reply text" [] [] Nothing
      responseToText (SRConversation conv) `shouldBe` "Reply text"

    it "extracts crCorrected from SRCorrection" $ do
      let corr = CorrectionResponse "orig" "corrected" True [] Nothing Nothing
      responseToText (SRCorrection corr) `shouldBe` "corrected"

    it "extracts trTargetText from SRTranslation" $ do
      let tr = TranslationResponse "src" "en" "target" "es" [] [] Nothing Nothing
      responseToText (SRTranslation tr) `shouldBe` "target"

    it "extracts cardRespFront + ' - ' + cardRespBack from SRCard" $ do
      let card = CardResponse "front" "back" [] Nothing Nothing Nothing Nothing Nothing [] Nothing
      responseToText (SRCard card) `shouldBe` "front - back"

    it "returns raw text for SRRawText" $ do
      responseToText (SRRawText "raw text") `shouldBe` "raw text"
