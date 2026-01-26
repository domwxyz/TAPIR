{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Types.Response
-- Description : Structured response types for all interaction modes
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the structured response types that the LLM returns
-- via tool/function calling. Each mode has its own response type with
-- fields optimized for language learning.

module Tapir.Types.Response
  ( -- * Top-Level Response
    StructuredResponse(..)
  , responseToText
  
    -- * Conversation Mode
  , ConversationResponse(..)
  , VocabHighlight(..)
  
    -- * Correction Mode
  , CorrectionResponse(..)
  , Correction(..)
  , CorrectionCategory(..)
  
    -- * Translation Mode
  , TranslationResponse(..)
  , TranslationNote(..)
  , Formality(..)
  
    -- * Card Generation Mode
  , CardResponse(..)
  
    -- * Parsing Utilities
  , parseResponseForMode
  ) where

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

import qualified Tapir.Types.Mode as M
import Tapir.Types.Mode (Mode)

-- ════════════════════════════════════════════════════════════════
-- TOP-LEVEL RESPONSE
-- ════════════════════════════════════════════════════════════════

-- | Unified structured response type for all modes
data StructuredResponse
  = SRConversation !ConversationResponse
  | SRCorrection !CorrectionResponse
  | SRTranslation !TranslationResponse
  | SRCard !CardResponse
  | SRRawText !Text  -- ^ Fallback for unparseable responses
  deriving (Eq, Show, Generic)

-- | Extract plain text from any structured response (for storage/display)
responseToText :: StructuredResponse -> Text
responseToText = \case
  SRConversation cr -> convReply cr
  SRCorrection cr   -> crCorrected cr
  SRTranslation tr  -> trTargetText tr
  SRCard cr         -> cardRespFront cr <> " - " <> cardRespBack cr
  SRRawText t       -> t

-- ════════════════════════════════════════════════════════════════
-- CONVERSATION MODE
-- ════════════════════════════════════════════════════════════════

-- | Response for free-form conversation practice
data ConversationResponse = ConversationResponse
  { convReply       :: !Text
    -- ^ The main conversational reply in target language
  , convCorrections :: ![Correction]
    -- ^ Corrections for any errors in learner's message (empty if perfect)
  , convVocab       :: ![VocabHighlight]
    -- ^ 1-2 vocabulary items worth learning from this exchange
  , convGrammarTip  :: !(Maybe Text)
    -- ^ Brief grammar note if relevant to the conversation
  } deriving (Eq, Show, Generic)

instance FromJSON ConversationResponse where
  parseJSON = withObject "ConversationResponse" $ \v -> ConversationResponse
    <$> v .:  "reply"
    <*> v .:? "corrections" .!= []
    <*> v .:? "vocab_highlights" .!= []
    <*> v .:? "grammar_tip"

instance ToJSON ConversationResponse where
  toJSON ConversationResponse{..} = object
    [ "reply"            .= convReply
    , "corrections"      .= convCorrections
    , "vocab_highlights" .= convVocab
    , "grammar_tip"      .= convGrammarTip
    ]

-- | A vocabulary item highlighted from the conversation
data VocabHighlight = VocabHighlight
  { vhWord         :: !Text
    -- ^ The word/phrase in target language
  , vhTranslation  :: !Text
    -- ^ Translation in native language
  , vhPartOfSpeech :: !(Maybe Text)
    -- ^ "noun", "verb", "adjective", etc.
  , vhGender       :: !(Maybe Text)
    -- ^ For gendered languages: "masculine", "feminine", "neuter"
  , vhPlural       :: !(Maybe Text)
    -- ^ Plural form if irregular
  } deriving (Eq, Show, Generic)

instance FromJSON VocabHighlight where
  parseJSON = withObject "VocabHighlight" $ \v -> VocabHighlight
    <$> v .:  "word"
    <*> v .:  "translation"
    <*> v .:? "part_of_speech"
    <*> v .:? "gender"
    <*> v .:? "plural"

instance ToJSON VocabHighlight where
  toJSON VocabHighlight{..} = object
    [ "word"           .= vhWord
    , "translation"    .= vhTranslation
    , "part_of_speech" .= vhPartOfSpeech
    , "gender"         .= vhGender
    , "plural"         .= vhPlural
    ]

-- ════════════════════════════════════════════════════════════════
-- CORRECTION MODE
-- ════════════════════════════════════════════════════════════════

-- | Response for grammar/spelling correction
data CorrectionResponse = CorrectionResponse
  { crOriginal      :: !Text
    -- ^ The original text submitted by learner
  , crCorrected     :: !Text
    -- ^ The corrected version
  , crIsCorrect     :: !Bool
    -- ^ True if no corrections were needed
  , crCorrections   :: ![Correction]
    -- ^ List of individual corrections with explanations
  , crEncouragement :: !(Maybe Text)
    -- ^ Encouraging message ("Great job!", "Keep practicing!")
  , crOverallNote   :: !(Maybe Text)
    -- ^ General feedback about writing style/level
  } deriving (Eq, Show, Generic)

instance FromJSON CorrectionResponse where
  parseJSON = withObject "CorrectionResponse" $ \v -> CorrectionResponse
    <$> v .:  "original"
    <*> v .:  "corrected"
    <*> v .:? "is_correct" .!= False
    <*> v .:? "corrections" .!= []
    <*> v .:? "encouragement"
    <*> v .:? "overall_note"

instance ToJSON CorrectionResponse where
  toJSON CorrectionResponse{..} = object
    [ "original"      .= crOriginal
    , "corrected"     .= crCorrected
    , "is_correct"    .= crIsCorrect
    , "corrections"   .= crCorrections
    , "encouragement" .= crEncouragement
    , "overall_note"  .= crOverallNote
    ]

-- | A single correction with explanation
data Correction = Correction
  { corrOriginal    :: !Text
    -- ^ The incorrect word/phrase
  , corrFixed       :: !Text
    -- ^ The corrected word/phrase
  , corrExplanation :: !Text
    -- ^ Why this was wrong and how to remember
  , corrCategory    :: !(Maybe CorrectionCategory)
    -- ^ Type of error
  , corrSeverity    :: !(Maybe Text)
    -- ^ "minor", "moderate", "significant"
  } deriving (Eq, Show, Generic)

instance FromJSON Correction where
  parseJSON = withObject "Correction" $ \v -> Correction
    <$> v .:  "original"
    <*> v .:  "fixed"
    <*> v .:  "explanation"
    <*> v .:? "category"
    <*> v .:? "severity"

instance ToJSON Correction where
  toJSON Correction{..} = object
    [ "original"    .= corrOriginal
    , "fixed"       .= corrFixed
    , "explanation" .= corrExplanation
    , "category"    .= corrCategory
    , "severity"    .= corrSeverity
    ]

-- | Category of correction
data CorrectionCategory
  = CatGrammar
  | CatSpelling
  | CatWordChoice
  | CatPunctuation
  | CatStyle
  | CatGender       -- ^ Gender agreement errors
  | CatConjugation  -- ^ Verb conjugation errors
  | CatAccent       -- ^ Missing/wrong accent marks
  | CatOther
  deriving (Eq, Show, Generic)

instance FromJSON CorrectionCategory where
  parseJSON = withText "CorrectionCategory" $ \case
    "grammar"     -> pure CatGrammar
    "spelling"    -> pure CatSpelling
    "word_choice" -> pure CatWordChoice
    "punctuation" -> pure CatPunctuation
    "style"       -> pure CatStyle
    "gender"      -> pure CatGender
    "conjugation" -> pure CatConjugation
    "accent"      -> pure CatAccent
    _             -> pure CatOther

instance ToJSON CorrectionCategory where
  toJSON = \case
    CatGrammar     -> "grammar"
    CatSpelling    -> "spelling"
    CatWordChoice  -> "word_choice"
    CatPunctuation -> "punctuation"
    CatStyle       -> "style"
    CatGender      -> "gender"
    CatConjugation -> "conjugation"
    CatAccent      -> "accent"
    CatOther       -> "other"

-- ════════════════════════════════════════════════════════════════
-- TRANSLATION MODE
-- ════════════════════════════════════════════════════════════════

-- | Response for translation requests
data TranslationResponse = TranslationResponse
  { trSourceText    :: !Text
    -- ^ Original text that was translated
  , trSourceLang    :: !Text
    -- ^ Detected/specified source language
  , trTargetText    :: !Text
    -- ^ The translation
  , trTargetLang    :: !Text
    -- ^ Target language
  , trNotes         :: ![TranslationNote]
    -- ^ Notes on tricky translations, idioms, cultural context
  , trAlternatives  :: ![Text]
    -- ^ Alternative translations if ambiguous
  , trFormality     :: !(Maybe Formality)
    -- ^ Register of the translation
  , trLiteralMeaning :: !(Maybe Text)
    -- ^ Literal translation if idiomatic (helpful for learners)
  } deriving (Eq, Show, Generic)

instance FromJSON TranslationResponse where
  parseJSON = withObject "TranslationResponse" $ \v -> TranslationResponse
    <$> v .:  "source_text"
    <*> v .:  "source_lang"
    <*> v .:  "target_text"
    <*> v .:  "target_lang"
    <*> v .:? "notes" .!= []
    <*> v .:? "alternatives" .!= []
    <*> v .:? "formality"
    <*> v .:? "literal_meaning"

instance ToJSON TranslationResponse where
  toJSON TranslationResponse{..} = object
    [ "source_text"     .= trSourceText
    , "source_lang"     .= trSourceLang
    , "target_text"     .= trTargetText
    , "target_lang"     .= trTargetLang
    , "notes"           .= trNotes
    , "alternatives"    .= trAlternatives
    , "formality"       .= trFormality
    , "literal_meaning" .= trLiteralMeaning
    ]

-- | A note explaining a translation choice
data TranslationNote = TranslationNote
  { tnPhrase      :: !Text
    -- ^ The phrase being explained
  , tnExplanation :: !Text
    -- ^ Why it was translated this way
  , tnCultural    :: !(Maybe Text)
    -- ^ Cultural context if relevant
  } deriving (Eq, Show, Generic)

instance FromJSON TranslationNote where
  parseJSON = withObject "TranslationNote" $ \v -> TranslationNote
    <$> v .: "phrase"
    <*> v .: "explanation"
    <*> v .:? "cultural_context"

instance ToJSON TranslationNote where
  toJSON TranslationNote{..} = object
    [ "phrase"           .= tnPhrase
    , "explanation"      .= tnExplanation
    , "cultural_context" .= tnCultural
    ]

-- | Formality level of translation
data Formality = Formal | Informal | Neutral
  deriving (Eq, Show, Generic)

instance FromJSON Formality where
  parseJSON = withText "Formality" $ \case
    "formal"   -> pure Formal
    "informal" -> pure Informal
    "neutral"  -> pure Neutral
    _          -> pure Neutral

instance ToJSON Formality where
  toJSON Formal   = "formal"
  toJSON Informal = "informal"
  toJSON Neutral  = "neutral"

-- ════════════════════════════════════════════════════════════════
-- CARD GENERATION MODE
-- ════════════════════════════════════════════════════════════════

-- | Response for Anki card generation
data CardResponse = CardResponse
  { cardRespFront        :: !Text
    -- ^ Front of card (target language)
  , cardRespBack         :: !Text
    -- ^ Back of card (native language + context)
  , cardRespTags         :: ![Text]
    -- ^ Suggested tags
  , cardRespExample      :: !(Maybe Text)
    -- ^ Example sentence using the word
  , cardRespExampleTrans :: !(Maybe Text)
    -- ^ Translation of the example sentence
  , cardRespPronunciation :: !(Maybe Text)
    -- ^ IPA or phonetic pronunciation
  , cardRespAudio        :: !(Maybe Text)
    -- ^ URL to audio pronunciation (if available)
  , cardRespNotes        :: !(Maybe Text)
    -- ^ Usage notes, conjugation patterns, etc.
  , cardRespRelated      :: ![Text]
    -- ^ Related words (synonyms, antonyms, word family)
  , cardRespMnemonic     :: !(Maybe Text)
    -- ^ Memory aid/mnemonic device
  } deriving (Eq, Show, Generic)

instance FromJSON CardResponse where
  parseJSON = withObject "CardResponse" $ \v -> CardResponse
    <$> v .:  "front"
    <*> v .:  "back"
    <*> v .:? "tags" .!= []
    <*> v .:? "example"
    <*> v .:? "example_translation"
    <*> v .:? "pronunciation"
    <*> v .:? "audio_url"
    <*> v .:? "notes"
    <*> v .:? "related_words" .!= []
    <*> v .:? "mnemonic"

instance ToJSON CardResponse where
  toJSON CardResponse{..} = object
    [ "front"               .= cardRespFront
    , "back"                .= cardRespBack
    , "tags"                .= cardRespTags
    , "example"             .= cardRespExample
    , "example_translation" .= cardRespExampleTrans
    , "pronunciation"       .= cardRespPronunciation
    , "audio_url"           .= cardRespAudio
    , "notes"               .= cardRespNotes
    , "related_words"       .= cardRespRelated
    , "mnemonic"            .= cardRespMnemonic
    ]

-- ════════════════════════════════════════════════════════════════
-- PARSING UTILITIES
-- ════════════════════════════════════════════════════════════════

-- | Parse a JSON string into a StructuredResponse based on mode
parseResponseForMode :: M.Mode -> Text -> Either Text StructuredResponse
parseResponseForMode mode jsonText =
  let bs = BL.fromStrict (TE.encodeUtf8 jsonText)
  in case mode of
    M.Conversation   -> bimap T.pack SRConversation (eitherDecode bs)
    M.Correction     -> bimap T.pack SRCorrection (eitherDecode bs)
    M.Translation    -> bimap T.pack SRTranslation (eitherDecode bs)
    M.CardGeneration -> bimap T.pack SRCard (eitherDecode bs)
    M.CustomMode _   -> Right (SRRawText jsonText)
