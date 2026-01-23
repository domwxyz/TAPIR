{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Tapir.Client.LLM.Tools
-- Description : Tool/function definitions for structured LLM output
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the tool schemas that instruct LLMs to return
-- structured JSON responses. The schemas are designed to be compatible
-- with OpenAI, Anthropic, and OpenRouter APIs.

module Tapir.Client.LLM.Tools
  ( -- * Tool Definitions
    Tool(..)
  , ToolFunction(..)
  , ToolChoice(..)
  
    -- * Mode-Specific Tools
  , conversationTool
  , correctionTool
  , translationTool
  , cardGenerationTool
  
    -- * Utilities
  , toolForMode
  , toolNameForMode
  , allTools
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Tapir.Types.Mode (Mode(..))

-- ════════════════════════════════════════════════════════════════
-- TOOL TYPES
-- ════════════════════════════════════════════════════════════════

-- | Tool definition (OpenAI/OpenRouter format)
data Tool = Tool
  { toolType     :: !Text        -- ^ Always "function"
  , toolFunction :: !ToolFunction
  } deriving (Eq, Show, Generic)

instance ToJSON Tool where
  toJSON Tool{..} = object
    [ "type"     .= toolType
    , "function" .= toolFunction
    ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \v -> Tool
    <$> v .: "type"
    <*> v .: "function"

-- | Function definition within a tool
data ToolFunction = ToolFunction
  { tfName        :: !Text   -- ^ Function name (used for routing)
  , tfDescription :: !Text   -- ^ Description for the LLM
  , tfParameters  :: !Value  -- ^ JSON Schema for parameters
  , tfStrict      :: !Bool   -- ^ Enforce strict schema (OpenAI)
  } deriving (Eq, Show, Generic)

instance ToJSON ToolFunction where
  toJSON ToolFunction{..} = object
    [ "name"        .= tfName
    , "description" .= tfDescription
    , "parameters"  .= tfParameters
    , "strict"      .= tfStrict
    ]

instance FromJSON ToolFunction where
  parseJSON = withObject "ToolFunction" $ \v -> ToolFunction
    <$> v .: "name"
    <*> v .: "description"
    <*> v .: "parameters"
    <*> v .:? "strict" .!= False

-- | Tool choice specification
data ToolChoice
  = ToolChoiceAuto              -- ^ Let model decide
  | ToolChoiceNone              -- ^ Don't use tools
  | ToolChoiceRequired          -- ^ Must use a tool
  | ToolChoiceForced !Text      -- ^ Must use specific tool
  deriving (Eq, Show, Generic)

instance ToJSON ToolChoice where
  toJSON ToolChoiceAuto     = String "auto"
  toJSON ToolChoiceNone     = String "none"
  toJSON ToolChoiceRequired = String "required"
  toJSON (ToolChoiceForced name) = object
    [ "type" .= ("function" :: Text)
    , "function" .= object ["name" .= name]
    ]

-- ════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════

-- | Get the appropriate tool for a mode
toolForMode :: Mode -> Maybe Tool
toolForMode Conversation   = Just conversationTool
toolForMode Correction     = Just correctionTool
toolForMode Translation    = Just translationTool
toolForMode CardGeneration = Just cardGenerationTool
toolForMode (CustomMode _) = Nothing

-- | Get tool name for a mode (for ToolChoiceForced)
toolNameForMode :: Mode -> Maybe Text
toolNameForMode mode = tfName . toolFunction <$> toolForMode mode

-- | All defined tools
allTools :: [Tool]
allTools = 
  [ conversationTool
  , correctionTool
  , translationTool
  , cardGenerationTool
  ]

-- ════════════════════════════════════════════════════════════════
-- CONVERSATION TOOL
-- ════════════════════════════════════════════════════════════════

conversationTool :: Tool
conversationTool = Tool "function" ToolFunction
  { tfName = "send_conversation_reply"
  , tfDescription = "Send a conversational reply with vocabulary highlights and optional learning aids. Use this for all conversation responses."
  , tfStrict = True
  , tfParameters = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "reply" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Your conversational reply in the target language. Be natural and engaging." :: Text)
              ]
          , "vocab_highlights" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("2-3 vocabulary items from your reply that are useful for the learner to know" :: Text)
              , "items" .= vocabHighlightSchema
              ]
          , "grammar_tip" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Brief grammar note if your reply uses an interesting or tricky construction" :: Text)
              ]
          , "follow_up" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("A suggested follow-up question to keep the conversation going" :: Text)
              ]
          , "corrections" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("Gentle corrections if the learner made errors in their message" :: Text)
              , "items" .= correctionSchema
              ]
          ]
      , "required" .= (["reply", "vocab_highlights"] :: [Text])
      ]
  }

-- ════════════════════════════════════════════════════════════════
-- CORRECTION TOOL
-- ════════════════════════════════════════════════════════════════

correctionTool :: Tool
correctionTool = Tool "function" ToolFunction
  { tfName = "submit_correction"
  , tfDescription = "Analyze and correct the learner's text, providing detailed explanations for each correction."
  , tfStrict = True
  , tfParameters = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "original" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The original text submitted by the learner, exactly as written" :: Text)
              ]
          , "corrected" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The fully corrected version of the text" :: Text)
              ]
          , "is_correct" .= object
              [ "type" .= ("boolean" :: Text)
              , "description" .= ("True if the original text had no errors" :: Text)
              ]
          , "corrections" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("List of individual corrections, empty if is_correct is true" :: Text)
              , "items" .= correctionSchema
              ]
          , "encouragement" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("A brief encouraging message for the learner" :: Text)
              ]
          , "overall_note" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("General feedback about writing style or level-appropriate suggestions" :: Text)
              ]
          ]
      , "required" .= (["original", "corrected", "is_correct", "corrections"] :: [Text])
      ]
  }

-- ════════════════════════════════════════════════════════════════
-- TRANSLATION TOOL
-- ════════════════════════════════════════════════════════════════

translationTool :: Tool
translationTool = Tool "function" ToolFunction
  { tfName = "submit_translation"
  , tfDescription = "Provide a translation with notes on tricky phrases, alternatives, and cultural context."
  , tfStrict = True
  , tfParameters = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "source_text" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The original text being translated" :: Text)
              ]
          , "source_lang" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The detected or specified source language (e.g., 'English', 'Spanish')" :: Text)
              ]
          , "target_text" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The translated text" :: Text)
              ]
          , "target_lang" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The target language (e.g., 'Spanish', 'English')" :: Text)
              ]
          , "notes" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("Notes explaining tricky translations, idioms, or cultural context" :: Text)
              , "items" .= translationNoteSchema
              ]
          , "alternatives" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("Alternative translations if the source is ambiguous" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              ]
          , "formality" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["formal", "informal", "neutral"] :: [Text])
              , "description" .= ("The register/formality level of the translation" :: Text)
              ]
          , "literal_meaning" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Literal word-for-word translation if the main translation is idiomatic" :: Text)
              ]
          ]
      , "required" .= (["source_text", "source_lang", "target_text", "target_lang"] :: [Text])
      ]
  }

-- ════════════════════════════════════════════════════════════════
-- CARD GENERATION TOOL
-- ════════════════════════════════════════════════════════════════

cardGenerationTool :: Tool
cardGenerationTool = Tool "function" ToolFunction
  { tfName = "create_flashcard"
  , tfDescription = "Create an Anki flashcard with rich learning content including examples, pronunciation, and mnemonics."
  , tfStrict = True
  , tfParameters = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "front" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The front of the card: target language word/phrase. Include article for nouns (e.g., 'el libro', 'la mesa')." :: Text)
              ]
          , "back" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The back of the card: native language translation with brief context" :: Text)
              ]
          , "tags" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("Tags for organization: part of speech, topic, level, etc." :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              ]
          , "example" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("An example sentence using the word in the target language" :: Text)
              ]
          , "example_translation" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Translation of the example sentence" :: Text)
              ]
          , "pronunciation" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("IPA or phonetic pronunciation guide" :: Text)
              ]
          , "audio_url" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("URL to audio pronunciation if available" :: Text)
              ]
          , "notes" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Usage notes, conjugation patterns, common collocations, etc." :: Text)
              ]
          , "related_words" .= object
              [ "type" .= ("array" :: Text)
              , "description" .= ("Related words: synonyms, antonyms, word family members" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              ]
          , "mnemonic" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Memory aid or mnemonic device to help remember the word" :: Text)
              ]
          ]
      , "required" .= (["front", "back", "tags"] :: [Text])
      ]
  }

-- ════════════════════════════════════════════════════════════════
-- SHARED SCHEMAS
-- ════════════════════════════════════════════════════════════════

-- | Schema for a vocabulary highlight item
vocabHighlightSchema :: Value
vocabHighlightSchema = object
  [ "type" .= ("object" :: Text)
  , "additionalProperties" .= False
  , "properties" .= object
      [ "word" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("The word or phrase in the target language" :: Text)
          ]
      , "translation" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Translation in the learner's native language" :: Text)
          ]
      , "part_of_speech" .= object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["noun", "verb", "adjective", "adverb", "preposition", "conjunction", "pronoun", "interjection", "phrase", "idiom"] :: [Text])
          ]
      , "gender" .= object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["masculine", "feminine", "neuter"] :: [Text])
          , "description" .= ("Grammatical gender for gendered languages" :: Text)
          ]
      , "plural" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Plural form if irregular" :: Text)
          ]
      ]
  , "required" .= (["word", "translation"] :: [Text])
  ]

-- | Schema for a correction item
correctionSchema :: Value
correctionSchema = object
  [ "type" .= ("object" :: Text)
  , "additionalProperties" .= False
  , "properties" .= object
      [ "original" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("The incorrect word or phrase" :: Text)
          ]
      , "fixed" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("The corrected word or phrase" :: Text)
          ]
      , "explanation" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Brief explanation of why this was wrong and how to remember the correct form" :: Text)
          ]
      , "category" .= object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["grammar", "spelling", "word_choice", "punctuation", "style", "gender", "conjugation", "accent", "other"] :: [Text])
          ]
      , "severity" .= object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["minor", "moderate", "significant"] :: [Text])
          ]
      ]
  , "required" .= (["original", "fixed", "explanation"] :: [Text])
  ]

-- | Schema for a translation note
translationNoteSchema :: Value
translationNoteSchema = object
  [ "type" .= ("object" :: Text)
  , "additionalProperties" .= False
  , "properties" .= object
      [ "phrase" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("The phrase being explained" :: Text)
          ]
      , "explanation" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Why it was translated this way" :: Text)
          ]
      , "cultural_context" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Cultural context if relevant" :: Text)
          ]
      ]
  , "required" .= (["phrase", "explanation"] :: [Text])
  ]
