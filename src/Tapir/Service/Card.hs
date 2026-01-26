{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Service.Card
-- Description : Card extraction and creation logic
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module contains the business logic for extracting flashcards from
-- LLM responses and converting structured card responses to AnkiCard format.

module Tapir.Service.Card
  ( -- * Card Extraction
    extractCardFromResponse
  , cardResponseToAnkiCard

    -- * Parsing Helpers
  , parseJsonCard
  , parseLabeledCard
  , parseSimpleCard
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)

import Tapir.Types (AnkiCard(..))
import Tapir.Types.Response (CardResponse(..))
import Tapir.Core.Constants (ankiDefaultDeckSuffix)
import Tapir.Core.Parse (stripMarkdownFences)

-- | Extract a flashcard from an LLM response
-- Tries multiple parsing strategies in order:
-- 1. JSON format: {"front": "...", "back": "...", "tags": [...]}
-- 2. Labeled format: "Front: ...\nBack: ..."
-- 3. Simple format: first line = front, rest = back
extractCardFromResponse
  :: Text      -- ^ Language ID
  -> Text      -- ^ Session ID
  -> Maybe Int -- ^ Source message ID
  -> Text      -- ^ Raw response text
  -> UTCTime   -- ^ Current timestamp
  -> Maybe AnkiCard
extractCardFromResponse langId sessionId' sourceMsgId response now =
  parseJsonCard langId sessionId' sourceMsgId response now
    <|> parseLabeledCard langId sessionId' sourceMsgId response now
    <|> parseSimpleCard langId sessionId' sourceMsgId response now

-- | Try to parse JSON format card
parseJsonCard
  :: Text -> Text -> Maybe Int -> Text -> UTCTime -> Maybe AnkiCard
parseJsonCard langId sessionId' sourceMsgId response now = do
  let cleanResponse = stripMarkdownFences response
  case decode (BL.fromStrict $ TE.encodeUtf8 cleanResponse) of
    Just (Object obj) -> do
      frontVal <- KM.lookup (K.fromString "front") obj
      backVal <- KM.lookup (K.fromString "back") obj
      front <- case frontVal of
        String t -> Just t
        _ -> Nothing
      back <- case backVal of
        String t -> Just t
        _ -> Nothing
      let tags = case KM.lookup (K.fromString "tags") obj of
            Just (Array arr) ->
              mapMaybe (\v -> case v of
                String t -> Just t
                _ -> Nothing) (toList arr)
            _ -> [langId]
      pure $ mkCard langId sessionId' sourceMsgId front back tags now
    _ -> Nothing

-- | Try "Front: ...\nBack: ..." format (case-insensitive)
parseLabeledCard
  :: Text -> Text -> Maybe Int -> Text -> UTCTime -> Maybe AnkiCard
parseLabeledCard langId sessionId' sourceMsgId response now =
  let lowerResponse = T.toLower response
  in if "front:" `T.isInfixOf` lowerResponse && "back:" `T.isInfixOf` lowerResponse
     then
       -- Split on "back:" (trying both cases)
       let (beforeBack, afterBack) =
             if "Back:" `T.isInfixOf` response
             then T.breakOn "Back:" response
             else T.breakOn "back:" response
           -- Extract front: everything after "front:" up to "back:"
           frontPart = snd $ T.breakOn ":" $ snd $ T.breakOn "ront:" beforeBack
           frontText = T.strip $ T.drop 1 frontPart  -- Drop the ':'
           -- Extract back: everything after "back:"
           backText = T.strip $ T.drop 5 afterBack  -- Drop "Back:" or "back:"
       in if T.null frontText || T.null backText
          then Nothing
          else Just $ mkCard langId sessionId' sourceMsgId frontText backText [langId] now
     else Nothing

-- | Simple format: first non-empty line = front, rest = back
parseSimpleCard
  :: Text -> Text -> Maybe Int -> Text -> UTCTime -> Maybe AnkiCard
parseSimpleCard langId sessionId' sourceMsgId response now =
  let nonEmptyLines = filter (not . T.null . T.strip) (T.lines response)
  in case nonEmptyLines of
    (front:rest) | not (null rest) ->
      Just $ mkCard langId sessionId' sourceMsgId
        (T.strip front)
        (T.strip $ T.intercalate "\n" rest)
        [langId]
        now
    _ -> Nothing

-- | Create an AnkiCard from extracted data
mkCard :: Text -> Text -> Maybe Int -> Text -> Text -> [Text] -> UTCTime -> AnkiCard
mkCard langId sessionId' sourceMsgId front back tags now = AnkiCard
  { cardId          = Nothing
  , cardSessionId   = sessionId'
  , cardLanguageId  = langId
  , cardFront       = front
  , cardBack        = back
  , cardTags        = tags
  , cardDeck        = langId <> ankiDefaultDeckSuffix
  , cardSourceMsgId = sourceMsgId
  , cardAnkiNoteId  = Nothing
  , cardPushedAt    = Nothing
  , cardCreatedAt   = now
  }

-- | Convert a structured CardResponse to AnkiCard
cardResponseToAnkiCard :: CardResponse -> Text -> Text -> UTCTime -> AnkiCard
cardResponseToAnkiCard cr sessionId' langId now = AnkiCard
  { cardId          = Nothing
  , cardSessionId   = sessionId'
  , cardLanguageId  = langId
  , cardFront       = cardRespFront cr
  , cardBack        = formatCardBack (cardRespBack cr) (cardRespExample cr) (cardRespNotes cr)
  , cardTags        = cardRespTags cr
  , cardDeck        = langId <> ankiDefaultDeckSuffix
  , cardSourceMsgId = Nothing
  , cardAnkiNoteId  = Nothing
  , cardPushedAt    = Nothing
  , cardCreatedAt   = now
  }
  where
    formatCardBack back mExample mNotes =
      T.intercalate "\n\n" $ filter (not . T.null)
        [ back
        , maybe "" (\ex -> "Example: " <> ex) mExample
        , maybe "" id mNotes
        ]
