{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Core.Parse
-- Description : Consistent parsing utilities with explicit failure handling
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides standardized parsing functions with clear semantics:
--
-- * @parseXxx@ functions return @Either TapirError a@ for explicit error handling
-- * @parseXxxMaybe@ functions return @Maybe a@ for expected absence
-- * @parseXxxOrDefault@ functions return defaults with documented behavior
--
-- All silent defaults are explicitly named and documented.

module Tapir.Core.Parse
  ( -- * Time Parsing
    parseUTCTime
  , parseUTCTimeMaybe
  , formatUTCTime

    -- * JSON Parsing
  , parseJsonText
  , parseJsonTextMaybe

    -- * Tag Parsing (with explicit default)
  , parseTagsOrEmpty

    -- * Markdown/Text Processing
  , stripMarkdownFences

    -- * Validation
  , validateNonEmpty
  , validateInRange
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)

import Tapir.Types (TapirError(..))

-- ════════════════════════════════════════════════════════════════
-- TIME PARSING
-- ════════════════════════════════════════════════════════════════

-- | Parse ISO8601 timestamp, returning explicit error on failure.
parseUTCTime :: Text -> Either TapirError UTCTime
parseUTCTime txt = case iso8601ParseM (T.unpack txt) of
  Just t  -> Right t
  Nothing -> Left $ InternalError $ "Invalid ISO8601 timestamp: " <> txt

-- | Parse ISO8601 timestamp, returning Nothing on failure.
-- Use when "no timestamp" is an expected/valid case.
parseUTCTimeMaybe :: Text -> Maybe UTCTime
parseUTCTimeMaybe = iso8601ParseM . T.unpack

-- | Format UTCTime as ISO8601 text for storage.
formatUTCTime :: UTCTime -> Text
formatUTCTime = T.pack . iso8601Show

-- ════════════════════════════════════════════════════════════════
-- JSON PARSING
-- ════════════════════════════════════════════════════════════════

-- | Parse JSON from Text, returning explicit error on failure.
parseJsonText :: FromJSON a => Text -> Either TapirError a
parseJsonText txt = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 txt) of
  Right a  -> Right a
  Left err -> Left $ InternalError $ "JSON parse error: " <> T.pack err

-- | Parse JSON from Text, returning Nothing on failure.
parseJsonTextMaybe :: FromJSON a => Text -> Maybe a
parseJsonTextMaybe txt = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 txt) of
  Right a -> Just a
  Left _  -> Nothing

-- ════════════════════════════════════════════════════════════════
-- TAG PARSING
-- ════════════════════════════════════════════════════════════════

-- | Parse tags from JSON array text, returning empty list on failure.
--
-- This function explicitly returns an empty list on parse failure because:
-- 1. Tags are non-critical metadata - missing tags don't break functionality
-- 2. The original JSON is preserved in the database, so no data is lost
-- 3. Failed parsing usually indicates empty or legacy data
--
-- If you need to know about parse failures, use 'parseJsonText' instead.
parseTagsOrEmpty :: Text -> [Text]
parseTagsOrEmpty txt
  | T.null txt = []
  | otherwise = case parseJsonTextMaybe txt of
      Just tags -> tags
      Nothing   -> []

-- ════════════════════════════════════════════════════════════════
-- TEXT PROCESSING
-- ════════════════════════════════════════════════════════════════

-- | Strip markdown code fences (```json ... ``` or ``` ... ```)
stripMarkdownFences :: Text -> Text
stripMarkdownFences text =
  let lines' = T.lines text
      firstNonBlank = dropWhile (T.null . T.strip) lines'
  in case firstNonBlank of
    (l:rest)
      | "```" `T.isPrefixOf` l ->
          -- Find closing fence
          case break ("```" `T.isPrefixOf`) rest of
            (content, _:_) -> T.unlines content
            (content, []) -> T.unlines content
    _ -> text

-- ════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════

-- | Validate that text is non-empty.
validateNonEmpty :: Text -> Text -> Either TapirError Text
validateNonEmpty fieldName txt
  | T.null (T.strip txt) = Left $ InternalError $ fieldName <> " cannot be empty"
  | otherwise = Right txt

-- | Validate that a value is within a range.
validateInRange :: (Ord a, Show a) => Text -> a -> a -> a -> Either TapirError a
validateInRange fieldName minVal maxVal val
  | val < minVal = Left $ InternalError $ fieldName <> " must be >= " <> T.pack (show minVal)
  | val > maxVal = Left $ InternalError $ fieldName <> " must be <= " <> T.pack (show maxVal)
  | otherwise = Right val
