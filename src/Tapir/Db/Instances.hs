{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Tapir.Db.Instances
-- Description : SQLite type class instances for domain types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides the ToField/FromField/ToRow/FromRow instances for domain types.
-- These are technically orphan instances, but are isolated here with the
-- warning disabled to keep them organized in one place.

module Tapir.Db.Instances
  ( DbTimestamp(..)
  , MaybeDbTimestamp(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow

import Tapir.Types (Role(..), Mode(..), LearnerLevel(..), roleToText, textToRole, modeToText, textToMode, Session(..), Message(..), AnkiCard(..))
import Tapir.Core.Parse (parseUTCTimeMaybe, formatUTCTime, parseTagsOrEmpty)

-- ════════════════════════════════════════════════════════════════
-- ROLE INSTANCES
-- ════════════════════════════════════════════════════════════════

instance ToField Role where
  toField = toField . roleToText

instance FromField Role where
  fromField f = do
    txt <- fromField f
    case textToRole txt of
      Just r  -> pure r
      Nothing -> returnError ConversionFailed f "Invalid role"

-- ════════════════════════════════════════════════════════════════
-- MODE INSTANCES
-- ════════════════════════════════════════════════════════════════

instance ToField Mode where
  toField = toField . modeToText

instance FromField Mode where
  fromField f = do
    txt <- fromField f
    pure $ textToMode txt

-- ════════════════════════════════════════════════════════════════
-- LEARNER LEVEL INSTANCES
-- ════════════════════════════════════════════════════════════════

instance ToField LearnerLevel where
  toField = toField . T.pack . show

instance FromField LearnerLevel where
  fromField f = do
    txt <- fromField @Text f
    case txt of
      "A1" -> pure A1
      "A2" -> pure A2
      "B1" -> pure B1
      "B2" -> pure B2
      "C1" -> pure C1
      "C2" -> pure C2
      _    -> returnError ConversionFailed f "Invalid learner level"

-- ════════════════════════════════════════════════════════════════
-- TIMESTAMP WRAPPER TYPES
-- ════════════════════════════════════════════════════════════════

-- | Newtype wrapper for UTCTime that handles conversion to/from database text
-- Uses ISO 8601 format for storage
newtype DbTimestamp = DbTimestamp { unDbTimestamp :: UTCTime }
  deriving (Eq, Show)

instance FromField DbTimestamp where
  fromField f = do
    txt <- fromField f
    case parseUTCTimeMaybe txt of
      Just t  -> pure (DbTimestamp t)
      Nothing -> returnError ConversionFailed f "Invalid ISO8601 timestamp"

instance ToField DbTimestamp where
  toField (DbTimestamp t) = toField (formatUTCTime t)

-- | Newtype wrapper for optional timestamps
newtype MaybeDbTimestamp = MaybeDbTimestamp { unMaybeDbTimestamp :: Maybe UTCTime }
  deriving (Eq, Show)

instance FromField MaybeDbTimestamp where
  fromField f = do
    mTxt <- fromField f
    pure $ MaybeDbTimestamp $ mTxt >>= parseUTCTimeMaybe

instance ToField MaybeDbTimestamp where
  toField (MaybeDbTimestamp mt) = toField (formatUTCTime <$> mt)

-- ════════════════════════════════════════════════════════════════
-- SESSION INSTANCES
-- ════════════════════════════════════════════════════════════════

instance FromRow Session where
  fromRow = do
    sid <- field
    langId <- field
    mode <- field
    level <- field
    DbTimestamp createdAt <- field
    DbTimestamp updatedAt <- field
    title <- field
    active <- field
    pure Session
      { sessionId = sid
      , sessionLanguageId = langId
      , sessionMode = mode
      , sessionLearnerLevel = level
      , sessionCreatedAt = createdAt
      , sessionUpdatedAt = updatedAt
      , sessionTitle = title
      , sessionActive = active
      }

instance ToRow Session where
  toRow Session{..} = toRow
    ( sessionId
    , sessionLanguageId
    , sessionMode
    , sessionLearnerLevel
    , DbTimestamp sessionCreatedAt
    , DbTimestamp sessionUpdatedAt
    , sessionTitle
    , sessionActive
    )

-- ════════════════════════════════════════════════════════════════
-- MESSAGE INSTANCES
-- ════════════════════════════════════════════════════════════════

instance FromRow Message where
  fromRow = do
    mid <- field
    sid <- field
    role <- field
    content <- field
    mode <- field
    DbTimestamp ts <- field
    model <- field
    provider <- field
    tokens <- field
    err <- field
    pure Message
      { messageId = mid
      , messageSessionId = sid
      , messageRole = role
      , messageContent = content
      , messageMode = mode
      , messageTimestamp = ts
      , messageModel = model
      , messageProvider = provider
      , messageTokensUsed = tokens
      , messageError = err
      }

instance ToRow Message where
  toRow Message{..} = toRow
    ( messageSessionId
    , messageRole
    , messageContent
    , messageMode
    , DbTimestamp messageTimestamp
    , messageModel
    , messageProvider
    , messageTokensUsed
    , messageError
    )

-- ════════════════════════════════════════════════════════════════
-- ANKI CARD INSTANCES
-- ════════════════════════════════════════════════════════════════

instance FromRow AnkiCard where
  fromRow = do
    cid <- field
    sid <- field
    langId <- field
    front <- field
    back <- field
    tagsJson <- field
    deck <- field
    srcMsgId <- field
    noteId <- field
    MaybeDbTimestamp pushedAt <- field
    DbTimestamp createdAt <- field
    pure AnkiCard
      { cardId = cid
      , cardSessionId = sid
      , cardLanguageId = langId
      , cardFront = front
      , cardBack = back
      , cardTags = parseTagsOrEmpty tagsJson
      , cardDeck = deck
      , cardSourceMsgId = srcMsgId
      , cardAnkiNoteId = noteId
      , cardPushedAt = pushedAt
      , cardCreatedAt = createdAt
      }
