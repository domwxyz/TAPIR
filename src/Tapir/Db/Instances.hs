{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Tapir.Db.Instances
-- Description : SQLite type class instances for domain types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides the ToField/FromField instances for domain types.
-- These are technically orphan instances, but are isolated here with the
-- warning disabled to keep them organized in one place.

module Tapir.Db.Instances () where

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import Tapir.Types (Role(..), Mode(..), LearnerLevel(..), roleToText, textToRole, modeToText, textToMode)

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
