{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Db.Error
-- Description : Database error types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Db.Error
  ( DbError(..)
  , displayDbError
  , shortDbError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Errors specific to database operations
data DbError
  = DbQueryError !Text
  | DbSessionNotFound !Text          -- ^ Session ID
  | DbMigrationError !Int !Text      -- ^ Version and reason
  | DbTimestampParseError !Text !Text -- ^ Field name and value
  deriving (Eq, Show)

-- | User-friendly error messages
displayDbError :: DbError -> Text
displayDbError = \case
  DbQueryError msg ->
    "Database error: " <> msg
  DbSessionNotFound sid ->
    "Session not found: " <> sid
  DbMigrationError ver reason ->
    "Database migration to version " <> T.pack (show ver) <> " failed: " <> reason
  DbTimestampParseError field val ->
    "Invalid timestamp in " <> field <> ": " <> val

-- | Short error messages for status bar
shortDbError :: DbError -> Text
shortDbError = \case
  DbQueryError _          -> "Database error"
  DbSessionNotFound _     -> "Session not found"
  DbMigrationError _ _    -> "Migration error"
  DbTimestampParseError _ _ -> "Invalid timestamp"
