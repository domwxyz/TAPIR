{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Db.Schema
-- Description : Database schema initialization and migrations
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module handles database initialization, schema creation,
-- and future migrations for the TAPIR SQLite database.

module Tapir.Db.Schema
  ( -- * Schema Version
    schemaVersion

    -- * Initialization
  , initializeDatabase
  , createTables

    -- * Migrations
  , migrationScripts
  , runMigrations
  , getSchemaVersion
  ) where

import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

import Tapir.Types (TapirError(..))

-- | Current schema version
schemaVersion :: Int
schemaVersion = 1

-- ════════════════════════════════════════════════════════════════
-- SCHEMA DDL
-- ════════════════════════════════════════════════════════════════

-- | Sessions table DDL
sessionsTableSQL :: Query
sessionsTableSQL = Query $ T.unlines
  [ "CREATE TABLE IF NOT EXISTS sessions ("
  , "    id TEXT PRIMARY KEY,"
  , "    language_id TEXT NOT NULL,"
  , "    mode TEXT NOT NULL,"
  , "    learner_level TEXT NOT NULL,"
  , "    created_at TEXT NOT NULL,"
  , "    updated_at TEXT NOT NULL,"
  , "    title TEXT,"
  , "    active INTEGER NOT NULL DEFAULT 1"
  , ");"
  ]

-- | Sessions indexes
sessionsIndexesSQL :: [Query]
sessionsIndexesSQL =
  [ "CREATE INDEX IF NOT EXISTS idx_sessions_language ON sessions(language_id);"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_mode ON sessions(mode);"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_created ON sessions(created_at);"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_active ON sessions(active);"
  ]

-- | Messages table DDL
messagesTableSQL :: Query
messagesTableSQL = Query $ T.unlines
  [ "CREATE TABLE IF NOT EXISTS messages ("
  , "    id INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "    session_id TEXT NOT NULL,"
  , "    role TEXT NOT NULL,"
  , "    content TEXT NOT NULL,"
  , "    mode TEXT NOT NULL,"
  , "    timestamp TEXT NOT NULL,"
  , "    model TEXT,"
  , "    provider TEXT,"
  , "    tokens_used INTEGER,"
  , "    error TEXT,"
  , "    FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE"
  , ");"
  ]

-- | Messages indexes
messagesIndexesSQL :: [Query]
messagesIndexesSQL =
  [ "CREATE INDEX IF NOT EXISTS idx_messages_session ON messages(session_id);"
  , "CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp);"
  , "CREATE INDEX IF NOT EXISTS idx_messages_role ON messages(role);"
  ]

-- | Cards table DDL
cardsTableSQL :: Query
cardsTableSQL = Query $ T.unlines
  [ "CREATE TABLE IF NOT EXISTS cards ("
  , "    id INTEGER PRIMARY KEY AUTOINCREMENT,"
  , "    session_id TEXT NOT NULL,"
  , "    language_id TEXT NOT NULL,"
  , "    front TEXT NOT NULL,"
  , "    back TEXT NOT NULL,"
  , "    tags TEXT NOT NULL,"
  , "    deck TEXT NOT NULL,"
  , "    source_msg_id INTEGER,"
  , "    anki_note_id INTEGER,"
  , "    pushed_at TEXT,"
  , "    created_at TEXT NOT NULL,"
  , "    FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,"
  , "    FOREIGN KEY (source_msg_id) REFERENCES messages(id) ON DELETE SET NULL"
  , ");"
  ]

-- | Cards indexes
cardsIndexesSQL :: [Query]
cardsIndexesSQL =
  [ "CREATE INDEX IF NOT EXISTS idx_cards_session ON cards(session_id);"
  , "CREATE INDEX IF NOT EXISTS idx_cards_language ON cards(language_id);"
  , "CREATE INDEX IF NOT EXISTS idx_cards_pushed ON cards(pushed_at);"
  ]

-- | Metadata table DDL
metadataTableSQL :: Query
metadataTableSQL = Query $ T.unlines
  [ "CREATE TABLE IF NOT EXISTS metadata ("
  , "    key TEXT PRIMARY KEY,"
  , "    value TEXT NOT NULL,"
  , "    updated_at TEXT NOT NULL"
  , ");"
  ]

-- | Initial metadata
initMetadataSQL :: Query
initMetadataSQL = Query $ T.unlines
  [ "INSERT OR IGNORE INTO metadata (key, value, updated_at) VALUES"
  , "    ('schema_version', '1', datetime('now')),"
  , "    ('app_version', '1.0.0', datetime('now')),"
  , "    ('created_at', datetime('now'), datetime('now'));"
  ]

-- | Recent sessions view
recentSessionsViewSQL :: Query
recentSessionsViewSQL = Query $ T.unlines
  [ "CREATE VIEW IF NOT EXISTS v_recent_sessions AS"
  , "SELECT"
  , "    s.id,"
  , "    s.language_id,"
  , "    s.mode,"
  , "    s.learner_level,"
  , "    s.title,"
  , "    s.created_at,"
  , "    s.updated_at,"
  , "    COUNT(m.id) as message_count"
  , "FROM sessions s"
  , "LEFT JOIN messages m ON s.id = m.session_id"
  , "WHERE s.active = 1"
  , "GROUP BY s.id"
  , "ORDER BY s.updated_at DESC;"
  ]

-- | Session statistics view
sessionStatsViewSQL :: Query
sessionStatsViewSQL = Query $ T.unlines
  [ "CREATE VIEW IF NOT EXISTS v_session_stats AS"
  , "SELECT"
  , "    language_id,"
  , "    mode,"
  , "    COUNT(DISTINCT id) as session_count,"
  , "    SUM((SELECT COUNT(*) FROM messages WHERE session_id = sessions.id)) as total_messages,"
  , "    AVG((SELECT COUNT(*) FROM messages WHERE session_id = sessions.id)) as avg_messages_per_session"
  , "FROM sessions"
  , "WHERE active = 1"
  , "GROUP BY language_id, mode;"
  ]

-- ════════════════════════════════════════════════════════════════
-- INITIALIZATION
-- ════════════════════════════════════════════════════════════════

-- | Initialize database with full schema
-- This is the main entry point for database setup.
-- It enables foreign keys, creates all tables, and runs migrations.
initializeDatabase :: Connection -> IO (Either TapirError ())
initializeDatabase conn = do
  result <- try $ do
    -- Enable foreign keys (must be done on each connection)
    execute_ conn "PRAGMA foreign_keys = ON;"

    -- Use WAL mode for better concurrency
    execute_ conn "PRAGMA journal_mode = WAL;"

    -- Create all tables
    createTables conn

    -- Insert initial metadata
    execute_ conn initMetadataSQL

    -- Run any pending migrations
    runMigrations conn

  case result of
    Left (e :: SomeException) ->
      pure $ Left $ DatabaseError $ T.pack $ show e
    Right () -> do
      -- Verify schema version (returns Either, handle gracefully)
      verifySchemaVersion conn

-- | Create all database tables
createTables :: Connection -> IO ()
createTables conn = do
  -- Tables
  execute_ conn sessionsTableSQL
  execute_ conn messagesTableSQL
  execute_ conn cardsTableSQL
  execute_ conn metadataTableSQL

  -- Indexes
  mapM_ (execute_ conn) sessionsIndexesSQL
  mapM_ (execute_ conn) messagesIndexesSQL
  mapM_ (execute_ conn) cardsIndexesSQL

  -- Views
  execute_ conn recentSessionsViewSQL
  execute_ conn sessionStatsViewSQL

-- | Verify the schema version matches expected
-- Returns Left on version mismatch or parse failure
verifySchemaVersion :: Connection -> IO (Either TapirError ())
verifySchemaVersion conn = do
  result <- query_ conn
    "SELECT value FROM metadata WHERE key = 'schema_version'"
    :: IO [Only Text]
  case result of
    [Only ver] ->
      case readMaybe (T.unpack ver) of
        Just currentVersion
          | currentVersion == schemaVersion -> pure $ Right ()
          | otherwise -> pure $ Left $ MigrationError schemaVersion $
              "Schema version mismatch: expected " <> T.pack (show schemaVersion)
              <> ", got " <> T.pack (show currentVersion)
        Nothing -> pure $ Left $ DatabaseError $
          "Invalid schema version in database: " <> ver
    [] -> pure $ Left $ DatabaseError "No schema version found in metadata table"
    _ -> pure $ Left $ DatabaseError "Multiple schema version entries found"
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

-- ════════════════════════════════════════════════════════════════
-- MIGRATIONS
-- ════════════════════════════════════════════════════════════════

-- | Migration scripts for future schema updates
-- Format: (version, description, SQL)
migrationScripts :: [(Int, Text, Query)]
migrationScripts =
  []
  -- Example future migration:
  -- [ ( 2
  --   , "Add embedding column to messages"
  --   , "ALTER TABLE messages ADD COLUMN embedding BLOB;"
  --   )
  -- ]

-- | Get current schema version from database
-- Returns 0 for brand new database, or parsed version number
getSchemaVersion :: Connection -> IO Int
getSchemaVersion conn = do
  result <- query_ conn
    "SELECT value FROM metadata WHERE key = 'schema_version'"
    :: IO [Only Text]
  case result of
    [Only ver] -> pure $ fromMaybe 0 (readMaybe $ T.unpack ver)
    _ -> pure 0  -- No version found, brand new DB
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Run pending migrations
runMigrations :: Connection -> IO ()
runMigrations conn = do
  currentVersion <- getSchemaVersion conn
  let pendingMigrations = filter (\(v, _, _) -> v > currentVersion) migrationScripts

  -- Run each migration in order
  mapM_ (runMigration conn) pendingMigrations

-- | Run a single migration
runMigration :: Connection -> (Int, Text, Query) -> IO ()
runMigration conn (version, description, sql) = do
  -- Execute migration in a transaction
  execute_ conn "BEGIN TRANSACTION;"

  -- Run migration SQL
  execute_ conn sql

  -- Update schema version
  execute conn
    "UPDATE metadata SET value = ?, updated_at = datetime('now') WHERE key = 'schema_version'"
    (Only $ show version)

  -- Commit
  execute_ conn "COMMIT;"

  -- Log migration (in production, use proper logging)
  putStrLn $ "Ran migration " <> show version <> ": " <> T.unpack description
