{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Tapir.Db.Repository
-- Description : Database operations for TAPIR entities
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module implements the repository pattern for database operations,
-- providing CRUD operations for sessions, messages, and cards.
-- All functions use parameterized queries to prevent SQL injection.
--
-- ════════════════════════════════════════════════════════════════
-- ERROR HANDLING CONVENTIONS
-- ════════════════════════════════════════════════════════════════
--
-- This module follows these conventions:
--
-- 1. LOOKUPS (get by ID): Return 'Maybe a'
--    - 'Nothing' means "not found" (expected case)
--    - Database errors are thrown as exceptions (unexpected)
--    Example: getSession :: Connection -> Text -> IO (Maybe Session)
--
-- 2. MUTATIONS (create, update, delete): Return 'Either DbError ()'
--    - 'Left' contains the error
--    - 'Right ()' means success
--    Example: createSession :: Connection -> Session -> IO (Either DbError ())
--
-- 3. LISTINGS: Return 'Either DbError [a]'
--    - Empty list is valid, not an error
--    - 'Left' only for database failures
--    Example: listSessions :: Connection -> IO (Either DbError [Session])
--
-- The rationale: "not found" is a normal program state for lookups,
-- not an exceptional condition. Database failures are exceptional.

module Tapir.Db.Repository
  ( -- * Session Operations
    createSession
  , getSession
  , updateSession
  , updateSessionTimestamp
  , setSessionTitle
  , archiveSession
  , deleteSession
  , listSessions
  , listActiveSessions
  , getRecentSessions

    -- * Message Operations
  , saveMessage
  , getMessage
  , getSessionMessages
  , getMessageHistory
  , deleteMessage

    -- * Card Operations
  , saveCard
  , getCard
  , getSessionCards
  , getUnpushedCards
  , markCardPushed
  , deleteCard

    -- * Utility
  , withTransaction
  ) where

import Control.Exception (try)
import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Only(..), query, query_, execute, execute_, lastInsertRowId, SQLError, (:.)(..))
import Database.SQLite.Simple.FromRow (FromRow(..), field)

import Tapir.Types (Session(..), Message(..), AnkiCard(..), Role, Mode, LearnerLevel)
import Tapir.Db.Error (DbError(..))
import Tapir.Core.Parse (parseUTCTimeMaybe, formatUTCTime, parseTagsOrEmpty)
import Tapir.Db.Instances (DbTimestamp(..))  -- Import for SQLite instances and timestamp wrapper
import Data.Maybe (listToMaybe)

-- ════════════════════════════════════════════════════════════════
-- HELPER TYPES
-- ════════════════════════════════════════════════════════════════

-- | Helper type for queries that return session with message count
data SessionWithCount = SessionWithCount
  { swcSession :: !Session
  , swcMessageCount :: !Int
  } deriving (Eq, Show)

instance FromRow SessionWithCount where
  fromRow = do
    sid <- field
    langId <- field
    mode <- field
    level <- field
    title <- field
    DbTimestamp createdAt <- field
    DbTimestamp updatedAt <- field
    msgCount <- field
    pure $ SessionWithCount
      (Session sid langId mode level createdAt updatedAt title True)
      msgCount

-- ════════════════════════════════════════════════════════════════
-- TIME AND TAG HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Store UTCTime as ISO 8601 text (alias to Parse module)
utcToText :: UTCTime -> Text
utcToText = formatUTCTime

-- | Parse UTC time from text (alias to Parse module)
textToUtc :: Text -> Maybe UTCTime
textToUtc = parseUTCTimeMaybe

-- | Store [Text] as JSON array
tagsToText :: [Text] -> Text
tagsToText tags = TE.decodeUtf8 $ BL.toStrict $ encode tags

-- | Parse tags from JSON (alias to Parse module).
-- Returns empty list on parse failure - see Tapir.Core.Parse for design rationale.
textToTags :: Text -> [Text]
textToTags = parseTagsOrEmpty

-- ════════════════════════════════════════════════════════════════
-- SESSION OPERATIONS
-- ════════════════════════════════════════════════════════════════

-- | Create a new session. Returns Right () on success.
-- The caller already has the Session object; it doesn't need to be returned.
createSession :: Connection -> Session -> IO (Either DbError ())
createSession conn session = do
  result <- try $ execute conn
    "INSERT INTO sessions (id, language_id, mode, learner_level, created_at, updated_at, title, active) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( sessionId session
    , sessionLanguageId session
    , sessionMode session
    , sessionLearnerLevel session
    , utcToText $ sessionCreatedAt session
    , utcToText $ sessionUpdatedAt session
    , sessionTitle session
    , sessionActive session
    )
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Get a session by ID. Returns Nothing if not found.
-- Throws on database errors (unexpected).
getSession :: Connection -> Text -> IO (Maybe Session)
getSession conn sid = do
  rows <- query conn
    "SELECT id, language_id, mode, learner_level, created_at, updated_at, title, active \
    \FROM sessions WHERE id = ?"
    (Only sid)
  pure $ listToMaybe rows

-- | Update a session
updateSession :: Connection -> Session -> IO (Either DbError ())
updateSession conn session = do
  result <- try $ execute conn
    "UPDATE sessions SET language_id = ?, mode = ?, learner_level = ?, \
    \updated_at = ?, title = ?, active = ? WHERE id = ?"
    ( sessionLanguageId session
    , sessionMode session
    , sessionLearnerLevel session
    , utcToText $ sessionUpdatedAt session
    , sessionTitle session
    , sessionActive session
    , sessionId session
    )
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Update session timestamp only
updateSessionTimestamp :: Connection -> Text -> IO (Either DbError ())
updateSessionTimestamp conn sid = do
  now <- getCurrentTime
  result <- try $ execute conn
    "UPDATE sessions SET updated_at = ? WHERE id = ?"
    (utcToText now, sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Set session title
setSessionTitle :: Connection -> Text -> Text -> IO (Either DbError ())
setSessionTitle conn sid title = do
  now <- getCurrentTime
  result <- try $ execute conn
    "UPDATE sessions SET title = ?, updated_at = ? WHERE id = ?"
    (title, utcToText now, sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Archive a session (set active = 0)
archiveSession :: Connection -> Text -> IO (Either DbError ())
archiveSession conn sid = do
  result <- try $ execute conn
    "UPDATE sessions SET active = 0 WHERE id = ?"
    (Only sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Delete a session (cascades to messages and cards)
deleteSession :: Connection -> Text -> IO (Either DbError ())
deleteSession conn sid = do
  result <- try $ execute conn
    "DELETE FROM sessions WHERE id = ?"
    (Only sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | List all sessions (including archived)
listSessions :: Connection -> IO (Either DbError [Session])
listSessions conn = do
  result <- try $ query_ conn
    "SELECT id, language_id, mode, learner_level, created_at, updated_at, title, active \
    \FROM sessions ORDER BY updated_at DESC"
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right rows

-- | List active sessions only
listActiveSessions :: Connection -> IO (Either DbError [Session])
listActiveSessions conn = do
  result <- try $ query_ conn
    "SELECT id, language_id, mode, learner_level, created_at, updated_at, title, active \
    \FROM sessions WHERE active = 1 ORDER BY updated_at DESC"
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right rows

-- | Get recent sessions with message count (using view)
getRecentSessions :: Connection -> Int -> IO (Either DbError [(Session, Int)])
getRecentSessions conn limit = do
  result <- try $ query conn
    "SELECT id, language_id, mode, learner_level, title, created_at, updated_at, message_count \
    \FROM v_recent_sessions LIMIT ?"
    (Only limit)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right $ map (\swc -> (swcSession swc, swcMessageCount swc)) rows


-- ════════════════════════════════════════════════════════════════
-- MESSAGE OPERATIONS
-- ════════════════════════════════════════════════════════════════

-- | Save a new message (returns message with generated ID)
saveMessage :: Connection -> Message -> IO (Either DbError Message)
saveMessage conn msg = do
  result <- try $ do
    execute conn
      "INSERT INTO messages (session_id, role, content, mode, timestamp, model, provider, tokens_used, error) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( messageSessionId msg
      , messageRole msg
      , messageContent msg
      , messageMode msg
      , utcToText $ messageTimestamp msg
      , messageModel msg
      , messageProvider msg
      , messageTokensUsed msg
      , messageError msg
      )
    lastId <- lastInsertRowId conn
    pure $ msg { messageId = Just (fromIntegral lastId) }
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right msgWithId -> pure $ Right msgWithId

-- | Get a message by ID. Returns Nothing if not found.
-- Throws on database errors (unexpected).
getMessage :: Connection -> Int -> IO (Maybe Message)
getMessage conn mid = do
  rows <- query conn
    "SELECT id, session_id, role, content, mode, timestamp, model, provider, tokens_used, error \
    \FROM messages WHERE id = ?"
    (Only mid)
  pure $ listToMaybe rows

-- | Get all messages for a session (ordered by timestamp)
getSessionMessages :: Connection -> Text -> IO (Either DbError [Message])
getSessionMessages conn sid = do
  result <- try $ query conn
    "SELECT id, session_id, role, content, mode, timestamp, model, provider, tokens_used, error \
    \FROM messages WHERE session_id = ? ORDER BY timestamp ASC"
    (Only sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right rows

-- | Get message history (paginated, most recent first)
getMessageHistory :: Connection -> Text -> Int -> Int -> IO (Either DbError [Message])
getMessageHistory conn sid limit offset = do
  result <- try $ query conn
    "SELECT id, session_id, role, content, mode, timestamp, model, provider, tokens_used, error \
    \FROM messages WHERE session_id = ? ORDER BY timestamp DESC LIMIT ? OFFSET ?"
    (sid, limit, offset)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right $ reverse rows

-- | Delete a message
deleteMessage :: Connection -> Int -> IO (Either DbError ())
deleteMessage conn mid = do
  result <- try $ execute conn
    "DELETE FROM messages WHERE id = ?"
    (Only mid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()


-- ════════════════════════════════════════════════════════════════
-- CARD OPERATIONS
-- ════════════════════════════════════════════════════════════════

-- | Save a new card (returns card with generated ID)
saveCard :: Connection -> AnkiCard -> IO (Either DbError AnkiCard)
saveCard conn card = do
  result <- try $ do
    execute conn
      "INSERT INTO cards (session_id, language_id, front, back, tags, deck, source_msg_id, anki_note_id, pushed_at, created_at) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ( cardSessionId card
      , cardLanguageId card
      , cardFront card
      , cardBack card
      , tagsToText $ cardTags card
      , cardDeck card
      , cardSourceMsgId card
      , cardAnkiNoteId card
      , utcToText <$> cardPushedAt card
      , utcToText $ cardCreatedAt card
      )
    lastId <- lastInsertRowId conn
    pure $ card { cardId = Just (fromIntegral lastId) }
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right cardWithId -> pure $ Right cardWithId

-- | Get a card by ID. Returns Nothing if not found.
-- Throws on database errors (unexpected).
getCard :: Connection -> Int -> IO (Maybe AnkiCard)
getCard conn cid = do
  rows <- query conn
    "SELECT id, session_id, language_id, front, back, tags, deck, source_msg_id, anki_note_id, pushed_at, created_at \
    \FROM cards WHERE id = ?"
    (Only cid)
  pure $ listToMaybe rows

-- | Get all cards for a session
getSessionCards :: Connection -> Text -> IO (Either DbError [AnkiCard])
getSessionCards conn sid = do
  result <- try $ query conn
    "SELECT id, session_id, language_id, front, back, tags, deck, source_msg_id, anki_note_id, pushed_at, created_at \
    \FROM cards WHERE session_id = ? ORDER BY created_at DESC"
    (Only sid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right rows

-- | Get unpushed cards (not yet sent to Anki)
getUnpushedCards :: Connection -> IO (Either DbError [AnkiCard])
getUnpushedCards conn = do
  result <- try $ query_ conn
    "SELECT id, session_id, language_id, front, back, tags, deck, source_msg_id, anki_note_id, pushed_at, created_at \
    \FROM cards WHERE pushed_at IS NULL ORDER BY created_at ASC"
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right rows -> pure $ Right rows

-- | Mark a card as pushed to Anki
markCardPushed :: Connection -> Int -> Integer -> IO (Either DbError ())
markCardPushed conn cid ankiNoteId = do
  now <- getCurrentTime
  result <- try $ execute conn
    "UPDATE cards SET anki_note_id = ?, pushed_at = ? WHERE id = ?"
    (ankiNoteId, utcToText now, cid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Delete a card
deleteCard :: Connection -> Int -> IO (Either DbError ())
deleteCard conn cid = do
  result <- try $ execute conn
    "DELETE FROM cards WHERE id = ?"
    (Only cid)
  case result of
    Left (e :: SQLError) -> pure $ Left $ DbQueryError $ T.pack $ show e
    Right () -> pure $ Right ()


-- ════════════════════════════════════════════════════════════════
-- UTILITY
-- ════════════════════════════════════════════════════════════════

-- | Run operations in a transaction
-- On failure, attempts rollback and reports both errors if rollback also fails
withTransaction :: Connection -> IO a -> IO (Either DbError a)
withTransaction conn action = do
  result <- try $ do
    execute_ conn "BEGIN TRANSACTION;"
    r <- action
    execute_ conn "COMMIT;"
    pure r
  case result of
    Left (e :: SQLError) -> do
      rollbackResult <- try @SQLError $ execute_ conn "ROLLBACK;"
      let baseError = T.pack $ show e
      let fullError = case rollbackResult of
            Right () -> baseError
            Left rollbackErr -> baseError <> " (rollback also failed: "
                              <> T.pack (show rollbackErr) <> ")"
      pure $ Left $ DbQueryError fullError
    Right a -> pure $ Right a
