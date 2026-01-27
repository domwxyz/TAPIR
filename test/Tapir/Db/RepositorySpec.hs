{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Db.RepositorySpec
-- Description : Tests for database repository operations
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Db.RepositorySpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple hiding (withTransaction)

import Tapir.Types
import Tapir.Db.Error (DbError(..))
import Tapir.Db.Schema
import Tapir.Db.Repository

-- ════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Run test with in-memory database
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = do
  conn <- open ":memory:"
  -- Initialize schema
  result <- initializeDatabase conn
  case result of
    Left err -> do
      close conn
      error $ "Failed to initialize test database: " <> show err
    Right () -> do
      r <- action conn
      close conn
      pure r

-- | Create a test session
makeTestSession :: UTCTime -> Text -> Session
makeTestSession now sid = Session
  { sessionId = sid
  , sessionLanguageId = "spanish"
  , sessionMode = Conversation
  , sessionLearnerLevel = A1
  , sessionCreatedAt = now
  , sessionUpdatedAt = now
  , sessionTitle = Nothing
  , sessionActive = True
  }

-- | Create a test message
makeTestMessage :: UTCTime -> Text -> Role -> Text -> Message
makeTestMessage now sid role content = Message
  { messageId = Nothing  -- Will be assigned by DB
  , messageSessionId = sid
  , messageRole = role
  , messageContent = content
  , messageMode = Conversation
  , messageTimestamp = now
  , messageModel = Nothing
  , messageProvider = Nothing
  , messageTokensUsed = Nothing
  , messageError = Nothing
  }

-- | Create a test card
makeTestCard :: UTCTime -> Text -> AnkiCard
makeTestCard now sid = AnkiCard
  { cardId = Nothing  -- Will be assigned by DB
  , cardSessionId = sid
  , cardLanguageId = "spanish"
  , cardFront = "hola"
  , cardBack = "hello"
  , cardTags = ["greeting", "A1"]
  , cardDeck = "Spanish::Vocab"
  , cardSourceMsgId = Nothing
  , cardAnkiNoteId = Nothing
  , cardPushedAt = Nothing
  , cardCreatedAt = now
  }

-- ════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Schema" $ do
    it "initializes database successfully" $ withTestDb $ \conn -> do
      -- Schema should already be initialized by withTestDb
      version <- getSchemaVersion conn
      version `shouldBe` 1

    it "creates all required tables" $ withTestDb $ \conn -> do
      -- Check that tables exist by querying sqlite_master
      tables <- query_ conn
        "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        :: IO [Only Text]
      let tableNames = map (\(Only n) -> n) tables
      tableNames `shouldContain` ["sessions"]
      tableNames `shouldContain` ["messages"]
      tableNames `shouldContain` ["cards"]
      tableNames `shouldContain` ["metadata"]

    it "creates indexes" $ withTestDb $ \conn -> do
      indexes <- query_ conn
        "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_%'"
        :: IO [Only Text]
      let indexNames = map (\(Only n) -> n) indexes
      length indexNames `shouldSatisfy` (>= 7)

  describe "Session operations" $ do
    it "creates and retrieves a session" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "test-session-1"

      -- Create (now returns Either TapirError ())
      createResult <- createSession conn session
      createResult `shouldBe` Right ()

      -- Retrieve (now returns Maybe Session)
      getResult <- getSession conn "test-session-1"
      case getResult of
        Just s -> do
          sessionId s `shouldBe` "test-session-1"
          sessionLanguageId s `shouldBe` "spanish"
          sessionMode s `shouldBe` Conversation
          sessionLearnerLevel s `shouldBe` A1
          sessionActive s `shouldBe` True
        Nothing -> expectationFailure "Failed to retrieve session"

    it "updates session title" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "test-session-2"

      _ <- createSession conn session
      _ <- setSessionTitle conn "test-session-2" "My Spanish Chat"

      getResult <- getSession conn "test-session-2"
      case getResult of
        Just s -> sessionTitle s `shouldBe` Just "My Spanish Chat"
        Nothing -> expectationFailure "Failed to retrieve updated session"

    it "archives a session" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "test-session-3"

      _ <- createSession conn session
      _ <- archiveSession conn "test-session-3"

      getResult <- getSession conn "test-session-3"
      case getResult of
        Just s -> sessionActive s `shouldBe` False
        Nothing -> expectationFailure "Failed to retrieve archived session"

    it "lists active sessions only" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session1 = makeTestSession now "active-1"
          session2 = makeTestSession now "active-2"
          session3 = makeTestSession now "archived-1"

      _ <- createSession conn session1
      _ <- createSession conn session2
      _ <- createSession conn session3
      _ <- archiveSession conn "archived-1"

      result <- listActiveSessions conn
      case result of
        Right sessions -> do
          length sessions `shouldBe` 2
          map sessionId sessions `shouldContain` ["active-1"]
          map sessionId sessions `shouldContain` ["active-2"]
          map sessionId sessions `shouldNotContain` ["archived-1"]
        Left err -> expectationFailure $ "Failed to list sessions: " <> show err

    it "deletes session with cascade" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "to-delete"
          msg = makeTestMessage now "to-delete" User "Hola"

      _ <- createSession conn session
      _ <- saveMessage conn msg

      -- Verify message exists
      messages <- getSessionMessages conn "to-delete"
      case messages of
        Right ms -> length ms `shouldBe` 1
        _ -> expectationFailure "Failed to create test message"

      -- Delete session
      _ <- deleteSession conn "to-delete"

      -- Verify session is gone (now returns Maybe Session)
      getResult <- getSession conn "to-delete"
      getResult `shouldBe` Nothing

      -- Verify message is also gone (cascade delete)
      messagesAfter <- getSessionMessages conn "to-delete"
      case messagesAfter of
        Right ms -> length ms `shouldBe` 0
        _ -> pure ()

  describe "Message operations" $ do
    it "saves and retrieves messages" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "msg-session-1"
          msg = makeTestMessage now "msg-session-1" User "¿Cómo estás?"

      _ <- createSession conn session
      saveResult <- saveMessage conn msg

      case saveResult of
        Right savedMsg -> do
          messageId savedMsg `shouldSatisfy` (/= Nothing)
          messageContent savedMsg `shouldBe` "¿Cómo estás?"
          messageRole savedMsg `shouldBe` User
        Left err -> expectationFailure $ "Failed to save message: " <> show err

    it "retrieves messages in order" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "msg-session-2"
          msg1 = makeTestMessage now "msg-session-2" User "Hola"
          msg2 = makeTestMessage now "msg-session-2" Assistant "¡Hola! ¿Cómo estás?"
          msg3 = makeTestMessage now "msg-session-2" User "Bien, gracias"

      _ <- createSession conn session
      _ <- saveMessage conn msg1
      _ <- saveMessage conn msg2
      _ <- saveMessage conn msg3

      result <- getSessionMessages conn "msg-session-2"
      case result of
        Right messages -> do
          length messages `shouldBe` 3
          map messageContent messages `shouldBe` ["Hola", "¡Hola! ¿Cómo estás?", "Bien, gracias"]
          map messageRole messages `shouldBe` [User, Assistant, User]
        Left err -> expectationFailure $ "Failed to get messages: " <> show err

    it "stores and retrieves message metadata" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "msg-session-3"
          msg = (makeTestMessage now "msg-session-3" Assistant "Response")
            { messageModel = Just "z-ai/glm-4.7"
            , messageProvider = Just "OpenRouter"
            , messageTokensUsed = Just 42
            }

      _ <- createSession conn session
      _ <- saveMessage conn msg

      result <- getSessionMessages conn "msg-session-3"
      case result of
        Right [m] -> do
          messageModel m `shouldBe` Just "z-ai/glm-4.7"
          messageProvider m `shouldBe` Just "OpenRouter"
          messageTokensUsed m `shouldBe` Just 42
        _ -> expectationFailure "Failed to retrieve message with metadata"

  describe "Card operations" $ do
    it "saves and retrieves cards" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "card-session-1"
          card = makeTestCard now "card-session-1"

      _ <- createSession conn session
      saveResult <- saveCard conn card

      case saveResult of
        Right savedCard -> do
          cardId savedCard `shouldSatisfy` (/= Nothing)
          cardFront savedCard `shouldBe` "hola"
          cardBack savedCard `shouldBe` "hello"
          cardTags savedCard `shouldBe` ["greeting", "A1"]
        Left err -> expectationFailure $ "Failed to save card: " <> show err

    it "marks card as pushed" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "card-session-2"
          card = makeTestCard now "card-session-2"

      _ <- createSession conn session
      saveResult <- saveCard conn card

      case saveResult of
        Right savedCard -> do
          case cardId savedCard of
            Just cid -> do
              _ <- markCardPushed conn cid 12345678

              getResult <- getCard conn cid
              case getResult of
                Just c -> do
                  cardAnkiNoteId c `shouldBe` Just 12345678
                  cardPushedAt c `shouldSatisfy` (/= Nothing)
                Nothing -> expectationFailure "Failed to get pushed card"
            Nothing -> expectationFailure "Card ID was not assigned"
        Left err -> expectationFailure $ "Failed to save card: " <> show err

    it "retrieves unpushed cards" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "card-session-3"
          card1 = makeTestCard now "card-session-3"
          card2 = (makeTestCard now "card-session-3") { cardFront = "adiós", cardBack = "goodbye" }

      _ <- createSession conn session
      Right saved1 <- saveCard conn card1
      _ <- saveCard conn card2

      -- Mark first card as pushed
      case cardId saved1 of
        Just cid -> do
          _ <- markCardPushed conn cid 99999
          pure ()
        Nothing -> pure ()

      -- Get unpushed - should only have second card
      result <- getUnpushedCards conn
      case result of
        Right [card] -> cardFront card `shouldBe` "adiós"
        Right cards -> expectationFailure $ "Expected 1 card but got " <> show (length cards)
        Left err -> expectationFailure $ "Failed to get unpushed cards: " <> show err

    it "stores and retrieves tags as JSON" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "card-session-4"
          card = (makeTestCard now "card-session-4")
            { cardTags = ["noun", "food", "A1", "tapir-generated"] }

      _ <- createSession conn session
      Right saved <- saveCard conn card

      case cardId saved of
        Just cid -> do
          getResult <- getCard conn cid
          case getResult of
            Just c ->
              cardTags c `shouldBe` ["noun", "food", "A1", "tapir-generated"]
            Nothing -> expectationFailure "Failed to get card with tags"
        Nothing -> expectationFailure "Card ID not assigned"

  describe "Transaction handling" $ do
    it "rolls back on error" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let session = makeTestSession now "tx-session-1"

      _ <- createSession conn session

      -- This should fail due to foreign key violation (non-existent session)
      result <- withTransaction conn $ do
        execute conn
          "INSERT INTO messages (session_id, role, content, mode, timestamp) VALUES (?, ?, ?, ?, ?)"
          ("non-existent-session" :: Text, "user" :: Text, "test" :: Text, "conversation" :: Text, "2026-01-22T00:00:00Z" :: Text)

      case result of
        Left _ -> pure ()  -- Expected: foreign key violation
        Right _ -> expectationFailure "Transaction should have failed"

  describe "Foreign key constraints" $ do
    it "prevents inserting message with invalid session_id" $ withTestDb $ \conn -> do
      now <- getCurrentTime
      let msg = makeTestMessage now "non-existent-session" User "Test"

      result <- saveMessage conn msg
      case result of
        Left (DbQueryError _) -> pure ()  -- Expected
        Right _ -> expectationFailure "Should have failed due to foreign key constraint"
        Left other -> expectationFailure $ "Unexpected error: " <> show other
