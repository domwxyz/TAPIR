{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.Client.Anki
-- Description : AnkiConnect API client
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module implements the AnkiConnect API client for pushing flashcards
-- to Anki. AnkiConnect must be running on the user's machine (localhost:8765).
--
-- API Documentation: https://foosoft.net/projects/anki-connect/

module Tapir.Client.Anki
  ( -- * Client
    AnkiClient(..)
  , mkAnkiClient
  , mkAnkiClientWithConfig

    -- * Operations
  , checkConnection
  , addNote
  , addNotes
  , findNotes
  , getDeckNames
  , createDeck

    -- * Types
  , AnkiNote(..)
  , AnkiNoteOptions(..)
  , defaultNoteOptions
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Tapir.Client.Anki.Error (AnkiError(..))
import Tapir.Config.Types (AnkiClientConfig(..))
import Tapir.Config.Defaults (defaultAnkiConfig)

-- ════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════

-- | AnkiConnect API version
ankiConnectVersion :: Int
ankiConnectVersion = 6

-- | Build AnkiConnect URL from host and port
buildAnkiUrl :: AnkiClientConfig -> String
buildAnkiUrl cfg = "http://" <> T.unpack (ankiHost cfg) <> ":" <> show (ankiPort cfg)

-- ════════════════════════════════════════════════════════════════
-- CLIENT
-- ════════════════════════════════════════════════════════════════

-- | AnkiConnect client with HTTP manager
data AnkiClient = AnkiClient
  { acManager :: !Manager
  , acBaseUrl :: !String
  }

-- | Create an AnkiConnect client with default configuration
mkAnkiClient :: IO AnkiClient
mkAnkiClient = mkAnkiClientWithConfig defaultAnkiConfig

-- | Create an AnkiConnect client with custom configuration
mkAnkiClientWithConfig :: AnkiClientConfig -> IO AnkiClient
mkAnkiClientWithConfig cfg = do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (ankiTimeoutSeconds cfg * 1000000)
    }
  pure AnkiClient
    { acManager = manager
    , acBaseUrl = buildAnkiUrl cfg
    }

-- ════════════════════════════════════════════════════════════════
-- API REQUEST/RESPONSE TYPES
-- ════════════════════════════════════════════════════════════════

-- | Generic AnkiConnect request
data AnkiRequest = AnkiRequest
  { arAction  :: !Text
  , arVersion :: !Int
  , arParams  :: !(Maybe Value)
  } deriving (Show, Generic)

instance ToJSON AnkiRequest where
  toJSON AnkiRequest{..} = object $
    [ "action"  .= arAction
    , "version" .= arVersion
    ] ++ maybe [] (\p -> ["params" .= p]) arParams

-- | Generic AnkiConnect response
data AnkiResponse a = AnkiResponse
  { anResult :: !(Maybe a)
  , anError  :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (AnkiResponse a) where
  parseJSON = withObject "AnkiResponse" $ \v -> AnkiResponse
    <$> v .:? "result"
    <*> v .:? "error"

-- ════════════════════════════════════════════════════════════════
-- NOTE TYPES
-- ════════════════════════════════════════════════════════════════

-- | Options for adding a note
data AnkiNoteOptions = AnkiNoteOptions
  { anoAllowDuplicate :: !Bool
  , anoDuplicateScope :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON AnkiNoteOptions where
  toJSON AnkiNoteOptions{..} = object
    [ "allowDuplicate" .= anoAllowDuplicate
    , "duplicateScope" .= anoDuplicateScope
    ]

-- | Default note options (don't allow duplicates)
defaultNoteOptions :: AnkiNoteOptions
defaultNoteOptions = AnkiNoteOptions
  { anoAllowDuplicate = False
  , anoDuplicateScope = "deck"
  }

-- | A note to add to Anki
data AnkiNote = AnkiNote
  { anDeckName  :: !Text    -- ^ Target deck name
  , anModelName :: !Text    -- ^ Note type (e.g., "Basic", "Cloze")
  , anFront     :: !Text    -- ^ Front field content
  , anBack      :: !Text    -- ^ Back field content
  , anTags      :: ![Text]  -- ^ Tags to apply
  , anOptions   :: !AnkiNoteOptions
  } deriving (Show, Eq, Generic)

instance ToJSON AnkiNote where
  toJSON AnkiNote{..} = object
    [ "deckName"  .= anDeckName
    , "modelName" .= anModelName
    , "fields"    .= object
        [ "Front" .= anFront
        , "Back"  .= anBack
        ]
    , "tags"      .= anTags
    , "options"   .= anOptions
    ]

-- ════════════════════════════════════════════════════════════════
-- API OPERATIONS
-- ════════════════════════════════════════════════════════════════

-- | Check if AnkiConnect is running and accessible
checkConnection :: AnkiClient -> IO (Either AnkiError Bool)
checkConnection client = do
  result <- sendAction client "version" Nothing
  case result of
    Right (v :: Int) -> pure $ Right (v >= ankiConnectVersion)
    Left err -> pure $ Left err

-- | Add a single note to Anki, returns the note ID
addNote :: AnkiClient -> AnkiNote -> IO (Either AnkiError Integer)
addNote client note = do
  let params = object ["note" .= note]
  result <- sendAction client "addNote" (Just params)
  case result of
    Right noteId -> pure $ Right noteId
    Left err -> pure $ Left err

-- | Add multiple notes, returns list of note IDs (null for failures)
addNotes :: AnkiClient -> [AnkiNote] -> IO (Either AnkiError [Maybe Integer])
addNotes client notes = do
  let params = object ["notes" .= notes]
  result <- sendAction client "addNotes" (Just params)
  case result of
    Right noteIds -> pure $ Right noteIds
    Left err -> pure $ Left err

-- | Find notes by query (e.g., "deck:Spanish tag:verb")
findNotes :: AnkiClient -> Text -> IO (Either AnkiError [Integer])
findNotes client query = do
  let params = object ["query" .= query]
  result <- sendAction client "findNotes" (Just params)
  case result of
    Right noteIds -> pure $ Right noteIds
    Left err -> pure $ Left err

-- | Get list of all deck names
getDeckNames :: AnkiClient -> IO (Either AnkiError [Text])
getDeckNames client = do
  result <- sendAction client "deckNames" Nothing
  case result of
    Right names -> pure $ Right names
    Left err -> pure $ Left err

-- | Create a new deck
createDeck :: AnkiClient -> Text -> IO (Either AnkiError Integer)
createDeck client deckName = do
  let params = object ["deck" .= deckName]
  result <- sendAction client "createDeck" (Just params)
  case result of
    Right deckId -> pure $ Right deckId
    Left err -> pure $ Left err

-- ════════════════════════════════════════════════════════════════
-- INTERNAL HELPERS
-- ════════════════════════════════════════════════════════════════

-- | Send an action to AnkiConnect and parse the result
sendAction :: FromJSON a => AnkiClient -> Text -> Maybe Value -> IO (Either AnkiError a)
sendAction AnkiClient{..} action params = do
  let req = AnkiRequest
        { arAction  = action
        , arVersion = ankiConnectVersion
        , arParams  = params
        }

  result <- try $ do
    initReq <- parseRequest acBaseUrl
    let httpReq = initReq
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS $ encode req
          }

    resp <- httpLbs httpReq acManager
    let status = statusCode $ responseStatus resp
    let body = responseBody resp

    if status >= 200 && status < 300
      then case eitherDecode body of
        Right (AnkiResponse mResult mError) -> case mError of
          Just err -> pure $ Left $ AnkiConnectionError err
          Nothing -> case mResult of
            Just r  -> pure $ Right r
            Nothing -> pure $ Left $ AnkiConnectionError "No result in response"
        Left err -> pure $ Left $ AnkiConnectionError $ T.pack err
      else pure $ Left $ AnkiConnectionError $ "HTTP " <> T.pack (show status)

  case result of
    Left (_ :: SomeException) ->
      -- Connection failed - likely Anki is not running
      pure $ Left $ AnkiNotRunning
    Right r -> pure r
