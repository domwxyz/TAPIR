{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.UI.Event.Card
-- Description : Anki card event handlers
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Event.Card
  ( handleCardPush
  , handleCardDiscard
  ) where

import Brick
import Brick.BChan (writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Lens.Micro.Mtl ((.=))

import Tapir.Types (AnkiCard(..))
import Tapir.UI.Types
import Tapir.Client.Anki (addNote, AnkiNote(..), defaultNoteOptions)
import Tapir.Db.Repository (markCardPushed)
import Tapir.Core.Constants (ankiDefaultNoteType)

-- | Handle pushing card to Anki from CardPreviewModal
handleCardPush :: AnkiCard -> EventM Name AppState ()
handleCardPush card = do
  st <- get
  let ankiClient = _asAnkiClient st
      conn = _asDbConnection st
      chan = _asEventChannel st

  -- Convert AnkiCard to AnkiNote for the API
  let note = AnkiNote
        { anDeckName  = cardDeck card
        , anModelName = ankiDefaultNoteType
        , anFront     = cardFront card
        , anBack      = cardBack card
        , anTags      = cardTags card
        , anOptions   = defaultNoteOptions
        }

  -- Push to Anki asynchronously
  liftIO $ void $ forkIO $ do
    result <- addNote ankiClient note
    case result of
      Right noteId -> do
        -- Mark card as pushed in database
        case cardId card of
          Just cid -> void $ markCardPushed conn cid noteId
          Nothing  -> pure ()
        writeBChan chan (EvCardPushResult (Right noteId))
      Left err ->
        writeBChan chan (EvCardPushResult (Left err))

  -- Close modal (result will come via EvCardPushResult)
  asModal .= NoModal

-- | Handle discarding a pending card
handleCardDiscard :: EventM Name AppState ()
handleCardDiscard = do
  asPendingCard .= Nothing
  asModal .= NoModal
