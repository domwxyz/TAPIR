{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tapir.UI.App
-- Description : Main brick application wiring
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module defines the brick application, including:
-- - Main app definition
-- - Event handling
-- - Drawing
-- - Initial state construction

module Tapir.UI.App
  ( -- * Application
    tapirApp
  , runTapir

    -- * State Initialization
  , mkInitialState

    -- * Event Handling
  , handleEvent
  , handleMainEvent
  , handleModalEvent
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void, when, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection)
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Input.Events
import Lens.Micro.Mtl ((.=), (%=), use)

import Tapir.Types
import Tapir.Config.Types (AppConfig(..))
import Tapir.Client.LLM
import Tapir.Client.Anki (mkAnkiClient, checkConnection, addNote, AnkiNote(..), defaultNoteOptions)
import Tapir.Db.Repository (createSession, saveMessage, saveCard, getRecentSessions, getSessionMessages, updateSessionTimestamp, markCardPushed)
import Tapir.UI.Types
import Tapir.UI.Attrs (tapirAttrMap)
import Tapir.UI.Chat (renderChat)
import Tapir.UI.Input (renderInput, getEditorContent, mkInputEditor)
import Tapir.UI.StatusBar (renderStatusBar)
import Tapir.UI.Modals (renderModal)

-- ════════════════════════════════════════════════════════════════
-- APPLICATION DEFINITION
-- ════════════════════════════════════════════════════════════════

-- | The main brick application
tapirApp :: App AppState TapirEvent Name
tapirApp = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const tapirAttrMap
  }

-- | Run the TAPIR application
runTapir :: AppConfig -> LanguageModule -> LLMClient -> Connection -> IO ()
runTapir config langMod client conn = do
  -- Create event channel
  chan <- newBChan 10

  -- Create initial state
  initialState <- mkInitialState config langMod client conn chan

  -- Build vty with platform-specific config
  let vtyConfig = defaultConfig
  let buildVty = mkVty vtyConfig

  -- Try to run the entire TUI - may fail on non-native Windows consoles
  result <- try $ do
    initialVty <- buildVty
    customMain initialVty buildVty (Just chan) tapirApp initialState

  case result of
    Left (err :: SomeException) -> do
      putStrLn ""
      putStrLn "ERROR: Terminal UI failed."
      putStrLn $ "  " ++ show err
      putStrLn ""
      putStrLn "This usually happens when running in Git Bash or mintty."
      putStrLn "Please try one of these alternatives:"
      putStrLn "  1. Run in Windows Terminal, PowerShell, or Command Prompt"
      putStrLn "  2. Use: winpty cabal run tapir"
      putStrLn ""
    Right _ -> pure ()

-- ════════════════════════════════════════════════════════════════
-- STATE INITIALIZATION
-- ════════════════════════════════════════════════════════════════

-- | Create the initial application state
mkInitialState
  :: AppConfig
  -> LanguageModule
  -> LLMClient
  -> Connection
  -> BChan TapirEvent
  -> IO AppState
mkInitialState config langMod client conn chan = do
  -- Create a new session
  sid <- UUID.toText <$> nextRandom
  now <- getCurrentTime
  let session = Session
        { sessionId          = sid
        , sessionLanguageId  = languageId (languageInfo langMod)
        , sessionMode        = Conversation
        , sessionLearnerLevel = learnerLevel langMod
        , sessionCreatedAt   = now
        , sessionUpdatedAt   = now
        , sessionTitle       = Nothing
        , sessionActive      = True
        }

  -- Persist session to database
  _ <- createSession conn session

  -- Create Anki client
  ankiClient <- mkAnkiClient

  -- Check Anki connection status asynchronously
  _ <- forkIO $ do
    result <- checkConnection ankiClient
    let connected = case result of
          Right True -> True
          _          -> False
    writeBChan chan (EvAnkiStatusUpdate connected)

  pure AppState
    { _asSession        = session
    , _asMessages       = []
    , _asInputEditor    = mkInputEditor
    , _asCurrentMode    = Conversation
    , _asFocus          = FocusInput
    , _asModal          = NoModal
    , _asRequestState   = Idle
    , _asStreamingText  = ""
    , _asPendingCard    = Nothing
    , _asAnkiConnected  = False  -- Will be updated by status poll
    , _asLastError      = Nothing
    , _asConfig         = config
    , _asLangModule     = langMod
    , _asLlmClient      = client
    , _asAnkiClient     = ankiClient
    , _asDbConnection   = conn
    , _asEventChannel   = chan
    }

-- ════════════════════════════════════════════════════════════════
-- DRAWING
-- ════════════════════════════════════════════════════════════════

-- | Draw the entire UI
drawUI :: AppState -> [Widget Name]
drawUI st =
  let mainUI = vBox
        [ renderChat st
        , renderInput st
        , renderStatusBar st
        ]
      modalOverlay = renderModal st
  in [modalOverlay, mainUI]

-- ════════════════════════════════════════════════════════════════
-- EVENT HANDLING
-- ════════════════════════════════════════════════════════════════

-- | Main event handler
handleEvent :: BrickEvent Name TapirEvent -> EventM Name AppState ()
handleEvent = \case
  -- Custom events from BChan
  AppEvent ev -> handleCustomEvent ev

  -- VTY events (keyboard/mouse)
  VtyEvent vtyEv -> do
    st <- get
    case _asModal st of
      NoModal -> handleMainEvent vtyEv
      _       -> handleModalEvent vtyEv

  -- Mouse events (not used currently)
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()

-- | Handle custom events from BChan
handleCustomEvent :: TapirEvent -> EventM Name AppState ()
handleCustomEvent = \case
  EvStreamChunk chunk -> do
    asStreamingText %= (<> chunk)
    asRequestState .= Streaming
    -- Scroll to bottom
    let vp = viewportScroll NameHistoryViewport
    vScrollToEnd vp

  EvStreamComplete fullResponse -> do
    st <- get
    let mode = _asCurrentMode st
        sid = sessionId (_asSession st)
        conn = _asDbConnection st
    now <- liftIO getCurrentTime
    -- Create assistant message
    let msg = Message
          { messageId         = Nothing
          , messageSessionId  = sid
          , messageRole       = Assistant
          , messageContent    = fullResponse
          , messageMode       = mode
          , messageTimestamp  = now
          , messageModel      = Nothing
          , messageProvider   = Just "OpenRouter"
          , messageTokensUsed = Nothing
          , messageError      = Nothing
          }

    -- Save message to database
    savedResult <- liftIO $ saveMessage conn msg
    let savedMsg = case savedResult of
          Right m -> m
          Left _  -> msg

    asMessages %= (++ [savedMsg])
    asRequestState .= Idle
    asStreamingText .= ""

    -- Update session timestamp
    _ <- liftIO $ updateSessionTimestamp conn sid

    -- Handle card extraction for card mode
    when (mode == CardGeneration) $ do
      let langMod = _asLangModule st
          langId = languageId (languageInfo langMod)
          msgId = messageId savedMsg
      -- Try to extract card from response
      case extractCardFromResponse langId sid msgId fullResponse now of
        Just card -> do
          -- Save card to database
          savedCardResult <- liftIO $ saveCard conn card
          case savedCardResult of
            Right savedCard -> do
              asPendingCard .= Just savedCard
              asModal .= CardPreviewModal savedCard
            Left _ ->
              -- Failed to save, still show preview
              asPendingCard .= Just card
        Nothing -> pure ()  -- No card found in response

  EvStreamError err -> do
    asRequestState .= RequestFailed err
    asLastError .= Just err

  EvAnkiStatusUpdate connected ->
    asAnkiConnected .= connected

  EvCardPushResult result -> case result of
    Right _noteId -> do
      asPendingCard .= Nothing
      asModal .= NoModal
    Left err -> do
      asLastError .= Just err

  EvSessionsLoaded summaries ->
    asModal .= SessionsModal summaries 0

  EvMessagesLoaded msgs ->
    asMessages .= msgs

  EvLanguageReloaded newLangMod ->
    asLangModule .= newLangMod

  EvConfigReloaded newConfig ->
    asConfig .= newConfig

  EvTick ->
    -- Periodic tick for animations, status updates, etc.
    pure ()

-- | Handle keyboard events in main view (no modal)
handleMainEvent :: Event -> EventM Name AppState ()
handleMainEvent ev = case ev of
  -- Quit
  EvKey (KChar 'q') [MCtrl] -> asModal .= ConfirmQuitModal
  EvKey (KChar 'c') [MCtrl] -> do
    st <- get
    case _asRequestState st of
      Requesting -> asRequestState .= Idle
      Streaming  -> asRequestState .= Idle
      _          -> asModal .= ConfirmQuitModal

  -- Help
  EvKey (KFun 1) [] -> asModal .= HelpModal
  EvKey (KChar '?') [] -> do
    st <- get
    when (_asFocus st == FocusHistory) $
      asModal .= HelpModal

  -- Settings (Ctrl+, or F2)
  EvKey (KChar ',') [MCtrl] -> asModal .= SettingsModal
  EvKey (KFun 2) []         -> asModal .= SettingsModal

  -- Mode switching
  EvKey (KChar '\t') [] -> cycleMode 1
  EvKey KBackTab []     -> cycleMode (-1)
  EvKey (KChar '1') []  -> asCurrentMode .= Conversation
  EvKey (KChar '2') []  -> asCurrentMode .= Correction
  EvKey (KChar '3') []  -> asCurrentMode .= Translation
  EvKey (KChar '4') []  -> asCurrentMode .= CardGeneration

  -- New session
  EvKey (KChar 'n') [MCtrl] -> do
    st <- get
    let langMod = _asLangModule st
        conn = _asDbConnection st
    newSid <- liftIO $ UUID.toText <$> nextRandom
    now <- liftIO getCurrentTime
    let newSession = Session
          { sessionId          = newSid
          , sessionLanguageId  = languageId (languageInfo langMod)
          , sessionMode        = _asCurrentMode st
          , sessionLearnerLevel = learnerLevel langMod
          , sessionCreatedAt   = now
          , sessionUpdatedAt   = now
          , sessionTitle       = Nothing
          , sessionActive      = True
          }

    -- Persist new session to database
    _ <- liftIO $ createSession conn newSession

    asSession .= newSession
    asMessages .= []
    asInputEditor .= mkInputEditor

  -- Sessions list
  EvKey (KChar 's') [MCtrl] -> do
    st <- get
    let conn = _asDbConnection st
        chan = _asEventChannel st
    -- Trigger async load of sessions
    liftIO $ void $ forkIO $ do
      result <- getRecentSessions conn 50
      case result of
        Right sessionsWithCount -> do
          let summaries = map sessionToSummary sessionsWithCount
          writeBChan chan (EvSessionsLoaded summaries)
        Left _ ->
          -- On error, show empty list
          writeBChan chan (EvSessionsLoaded [])
    -- Show modal immediately with loading state (empty list)
    asModal .= SessionsModal [] 0

  -- Show pending card
  EvKey (KChar 'a') [MCtrl] -> do
    st <- get
    case _asPendingCard st of
      Just card -> asModal .= CardPreviewModal card
      Nothing   -> pure ()

  -- Focus switching
  EvKey KPageUp [] -> do
    asFocus .= FocusHistory
    let vp = viewportScroll NameHistoryViewport
    vScrollPage vp Up

  EvKey KPageDown [] -> do
    asFocus .= FocusHistory
    let vp = viewportScroll NameHistoryViewport
    vScrollPage vp Down

  -- Send message
  EvKey KEnter [] -> do
    st <- get
    let content = getEditorContent (_asInputEditor st)
    unless (T.null content) $ do
      unless (isRequesting (_asRequestState st)) $ do
        let sid = sessionId (_asSession st)
            mode = _asCurrentMode st
            conn = _asDbConnection st
        now <- liftIO getCurrentTime
        -- Create user message
        let msg = Message
              { messageId         = Nothing
              , messageSessionId  = sid
              , messageRole       = User
              , messageContent    = content
              , messageMode       = mode
              , messageTimestamp  = now
              , messageModel      = Nothing
              , messageProvider   = Nothing
              , messageTokensUsed = Nothing
              , messageError      = Nothing
              }

        -- Save message to database and get version with ID
        savedResult <- liftIO $ saveMessage conn msg
        let savedMsg = case savedResult of
              Right m -> m
              Left _  -> msg  -- On error, use original (will lose ID)

        asMessages %= (++ [savedMsg])
        asInputEditor .= mkInputEditor
        asRequestState .= Requesting
        asLastError .= Nothing

        -- Update session timestamp
        _ <- liftIO $ updateSessionTimestamp conn sid

        -- Trigger async LLM request
        let chan = _asEventChannel st
            client = _asLlmClient st
            langMod = _asLangModule st
        liftIO $ void $ forkIO $ sendLLMRequest st chan client langMod savedMsg

        -- Scroll to bottom
        let vp = viewportScroll NameHistoryViewport
        vScrollToEnd vp

  -- Editor events
  _ -> do
    asFocus .= FocusInput
    zoom asInputEditor $ handleEditorEvent (VtyEvent ev)

-- | Handle keyboard events when a modal is open
handleModalEvent :: Event -> EventM Name AppState ()
handleModalEvent ev = do
  st <- get
  case _asModal st of
    HelpModal -> case ev of
      EvKey _ _ -> asModal .= NoModal
      _         -> pure ()

    ConfirmQuitModal -> case ev of
      EvKey (KChar 'y') [] -> halt
      EvKey (KChar 'Y') [] -> halt
      EvKey (KChar 'n') [] -> asModal .= NoModal
      EvKey (KChar 'N') [] -> asModal .= NoModal
      EvKey KEsc []        -> asModal .= NoModal
      _                    -> pure ()

    SettingsModal -> case ev of
      EvKey KEsc []        -> asModal .= NoModal
      EvKey (KChar 's') [] -> asModal .= NoModal  -- TODO: Save
      EvKey (KChar 'r') [] -> asModal .= NoModal  -- TODO: Reload
      EvKey (KChar 'e') [] -> asModal .= NoModal  -- TODO: Edit prompts
      _                    -> pure ()

    CardPreviewModal card -> case ev of
      EvKey KEsc []        -> asModal .= NoModal
      EvKey KEnter []      -> do
        -- Push card to Anki
        let ankiClient = _asAnkiClient st
            conn = _asDbConnection st
            chan = _asEventChannel st

        -- Convert AnkiCard to AnkiNote for the API
        let note = AnkiNote
              { anDeckName  = cardDeck card
              , anModelName = "Basic"  -- Standard Anki note type
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
      EvKey (KChar 'd') [] -> do
        asPendingCard .= Nothing
        asModal .= NoModal
      _                    -> pure ()

    SessionsModal sums idx -> case ev of
      EvKey KEsc []        -> asModal .= NoModal
      EvKey (KChar 'j') [] -> asModal .= SessionsModal sums (min (idx + 1) (length sums - 1))
      EvKey KDown []       -> asModal .= SessionsModal sums (min (idx + 1) (length sums - 1))
      EvKey (KChar 'k') [] -> asModal .= SessionsModal sums (max (idx - 1) 0)
      EvKey KUp []         -> asModal .= SessionsModal sums (max (idx - 1) 0)
      EvKey KEnter [] -> do
        when (idx >= 0 && idx < length sums) $ do
          let selectedSummary = sums !! idx
              sid = summaryId selectedSummary
              conn = _asDbConnection st
              chan = _asEventChannel st

          -- Load messages for the selected session asynchronously
          liftIO $ void $ forkIO $ do
            result <- getSessionMessages conn sid
            case result of
              Right msgs -> writeBChan chan (EvMessagesLoaded msgs)
              Left _ -> writeBChan chan (EvMessagesLoaded [])

          -- Update session info in state immediately
          now <- liftIO getCurrentTime
          let newSession = Session
                { sessionId = sid
                , sessionLanguageId = summaryLanguageId selectedSummary
                , sessionMode = summaryMode selectedSummary
                , sessionLearnerLevel = learnerLevel (_asLangModule st)
                , sessionCreatedAt = now  -- Not accurate but placeholder
                , sessionUpdatedAt = summaryLastActivity selectedSummary
                , sessionTitle = Just (summaryTitle selectedSummary)
                , sessionActive = True
                }
          asSession .= newSession
          asCurrentMode .= summaryMode selectedSummary
          asMessages .= []  -- Will be populated by EvMessagesLoaded
          asModal .= NoModal
      _                    -> pure ()

    ErrorModal _ -> case ev of
      EvKey _ _ -> asModal .= NoModal
      _         -> pure ()

    NoModal -> pure ()

-- | Cycle through modes
cycleMode :: Int -> EventM Name AppState ()
cycleMode delta = do
  current <- use asCurrentMode
  let modes = [Conversation, Correction, Translation, CardGeneration]
      currentIdx = case elemIndex current modes of
        Just i  -> i
        Nothing -> 0
      newIdx = (currentIdx + delta) `mod` length modes
      newMode = modes !! newIdx
  asCurrentMode .= newMode
  where
    elemIndex x xs = go 0 xs
      where
        go _ [] = Nothing
        go i (y:ys)
          | x == y    = Just i
          | otherwise = go (i + 1) ys

-- ════════════════════════════════════════════════════════════════
-- LLM INTERACTION
-- ════════════════════════════════════════════════════════════════

-- | Send an LLM request asynchronously
sendLLMRequest
  :: AppState
  -> BChan TapirEvent
  -> LLMClient
  -> LanguageModule
  -> Message
  -> IO ()
sendLLMRequest st chan client _langMod userMsg = do
  let messages = _asMessages st ++ [userMsg]

  -- Build chat request
  -- Convert our messages to LLM ChatMessages
  let chatMsgs = map toClientMessage messages
      req = defaultChatRequest (providerDefaultModel (_asConfig st)) chatMsgs

  -- Stream callback
  let onToken token = writeBChan chan (EvStreamChunk token)

  -- Send streaming request
  result <- llmStreamComplete client req onToken Nothing

  case result of
    Left err -> writeBChan chan (EvStreamError err)
    Right sr -> writeBChan chan (EvStreamComplete (srFullResponse sr))
  where
    toClientMessage :: Message -> ChatMessage
    toClientMessage msg = ChatMessage
      { cmRole    = roleToText (messageRole msg)
      , cmContent = messageContent msg
      }

    providerDefaultModel _cfg = "z-ai/glm-4.7"  -- TODO: Get from config

-- ════════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS
-- ════════════════════════════════════════════════════════════════

-- | Convert a Session and message count to a SessionSummary
sessionToSummary :: (Session, Int) -> SessionSummary
sessionToSummary (sess, count) = SessionSummary
  { summaryId = sessionId sess
  , summaryTitle = case sessionTitle sess of
      Just t  -> t
      Nothing -> "Untitled Session"
  , summaryLanguageId = sessionLanguageId sess
  , summaryMode = sessionMode sess
  , summaryMessageCount = count
  , summaryLastActivity = sessionUpdatedAt sess
  }

-- | Extract a flashcard from an LLM response
-- Looks for common patterns:
-- 1. Labeled format: "Front: ...\nBack: ..."
-- 2. Simple format: first line = front, rest = back
extractCardFromResponse :: T.Text -> T.Text -> Maybe Int -> T.Text -> UTCTime -> Maybe AnkiCard
extractCardFromResponse langId sessionId' sourceMsgId response now =
  -- Try parsing patterns in order of preference
  tryLabeledFormat `orElse` trySimpleFormat
  where
    -- Try "Front: ...\nBack: ..." format (case-insensitive)
    tryLabeledFormat :: Maybe AnkiCard
    tryLabeledFormat =
      let lowerResponse = T.toLower response
      in if "front:" `T.isInfixOf` lowerResponse && "back:" `T.isInfixOf` lowerResponse
         then
           -- Find the position of "back:" (case-insensitive)
           let -- Split on "back:" (trying both cases)
               (beforeBack, afterBack) =
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
              else Just $ mkCard frontText backText
         else Nothing

    -- Simple format: first non-empty line = front, rest = back
    trySimpleFormat :: Maybe AnkiCard
    trySimpleFormat =
      let nonEmptyLines = filter (not . T.null . T.strip) (T.lines response)
      in case nonEmptyLines of
        (front:rest) | not (null rest) ->
          Just $ mkCard (T.strip front) (T.strip $ T.intercalate "\n" rest)
        _ -> Nothing

    mkCard :: T.Text -> T.Text -> AnkiCard
    mkCard front back = AnkiCard
      { cardId          = Nothing
      , cardSessionId   = sessionId'
      , cardLanguageId  = langId
      , cardFront       = front
      , cardBack        = back
      , cardTags        = [langId]  -- Default tag is the language
      , cardDeck        = langId <> "::TAPIR"  -- Default deck
      , cardSourceMsgId = sourceMsgId
      , cardAnkiNoteId  = Nothing
      , cardPushedAt    = Nothing
      , cardCreatedAt   = now
      }

    -- Helper for Maybe alternation
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse Nothing b = b
    orElse a       _ = a
