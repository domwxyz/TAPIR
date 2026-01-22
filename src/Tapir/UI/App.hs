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
import Data.Time (getCurrentTime)
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
    asMessages %= (++ [msg])
    asRequestState .= Idle
    asStreamingText .= ""

    -- Handle card extraction for card mode
    when (mode == CardGeneration) $ do
      -- Card extraction would happen here
      -- For now, just note that it needs implementation
      pure ()

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
    asSession .= newSession
    asMessages .= []
    asInputEditor .= mkInputEditor

  -- Sessions list
  EvKey (KChar 's') [MCtrl] -> do
    -- Would trigger async load of sessions
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
        asMessages %= (++ [msg])
        asInputEditor .= mkInputEditor
        asRequestState .= Requesting
        asLastError .= Nothing

        -- Trigger async LLM request
        let chan = _asEventChannel st
            client = _asLlmClient st
            langMod = _asLangModule st
        liftIO $ void $ forkIO $ sendLLMRequest st chan client langMod msg

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

    CardPreviewModal _card -> case ev of
      EvKey KEsc []        -> asModal .= NoModal
      EvKey KEnter []      -> do
        -- TODO: Push to Anki
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
        -- TODO: Load selected session
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
