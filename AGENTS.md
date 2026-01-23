# AGENTS.md - AI Agent Development Guide for TAPIR

**Project:** TAPIR (Translation API Router)
**Type:** Haskell + brick TUI for language learning
**Status:** Complete (Phase 6/6)
**Last Updated:** January 22, 2026

---

## Quick Start for Agents

### Build & Run
```bash
cabal build           # Build project
cabal test            # Run tests
cabal run tapir       # Run application
```

### Key File Locations
| Concern | Location |
|---------|----------|
| Entry point | `app/Main.hs` |
| Main app logic | `src/Tapir/UI/App.hs` |
| State types | `src/Tapir/UI/Types.hs` |
| Config loading | `src/Tapir/Config/Loader.hs` |
| Database ops | `src/Tapir/Db/Repository.hs` |
| LLM client | `src/Tapir/Client/LLM/OpenRouter.hs` |
| Anki client | `src/Tapir/Client/Anki.hs` |
| **Structured responses** | `src/Tapir/Types/Response.hs` |
| **Tool definitions** | `src/Tapir/Client/LLM/Tools.hs` |
| **Response parsing** | `src/Tapir/Client/LLM/Response.hs` |
| **Response rendering** | `src/Tapir/UI/Structured.hs` |

---

## Project Overview

TAPIR is a **language-agnostic terminal-based language learning assistant** built with Haskell and brick. It provides conversational practice, grammar correction, translation, and Anki flashcard generation through a keyboard-driven TUI interface.

### Core Features
- **Conversational practice** - Chat with LLM in target language
- **Grammar correction** - Detailed corrections with explanations
- **Translation** - Bidirectional translation with nuance preservation
- **Anki integration** - Direct flashcard generation and push to Anki
- **Language-agnostic** - All language-specific logic in YAML configs
- **Provider-agnostic** - Support for OpenRouter, Anthropic, OpenAI, Ollama

### Design Philosophy
1. **Configuration over code** - Language settings, prompts, providers in YAML
2. **Types as documentation** - ADTs model domain precisely
3. **Purity where practical** - Side effects at edges
4. **Keyboard-first UX** - Every action has a keybinding
5. **Offline-capable** - Local history persists without network
6. **Structured output** - Tool/function calling guarantees parseable responses

---

## Structured Response Architecture

TAPIR uses OpenAI-compatible tool/function calling to guarantee structured JSON responses from LLMs. This replaces the previous streaming text approach with a non-streaming, structured approach that provides:

- Guaranteed structure: LLM must call the specified function with valid JSON
- Mode-specific schemas: Each mode has its own tool definition
- Rich metadata: Responses include vocabulary, corrections, examples, notes, etc.
- Fallback handling: Falls back to raw text if tool calls fail

### Tool Definitions (`Tapir.Client.LLM.Tools`)

Each mode has a dedicated tool that the LLM must call:

| Mode | Tool Name | Purpose |
|------|-----------|---------|
| Conversation | `send_conversation_reply` | Reply with corrections, vocab highlights, grammar tips |
| Correction | `submit_correction` | Original/corrected text, detailed corrections |
| Translation | `submit_translation` | Translation with notes, alternatives, formality |
| Card Generation | `create_flashcard` | Front/back, examples, pronunciation, mnemonics |

**Tool Structure Example:**
```haskell
conversationTool = Tool "function" ToolFunction
  { tfName = "send_conversation_reply"
  , tfDescription = "Send a conversational reply..."
  , tfStrict = True  -- Enforce schema validation
  , tfParameters = object
      [ "type" .= "object"
      , "additionalProperties" .= False
      , "properties" .= object
          [ "reply" .= object [...]
          , "corrections" .= object [...]
          , "vocab_highlights" .= object [...]
          , "grammar_tip" .= object [...]
          ]
      ]
  }
```

### Response Types (`Tapir.Types.Response`)

The `StructuredResponse` sum type encapsulates all mode-specific responses:

```haskell
data StructuredResponse
  = SRConversation !ConversationResponse
  | SRCorrection !CorrectionResponse
  | SRTranslation !TranslationResponse
  | SRCard !CardResponse
  | SRRawText !Text  -- Fallback for unparseable responses
```

#### ConversationResponse
```haskell
data ConversationResponse = ConversationResponse
  { convReply       :: !Text              -- Main reply
  , convCorrections :: ![Correction]      -- Errors in user's message
  , convVocab       :: ![VocabHighlight]  -- 1-2 vocabulary items
  , convGrammarTip  :: !(Maybe Text)       -- Brief grammar note
  }
```

#### CorrectionResponse
```haskell
data CorrectionResponse = CorrectionResponse
  { crOriginal      :: !Text              -- Original text
  , crCorrected     :: !Text              -- Corrected version
  , crIsCorrect     :: !Bool              -- True if no errors
  , crCorrections   :: ![Correction]      -- List of corrections
  , crEncouragement :: !(Maybe Text)      -- "Great job!" message
  , crOverallNote   :: !(Maybe Text)      -- General feedback
  }

data Correction = Correction
  { corrOriginal    :: !Text
  , corrFixed       :: !Text
  , corrExplanation :: !Text
  , corrCategory    :: !(Maybe CorrectionCategory)
  , corrSeverity    :: !(Maybe Text)       -- "minor", "moderate", "significant"
  }
```

#### TranslationResponse
```haskell
data TranslationResponse = TranslationResponse
  { trSourceText    :: !Text              -- Original text
  , trSourceLang    :: !Text              -- Source language
  , trTargetText    :: !Text              -- Translation
  , trTargetLang    :: !Text              -- Target language
  , trNotes         :: ![TranslationNote]  -- Tricky phrases, idioms
  , trAlternatives  :: ![Text]            -- Alternative translations
  , trFormality     :: !(Maybe Formality)  -- formal, informal, neutral
  , trLiteralMeaning :: !(Maybe Text)      -- Word-for-word translation
  }
```

#### CardResponse
```haskell
data CardResponse = CardResponse
  { cardRespFront        :: !Text           -- Target language
  , cardRespBack         :: !Text           -- Native + context
  , cardRespTags         :: ![Text]         -- Organization tags
  , cardRespExample      :: !(Maybe Text)   -- Example sentence
  , cardRespExampleTrans :: !(Maybe Text)   -- Example translation
  , cardRespPronunciation :: !(Maybe Text)  -- IPA/phonetic
  , cardRespAudio        :: !(Maybe Text)   -- Audio URL
  , cardRespNotes        :: !(Maybe Text)   -- Usage notes
  , cardRespRelated      :: ![Text]         -- Related words
  , cardRespMnemonic     :: !(Maybe Text)   -- Memory aid
  }
```

### Request Building (`Tapir.Client.LLM.Request`)

Requests are built with tools and forced tool choice:

```haskell
buildRequestWithTools config langMod mode history currentMsg =
  let tool = toolForMode mode
      toolName = toolNameForMode mode
      toolChoice = Just (ToolChoiceForced toolName)  -- Force tool use
  in baseReq
      { crTools = Just [tool]
      , crToolChoice = toolChoice
      , crStream = False  -- Tool calls don't stream
      }
```

**Key Points:**
- `rcUseTools = True`: Include tool definitions in request
- `rcForceTools = True`: Use `ToolChoiceForced` to require specific tool
- `rcStream = False`: Non-streaming for tool calls (tool JSON isn't streamed well)

### Response Parsing (`Tapir.Client.LLM.Response`)

Parse tool call arguments into structured types:

```haskell
parseResponse :: Mode -> ChatResponse -> ParsedResponse
parseResponse mode resp =
  case respChoices resp of
    (choice:_) ->
      let msg = choiceMessage choice
      in case rmToolCalls msg of
        Just (tc:_) -> parseToolCallResponse mode tc
        _ -> ParsedRawText content  -- Fallback

parseToolCallResponse :: Mode -> ToolCall -> ParsedResponse
parseToolCallResponse mode tc =
  let args = fcArguments (tcFunction tc)  -- JSON string
  in case parseResponseForMode mode args of
    Right structured -> ParsedStructured structured
    Left err -> ParsedError ...
```

### Event Flow

1. **User sends message** → `handleEvent (KChar '\RET')`
2. **Build request** with tools → `buildRequestWithTools`
3. **Send to LLM** (non-streaming) → `llmRequestComplete`
4. **Receive response** with `tool_calls` array
5. **Parse tool call** → `parseToolCallResponse`
6. **Send event** → `EvStructuredResponse structured`
7. **Handle in UI** → `handleCustomEvent (EvStructuredResponse ...)`
8. **Save message** to database with plain text (`responseToText`)
9. **Set pending structured** → `asPendingStructured .= Just structured`
10. **Render** → `renderStructuredResponse` in viewport

### Rendering (`Tapir.UI.Structured`)

Each mode has dedicated rendering with sections, colors, and layout:

#### Conversation Mode
- Main reply highlighted as assistant message
- Inline corrections shown as: `✗ original → fixed (explanation)`
- Vocabulary displayed compactly: `Vocab: palabra=meaning · otro=other`
- Grammar tip shown if present

#### Correction Mode
- Shows "Original:" and "Corrected:" sections
- "Perfect!" message when `is_correct = true`
- Detailed corrections with categories and severity:
  ```
  original → fixed [grammar] (moderate)
      Explanation of why this was wrong...
  ```
- Encouragement and overall notes in separate sections

#### Translation Mode
- Source and target languages as section headers
- Formality badge: `[formal]`, `[informal]`, or `[neutral]`
- Literal meaning in parentheses when idiomatic
- Alternatives and translation notes in separate sections

#### Card Mode
- Front and back prominently displayed
- Pronunciation with 🔊 icon
- Example with translation
- Notes, mnemonic (💡), related words, tags in sections

### Storage

While structured responses are rendered in the UI, only plain text is stored:

```haskell
-- Extract plain text for database storage
responseToText :: StructuredResponse -> Text
responseToText = \case
  SRConversation cr -> convReply cr
  SRCorrection cr   -> crCorrected cr
  SRTranslation tr  -> trTargetText tr
  SRCard cr         -> cardRespFront cr <> " — " <> cardRespBack cr
  SRRawText t       -> t
```

This keeps the database simple while preserving all functionality. The structured response is transient, stored only in `asPendingStructured` during the session.

---

## Implementation Status

### Completed Features
- Configuration system with YAML loading from `~/.config/tapir/`
- Language modules (Spanish reference implementation)
- SQLite database with full schema and repository pattern
- LLM client with OpenRouter integration and streaming support
- Complete brick TUI with all four modes (Chat, Correct, Translate, Card)
- Streaming token display via BChan
- Session management (create, list, load, delete with message history)
- Modal dialogs (Help, Settings, Sessions, Card Preview, Quit confirm)
- Database persistence for messages, sessions, cards
- Anki client with connection checking and note pushing
- Settings modal with level cycling and prompt preview
- Card generation with robust JSON parsing and markdown fence handling
- Text wrapping with dynamic width calculation
- System prompt injection per mode

### Known Limitations
- **Windows terminals**: Must use Windows Terminal/PowerShell (not Git Bash/mintty)

---

## Project Structure

```
TAPIR/
├── app/
│   └── Main.hs                 # Entry point, CLI parsing, initialization
├── src/Tapir/
│   ├── Types.hs                # Message, Session, AnkiCard, Role, TapirError (re-exports)
│   ├── Types/
│   │   ├── Mode.hs             # Mode enum (Conversation, Correction, Translation, CardGeneration)
│   │   ├── Language.hs         # LanguageInfo, LanguageModule, LearnerLevel
│   │   ├── Provider.hs         # ProviderType, ProviderConfig
│   │   └── Response.hs        # Structured response types for all modes
│   ├── Config/
│   │   ├── Types.hs            # AppConfig, UIConfig, DatabaseConfig
│   │   ├── Loader.hs           # YAML loading, prompt interpolation
│   │   └── Defaults.hs         # Default configurations
│   ├── UI/
│   │   ├── Types.hs            # AppState, Modal, RequestState, TapirEvent
│   │   ├── App.hs              # Main brick app, handleEvent, handleCustomEvent
│   │   ├── Attrs.hs            # Color theme attributes
│   │   ├── Widgets.hs          # Reusable widget helpers
│   │   ├── Chat.hs             # Chat history viewport rendering
│   │   ├── Input.hs            # Text editor widget
│   │   ├── StatusBar.hs        # Mode tabs, status info
│   │   ├── Modals.hs           # All modal dialogs
│   │   └── Structured.hs       # Structured response rendering
│   ├── Client/
│   │   ├── LLM.hs              # Abstract LLM client interface
│   │   ├── LLM/
│   │   │   ├── Types.hs        # ChatMessage, ChatRequest, StreamChunk, ToolCall
│   │   │   ├── Tools.hs        # Tool definitions for structured output
│   │   │   ├── Request.hs      # Request building with tools
│   │   │   ├── Response.hs     # Response parsing from tool calls
│   │   │   └── OpenRouter.hs   # OpenRouter implementation with SSE
│   │   └── Anki.hs             # AnkiConnect client
│   ├── Db/
│   │   ├── Schema.hs           # Database initialization, migrations
│   │   └── Repository.hs       # All CRUD operations
│   └── Util/                   # Utility functions
├── test/                       # Test suite
│   ├── Spec.hs                 # Main test entry
│   └── Tapir/
│       ├── Config/LoaderSpec.hs
│       ├── Client/LLMSpec.hs
│       └── Db/RepositorySpec.hs
├── languages/                  # Template language modules
│   └── spanish.yaml            # Reference implementation
├── CLAUDE.md                   # Development guide
├── AGENTS.md                   # AI agent guide (this file)
└── README.md                   # User-facing documentation
```

---

## Configuration System

### Config File Locations
```
~/.config/tapir/
├── config.yaml              # Main configuration
└── languages/
    ├── spanish.yaml         # Spanish language module (reference)
    ├── french.yaml          # Additional languages
    └── japanese.yaml        # User can add more
```

### Main Config Structure (`~/.config/tapir/config.yaml`)

```yaml
active_language: spanish

provider:
  type: openrouter           # openrouter | anthropic | openai | ollama
  api_key: "your-api-key"    # Or use ${OPENROUTER_API_KEY}
  model: "z-ai/glm-4.7"
  temperature: 0.7
  max_tokens: 2000
  stream: true

ui:
  theme: default             # default | dark | light
  chat:
    show_timestamps: true
    word_wrap: true
  editor:
    max_lines: 5

database:
  path: "~/.local/share/tapir/tapir.db"

anki:
  enabled: true
  host: "localhost"
  port: 8765
```

### Language Module Structure (`~/.config/tapir/languages/spanish.yaml`)

```yaml
language:
  id: spanish
  name: "Spanish"
  native_name: "Español"
  code: es
  native_language: english
  native_language_code: en
  variant: latam              # latam | spain | neutral

learner_level: A1             # A1 | A2 | B1 | B2 | C1 | C2

anki:
  default_deck: "Spanish::Vocab"
  default_tags: ["spanish", "tapir"]
  note_type: "Basic"
  fields:
    front: "Front"
    back: "Back"

modes:
  conversation:
    label: "Chat"
    description: "Conversation practice"
    system_prompt: |
      You are a {{language}} learning companion...
      [variables: {{language}}, {{native_language}}, {{variant}},
       {{level}}, {{level_description}}]

  correction:
    label: "Correct"
    system_prompt: |
      You are a {{language}} grammar checker...

  translation:
    label: "Translate"
    system_prompt: |
      Translate between {{language}} and {{native_language}}...

  card_generation:
    label: "Card"
    system_prompt: |
      Generate JSON flashcard: {"front": "...", "back": "...", "tags": [...]}
```

### Prompt Variables
Available for interpolation in system prompts:
- `{{language}}` - Target language name (e.g., "Spanish")
- `{{native_language}}` - User's native language (e.g., "English")
- `{{variant}}` - Regional variant (e.g., "Latin American")
- `{{level}}` - CEFR level (e.g., "A1")
- `{{level_description}}` - Level description (e.g., "beginner")

---

## Database Schema

### Tables

#### `sessions`
```sql
id TEXT PRIMARY KEY              -- UUID v4
language_id TEXT NOT NULL        -- e.g., "spanish"
mode TEXT NOT NULL               -- "conversation", "correction", etc.
learner_level TEXT NOT NULL      -- "A1", "B2", etc.
created_at TEXT NOT NULL
updated_at TEXT NOT NULL
title TEXT                       -- Optional user-set title
active INTEGER NOT NULL DEFAULT 1  -- 0 = archived, 1 = active
```

#### `messages`
```sql
id INTEGER PRIMARY KEY AUTOINCREMENT
session_id TEXT NOT NULL         -- FK to sessions.id
role TEXT NOT NULL               -- "user" | "assistant"
content TEXT NOT NULL
mode TEXT NOT NULL
timestamp TEXT NOT NULL
model TEXT                       -- Model used (e.g., "glm-4.7")
provider TEXT                    -- "OpenRouter", etc.
tokens_used INTEGER
error TEXT                       -- Error message if failed
FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
```

#### `cards`
```sql
id INTEGER PRIMARY KEY AUTOINCREMENT
session_id TEXT NOT NULL         -- FK to sessions.id
language_id TEXT NOT NULL
front TEXT NOT NULL              -- Target language text
back TEXT NOT NULL               -- Native language + context
tags TEXT NOT NULL               -- JSON array
deck TEXT NOT NULL
source_msg_id INTEGER            -- FK to messages.id
anki_note_id INTEGER             -- AnkiConnect note ID after push
pushed_at TEXT                   -- ISO 8601 timestamp
created_at TEXT NOT NULL
FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
FOREIGN KEY (source_msg_id) REFERENCES messages(id) ON DELETE SET NULL
```

---

## Architecture Patterns

### Event Flow

1. User input captured by brick
2. Events dispatched to `handleEvent` in `Tapir.UI.App`
3. LLM requests spawn async thread via `forkIO`
4. Streaming tokens sent via `BChan TapirEvent`
5. `handleCustomEvent` updates UI state in `EventM`
6. brick re-renders UI

**Critical Pattern**: Never update UI directly from async threads. Always use `BChan` to send events to brick's event loop.

### Streaming Architecture

```haskell
-- In async thread:
streamRequest client req $ \token -> do
  writeBChan eventChan (EvStreamChunk token)

-- In EventM handler:
handleCustomEvent (EvStreamChunk token) = do
  asStreamingText %= (<> token)
  vScrollToEnd viewport
```

### State Management

`AppState` uses lenses (generated by `makeLenses`) for updates:

```haskell
-- Update a field
asCurrentMode .= Conversation

-- Modify a field
asMessages %= (++ [newMessage])

-- Zoom into a sub-structure
zoom asInputEditor $ handleEditorEvent ev
```

### Error Handling

All IO operations return `Either TapirError a`:

```haskell
-- Pattern in IO operations
doAction :: IO (Either TapirError Result)
doAction = do
  result <- try $ ... -- risky operation
  case result of
    Left (e :: SomeException) -> pure $ Left $ DatabaseError (T.pack $ show e)
    Right val -> pure $ Right val

-- Pattern in EventM
result <- liftIO doAction
case result of
  Right val -> ... -- use value
  Left err  -> asLastError .= Just err
```

---

## Key Types Reference

### Core Domain Types (`Tapir.Types`)

```haskell
data Role = User | Assistant | System

data Mode
  = Conversation
  | Correction
  | Translation
  | CardGeneration

data LearnerLevel = A1 | A2 | B1 | B2 | C1 | C2

data Message = Message
  { messageId         :: Maybe Int
  , messageSessionId  :: Text
  , messageRole       :: Role
  , messageContent    :: Text
  , messageMode       :: Mode
  , messageTimestamp  :: UTCTime
  , messageModel      :: Maybe Text
  , messageProvider   :: Maybe Text
  , messageTokensUsed :: Maybe Int
  , messageError      :: Maybe Text
  }

data Session = Session
  { sessionId          :: Text
  , sessionLanguageId  :: Text
  , sessionMode        :: Mode
  , sessionLearnerLevel :: LearnerLevel
  , sessionCreatedAt   :: UTCTime
  , sessionUpdatedAt   :: UTCTime
  , sessionTitle       :: Maybe Text
  , sessionActive      :: Bool
  }

data AnkiCard = AnkiCard
  { cardId          :: Maybe Int
  , cardSessionId   :: Text
  , cardLanguageId  :: Text
  , cardFront       :: Text
  , cardBack        :: Text
  , cardTags        :: [Text]
  , cardDeck        :: Text
  , cardSourceMsgId :: Maybe Int
  , cardAnkiNoteId  :: Maybe Integer
  , cardPushedAt    :: Maybe UTCTime
  , cardCreatedAt   :: UTCTime
  }

data TapirError
  = ConfigNotFound FilePath
  | ConfigParseError FilePath Text
  | LanguageNotFound Text
  | APIKeyMissing
  | APIError Int Text
  | NetworkError Text
  | StreamingError Text
  | DatabaseError Text
  | SessionNotFound Text
  | AnkiNotRunning
  | CardParseError Text
  | InternalError Text
```

### UI Types (`Tapir.UI.Types`)

```haskell
data Modal
  = NoModal
  | HelpModal
  | SessionsModal [SessionSummary] Int
  | CardPreviewModal AnkiCard
  | SettingsModal
  | ConfirmQuitModal
  | ErrorModal TapirError
  | PromptPreviewModal Mode Text

data RequestState
  = Idle
  | Requesting
  | Streaming
  | RequestFailed TapirError

data TapirEvent
  = EvStreamChunk Text
  | EvStreamComplete Text
  | EvStreamError TapirError
  | EvAnkiStatusUpdate Bool
  | EvCardPushResult (Either TapirError Integer)
  | EvSessionsLoaded [SessionSummary]
  | EvMessagesLoaded [Message]
  | EvLanguageReloaded LanguageModule
  | EvConfigReloaded AppConfig
  | EvTick
```

---

## Common Functions by Task

| Task | Function | Location |
|------|----------|----------|
| Load config | `loadConfig` | `Tapir.Config.Loader` |
| Load language module | `loadLanguageModule` | `Tapir.Config.Loader` |
| Get system prompt | `getSystemPrompt` | `Tapir.Config.Loader` |
| Interpolate prompt | `interpolatePrompt` | `Tapir.Config.Loader` |
| Create LLM client | `mkLLMClient` | `Tapir.Client.LLM` |
| Send streaming request | `llmStreamComplete` | `Tapir.Client.LLM` |
| **Build request with tools** | `buildRequestWithTools` | `Tapir.Client.LLM.Request` |
| **Get tool for mode** | `toolForMode` | `Tapir.Client.LLM.Tools` |
| **Parse LLM response** | `parseResponse` | `Tapir.Client.LLM.Response` |
| **Parse response for mode** | `parseResponseForMode` | `Tapir.Types.Response` |
| **Render structured response** | `renderStructuredResponse` | `Tapir.UI.Structured` |
| Save message | `saveMessage` | `Tapir.Db.Repository` |
| Get messages | `getMessagesBySession` | `Tapir.Db.Repository` |
| Create session | `createSession` | `Tapir.Db.Repository` |
| List sessions | `listSessions` | `Tapir.Db.Repository` |
| Delete session | `deleteSession` | `Tapir.Db.Repository` |
| Check Anki | `checkAnkiConnection` | `Tapir.Client.Anki` |
| Add Anki note | `addNote` | `Tapir.Client.Anki` |
| Convert card response to Anki card | `cardResponseToAnkiCard` | `Tapir.UI.App` |

---

## Keyboard Shortcuts

 | Key | Action |
|-----|--------|
| **Main Interface** | |
| `Ctrl+P` | Command menu |
| `Ctrl+N` | New session |
| `Ctrl+S` | Session list |
| `Ctrl+A` | Show pending card |
| `Ctrl+Q` | Quit (with confirmation) |
| `Ctrl+C` | Cancel request / Quit |
| `F1` | Help modal |
| `F2` | Settings modal |
 | **Navigation** | |
| `Tab` / `Shift+Tab` | Cycle modes (Chat/Correct/Translate/Card) |
| `PageUp` / `PageDown` | Scroll history |
| `Enter` | Send message |
| **Modal Navigation** | |
| `Esc` | Close modal |
| `j` / `k` or `↑` / `↓` | Navigate list (command menu, sessions) |
| `Enter` | Select / Execute |
| **Command Menu** | |
| | Lists all available commands (Ctrl+P) |
| **Settings Modal** | |
| `+` / `-` | Cycle learner level |
| `E` | View system prompt |
| `S` | Save settings |
| `R` | Reload config |
| **Session List** | |
| `D` | Delete session |
| `N` | New session |
| **Card Preview** | |
| `Enter` | Push to Anki |
| `D` | Discard card |

---

## Common Pitfalls & Solutions

### 1. Streaming UI Updates from Async Thread
**Wrong:**
```haskell
async $ do
  forM_ tokens $ \token ->
    modifyAppState (addToken token)  -- Direct mutation!
```

**Right:**
```haskell
async $ do
  forM_ tokens $ \token ->
    writeBChan eventChan (EvStreamChunk token)  -- Via BChan!
```

### 2. Using `txtWrap` in Viewport
**Problem:** `txtWrap` creates infinite-height widget, viewport crashes
**Solution:** Use `txt` with manual wrapping via `word-wrap` package

### 3. Windows Terminal Issues
**Symptoms:** "GetConsoleScreenBufferInfo: invalid argument"
**Cause:** Running in Git Bash or mintty
**Solution:** Use Windows Terminal, PowerShell, or Command Prompt

### 4. Missing API Key
**Symptoms:** "API key not configured" error
**Solutions:**
- Set environment: `export OPENROUTER_API_KEY="sk-or-v1-..."`
- Add to config.yaml: `api_key: "sk-or-v1-..."`

### 5. Unicode Display Issues
**Symptoms:** Unicode characters display as boxes
**Solutions:**
- Ensure terminal supports UTF-8
- Set locale: `export LANG=en_US.UTF-8`
- Use a modern terminal (Windows Terminal, iTerm2, etc.)

### 6. Language Module Not Found
**Symptoms:** "Language module not found" error
**Solutions:**
- Ensure `~/.config/tapir/languages/spanish.yaml` exists
- Check `active_language` in config matches filename (without `.yaml`)

---

## Testing

### Running Tests

```bash
# All tests
cabal test

# With output
cabal test --test-show-details=streaming

# Specific test suite
cabal test --test-option=--match="/Repository/"
```

### Test Coverage
- **LLM Types**: JSON serialization/parsing
- **Config Loader**: Prompt interpolation, YAML loading
- **Database**: Schema, CRUD, transactions, foreign keys, cascades

### Adding Tests

```haskell
spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads valid config" $ do
      result <- loadConfig
      result `shouldSatisfy` isRight

    it "returns Left for missing config" $ do
      result <- loadConfig "nonexistent.yaml"
      result `shouldSatisfy` isLeft
```

---

## External APIs

### OpenRouter API

**Base URL:** `https://openrouter.ai/api/v1/chat/completions`

**Request Headers:**
```
Content-Type: application/json
Authorization: Bearer sk-or-v1-...
HTTP-Referer: https://github.com/yourusername/tapir
X-Title: TAPIR Language Learning Assistant
```

**Request Body:**
```json
{
  "model": "z-ai/glm-4.7",
  "messages": [
    {"role": "system", "content": "..."},
    {"role": "user", "content": "..."}
  ],
  "stream": false,
  "temperature": 0.7,
  "max_tokens": 2000,
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "send_conversation_reply",
        "description": "Send a conversational reply...",
        "parameters": { ... },
        "strict": true
      }
    }
  ],
  "tool_choice": {
    "type": "function",
    "function": {"name": "send_conversation_reply"}
  }
}
```

**Streaming Response (SSE):**
```
data: {"id":"gen-xxx","model":"z-ai/glm-4.7","choices":[{"delta":{"content":"Hello"},"index":0}]}

data: {"id":"gen-xxx","model":"z-ai/glm-4.7","choices":[{"delta":{"content":" there!"},"index":0}]}

data: [DONE]
```

### AnkiConnect API

**Endpoint:** `http://localhost:8765`

**Check Connection:**
```json
{"action": "version", "version": 6}
```

**Add Note Request:**
```json
{
  "action": "addNote",
  "version": 6,
  "params": {
    "note": {
      "deckName": "Spanish::Vocab",
      "modelName": "Basic",
      "fields": {
        "Front": "Hola",
        "Back": "Hello"
      },
      "options": {
        "allowDuplicate": false
      },
      "tags": ["spanish", "tapir"]
    }
  }
}
```

**Response:**
```json
{
  "result": 1234567890,
  "error": null
}
```

---

## Technology Stack

### Core Dependencies
- `base >= 4.17 && < 5`
- `text >= 2.0`
- `bytestring >= 0.11`
- `containers >= 0.6`
- `time >= 1.12`
- `uuid >= 1.3`

### TUI
- `brick >= 2.1` - Main TUI framework
- `vty >= 6.0` - Terminal handling
- `vty-unix >= 0.2` - Unix terminal support
- `vty-crossplatform` - Cross-platform terminal
- `microlens >= 0.4` - Lens library
- `microlens-th >= 0.4` - Template Haskell for lenses

### HTTP & JSON
- `req >= 3.13` - HTTP client
- `aeson >= 2.1` - JSON library
- `aeson-pretty >= 0.8` - Pretty JSON printing
- `http-client >= 0.7` - HTTP client backend
- `http-types >= 0.12` - HTTP types

### Database
- `sqlite-simple >= 0.4` - SQLite bindings

### Config & Utilities
- `yaml >= 0.11` - YAML parsing
- `directory >= 1.3` - File system operations
- `filepath >= 1.4` - Path manipulation
- `mtl >= 2.3` - Monad transformers
- `async >= 2.2` - Async operations
- `stm >= 2.5` - Software transactional memory
- `word-wrap >= 0.5` - Text wrapping

### GHC Extensions Used
- `OverloadedStrings` - String literals as `Text`
- `DeriveGeneric` - Generic deriving for JSON
- `DerivingStrategies` - Newtype deriving
- `GeneralizedNewtypeDeriving` - Newtype deriving
- `RecordWildCards` - Record pattern matching
- `LambdaCase` - `\case` syntax
- `TypeApplications` - Explicit type application
- `TemplateHaskell` - Lenses generation
- `QuasiQuotes` - Raw string literals (SQL)

---

## Development Workflow

### Making Changes
1. Read relevant source files to understand context
2. Check AGENTS.md and CLAUDE.md for architectural guidance
3. Make changes following existing patterns
4. Run `cabal build` to check for errors
5. Run tests if applicable: `cabal test`
6. Test manually: `cabal run tapir`

### Adding New Features
1. Define types in appropriate `Tapir.Types.*` module
2. Implement business logic in appropriate service module
3. Update UI in `Tapir.UI.*` modules
4. Add tests to `test/Tapir/`
5. Update documentation if architectural patterns change

### Debugging

**Enable debug output:**
```haskell
import Debug.Trace

handleEvent e = trace ("Event: " ++ show e) $ do
  -- your handler
```

**Check database:**
```bash
sqlite3 ~/.local/share/tapir/tapir.db ".schema"
sqlite3 ~/.local/share/tapir/tapir.db "SELECT * FROM sessions LIMIT 5;"
sqlite3 ~/.local/share/tapir/tapir.db "SELECT * FROM messages ORDER BY id DESC LIMIT 10;"
```

---

## Summary for Agents

This guide provides everything needed to work with TAPIR:

1. **Quick Start** - Build commands and key file locations
2. **Architecture** - Event flow, streaming, state management patterns
3. **Types** - Core domain types and UI types
4. **Configuration** - YAML structure and prompt variables
5. **Database** - Schema and repository functions
6. **APIs** - OpenRouter and AnkiConnect integration
7. **Troubleshooting** - Common issues and solutions
8. **Testing** - Running and writing tests

---

*Last Updated: January 23, 2026*
