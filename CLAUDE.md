# TAPIR Development Guide

**Project:** TAPIR (Translation API Router)
**Type:** Haskell + brick TUI for language learning
**Location:** ~/dev/TAPIR/
**Status:** Complete (Phase 6/6)

---

## Current State

### What's Working

- **Configuration**: YAML config loading from `~/.config/tapir/config.yaml`
- **Language Modules**: Spanish module loads from `~/.config/tapir/languages/spanish.yaml`
- **Database**: SQLite with full schema, repository pattern, tested, message persistence
- **LLM Client**: Multi-provider support (OpenRouter, OpenAI, Ollama) with tool/function calling for structured responses
- **Structured Responses**: Guaranteed JSON responses with mode-specific schemas
- **Response Rendering**: Sectioned display for corrections, vocab, translation notes, card metadata
- **TUI**: brick-based interface with chat, input, status bar, modals
- **Mode Switching**: All four modes (Chat, Correct, Translate, Card)
- **Session Management**: Create, list, load, delete sessions with message history
- **Settings Modal**: Level cycling, prompt preview, language settings
- **Card Generation**: Robust JSON parsing from tool calls
- **Anki Integration**: Connection checking, note pushing via AnkiConnect
- **Text Wrapping**: Dynamic width calculation for proper message display

### Known Issues

None currently known

---

## Project Structure

```
TAPIR/
├── app/Main.hs                 # Entry point
├── src/Tapir/
│   ├── Types.hs               # Message, Session, AnkiCard, Role, TapirError (re-exports)
│   ├── Types/                 # Domain types
│   │   ├── Mode.hs           # Mode enum
│   │   ├── Language.hs       # LanguageInfo, LanguageModule
│   │   ├── Provider.hs       # ProviderType, ProviderConfig
│   │   └── Response.hs       # Structured response types
│   ├── Core/                  # Core utilities
│   │   ├── Constants.hs      # API endpoints, provider names, env vars
│   │   ├── Error.hs          # Safe list operations, error handling
│   │   └── Logging.hs        # Logging placeholder
│   ├── Config/
│   │   ├── Types.hs          # AppConfig, UIConfig, etc.
│   │   ├── Loader.hs         # YAML loading, prompt interpolation
│   │   └── Defaults.hs       # Default configuration
│   ├── Service/               # Business logic layer
│   │   ├── LLM.hs            # LLM request orchestration
│   │   ├── Card.hs           # Card generation & Anki export
│   │   └── Message.hs        # Message persistence
│   ├── Client/
│   │   ├── LLM.hs            # Abstract LLM interface
│   │   ├── LLM/
│   │   │   ├── Base.hs       # Generic OpenAI-compatible client
│   │   │   ├── Types.hs      # ChatMessage, ChatRequest, ToolCall
│   │   │   ├── Tools.hs      # Tool definitions
│   │   │   ├── Request.hs    # Request building
│   │   │   ├── Response.hs   # Response parsing
│   │   │   ├── SSE.hs        # Server-Sent Events parser
│   │   │   ├── OpenRouter.hs # OpenRouter implementation
│   │   │   ├── OpenAI.hs     # OpenAI implementation
│   │   │   └── Ollama.hs     # Ollama implementation (local)
│   │   └── Anki.hs           # AnkiConnect client
│   ├── Db/
│   │   ├── Schema.hs         # Database initialization
│   │   ├── Repository.hs     # CRUD operations
│   │   └── Instances.hs      # SQLite type instances
│   └── UI/
│       ├── Types.hs          # AppState, TapirEvent, Name
│       ├── App.hs            # Main brick app definition
│       ├── Draw.hs           # Render UI to screen
│       ├── Event.hs          # Main event dispatcher
│       ├── Event/            # Event handlers
│       │   ├── Main.hs       # Keyboard input handling
│       │   ├── Custom.hs     # Async event processing
│       │   ├── Message.hs    # Message send/receive
│       │   ├── Session.hs    # Session management
│       │   ├── Card.hs       # Card generation
│       │   ├── Modal.hs      # Modal dialog events
│       │   └── Settings.hs   # Settings modal events
│       ├── Attrs.hs          # Color theme attributes
│       ├── Widgets.hs        # Reusable widget helpers
│       ├── Chat.hs           # Chat history display
│       ├── Input.hs          # Text editor widget
│       ├── StatusBar.hs      # Mode tabs, status info
│       ├── Modals.hs         # Help, Settings, Sessions dialogs
│       ├── Structured.hs     # Structured response rendering
│       └── Command.hs        # Command parsing/execution
├── test/                      # Test suite
└── languages/                 # Template language modules
```

---

## Configuration

### Config File Location

`~/.config/tapir/config.yaml`

```yaml
active_language: spanish

provider:
  # Provider type: openrouter (default), openai, ollama
  type: openrouter
  api_key: "your-api-key-here"
  model: "z-ai/glm-4.7"
  # base_url: "http://localhost:11434"  # Optional: custom endpoint
  temperature: 0.7
  max_tokens: 2000
  stream: true

ui:
  theme: default
  chat:
    show_timestamps: true

database:
  path: "~/.local/share/tapir/tapir.db"

anki:
  enabled: true
  host: "localhost"
  port: 8765
```

### Provider Examples

**OpenRouter (default):**
```yaml
provider:
  type: openrouter
  api_key_env: OPENROUTER_API_KEY
  model: "z-ai/glm-4.7"
```

**OpenAI:**
```yaml
provider:
  type: openai
  api_key_env: OPENAI_API_KEY
  model: "gpt-4o"
```

**Ollama (local):**
```yaml
provider:
  type: ollama
  base_url: "http://localhost:11434"
  model: "llama3.2"
  timeout_seconds: 60  # Longer timeout for local inference
```

### Language Module Location

`~/.config/tapir/languages/spanish.yaml`

---

## Build & Run

```bash
# Build
cabal build

# Run tests
cabal test --test-show-details=streaming

# Run application
cabal run tapir

# Clean rebuild
cabal clean && cabal build

# Interactive REPL
cabal repl
```

---

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| **Navigation** | |
| `Tab` / `Shift+Tab` | Cycle modes |
| `PageUp/Down` | Scroll history |
| `Up/Down` | Scroll history (line) |
| **Input** | |
| `Enter` | Send message |
| **Sessions** | |
| `Ctrl+N` | New session |
| `Ctrl+S` | Sessions list |
| `J` / `K` | Navigate list |
| `D` | Delete session |
| **Settings** | |
| `F2` | Settings modal |
| `+` / `-` | Cycle learner level |
| `E` | View system prompt |
| **Cards** | |
| `Ctrl+A` | Show pending card |
| **Modals** | |
| `Esc` | Close modal |
| **Quit** | |
| `Ctrl+Q` | Quit (with confirmation) |
| `Ctrl+C` | Cancel / Quit |

---

## Architecture Notes

### Event Flow

1. User input captured by brick
2. Events dispatched to `handleEvent`
3. LLM requests spawn async thread
4. Structured response received (via tool calls)
5. `EvStructuredResponse` sent via `BChan`
6. `AppEvent` handler updates state
7. brick re-renders UI with `renderStructuredResponse`

### Structured Response System

TAPIR uses OpenAI-compatible tool/function calling to guarantee structured JSON:

1. **Tool Definitions** (`Tapir.Client.LLM.Tools`):
   - Four mode-specific tools: `send_conversation_reply`, `submit_correction`, `submit_translation`, `create_flashcard`
   - Each tool has JSON Schema for parameters
   - `tfStrict = True` enforces schema validation

2. **Request Building** (`Tapir.Client.LLM.Request`):
   - `buildRequestWithTools` adds tool definitions
   - `tool_choice` forces specific tool usage
   - `stream = False` (tool calls don't stream well)

3. **Response Parsing** (`Tapir.Client.LLM.Response`):
   - Parse `tool_calls` from LLM response
   - Extract JSON arguments
   - Parse into `StructuredResponse` sum type

4. **Rendering** (`Tapir.UI.Structured`):
   - Mode-specific rendering functions
   - Sectioned display with colors
   - Conversation: inline corrections, vocab highlights, grammar tips
   - Correction: original/corrected, detailed corrections, encouragement
   - Translation: source/target, alternatives, formality, notes
   - Card: front/back, examples, pronunciation, mnemonics

5. **Storage**:
   - Only plain text stored to database (`responseToText`)
   - Structured data is transient (`asPendingStructured`)

### Key Types

- `AppState` - Full application state with lenses
- `TapirEvent` - Custom events (stream chunks, responses, etc.)
- `Name` - Resource names for focus/viewport management
- `LLMClient` - Abstract interface for LLM providers

### Important Patterns

- Use `BChan` for async -> UI communication
- Never update UI from background threads directly
- All state changes via lens operations in `EventM`
- Modals overlay main UI via layered rendering

---

## Testing

```bash
# All tests
cabal test

# With output
cabal test --test-show-details=streaming

# Specific module
cabal test --test-option=--match="/Repository/"
```

### Test Coverage
- LLM Types: JSON serialization/parsing
- Config Loader: Prompt interpolation
- Database: Schema, CRUD, transactions, foreign keys

---

## Troubleshooting

### "API key not configured"
- Check `~/.config/tapir/config.yaml` has `provider.api_key` set
- Or set `OPENROUTER_API_KEY` environment variable

### "Language module not found"
- Ensure `~/.config/tapir/languages/spanish.yaml` exists
- Check `active_language` in config matches filename

### "Could not find module 'Brick'"
- Run `cabal build --only-dependencies` first

---

## Reference

### External Resources
- [brick documentation](https://github.com/jtdaugherty/brick)
- [OpenRouter API](https://openrouter.ai/docs)
- [AnkiConnect API](https://foosoft.net/projects/anki-connect/)

---

*Last Updated: January 25, 2026*
