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
- **LLM Client**: OpenRouter integration with streaming support
- **TUI**: brick-based interface with chat, input, status bar, modals
- **Streaming**: Real-time token display via BChan
- **Mode Switching**: All four modes (Chat, Correct, Translate, Card)
- **Session Management**: Create, list, load, delete sessions with message history
- **Settings Modal**: Level cycling, prompt preview, language settings
- **Card Generation**: Robust JSON parsing with markdown fence handling
- **Anki Integration**: Connection checking, note pushing via AnkiConnect
- **Text Wrapping**: Dynamic width calculation for proper message display

### Known Issues

- **Windows terminals**: Must use Windows Terminal/PowerShell (not Git Bash/mintty)
- **Ctrl+, shortcut**: May not work on some terminals; use F2 as alternative

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
│   │   └── Provider.hs       # ProviderType, ProviderConfig
│   ├── Config/
│   │   ├── Types.hs          # AppConfig, UIConfig, etc.
│   │   ├── Loader.hs         # YAML loading, prompt interpolation
│   │   └── Defaults.hs       # Default configuration
│   ├── Client/
│   │   ├── LLM.hs            # Abstract LLM interface
│   │   ├── LLM/
│   │   │   ├── Types.hs      # ChatMessage, ChatRequest, etc.
│   │   │   └── OpenRouter.hs # OpenRouter implementation
│   │   └── Anki.hs           # AnkiConnect client
│   ├── Db/
│   │   ├── Schema.hs         # Database initialization
│   │   └── Repository.hs     # CRUD operations
│   └── UI/
│       ├── Types.hs          # AppState, TapirEvent, Name
│       ├── App.hs            # Main brick app, event handling
│       ├── Attrs.hs          # Color theme attributes
│       ├── Widgets.hs        # Reusable widget helpers
│       ├── Chat.hs           # Chat history display
│       ├── Input.hs          # Text editor widget
│       ├── StatusBar.hs      # Mode tabs, status info
│       └── Modals.hs         # Help, Settings, Sessions dialogs
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
  type: openrouter
  api_key: "your-api-key-here"
  model: "z-ai/glm-4.7"
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

### Language Module Location

`~/.config/tapir/languages/spanish.yaml`

---

## Build & Run

```bash
# Build
cabal build

# Run tests
cabal test --test-show-details=streaming

# Run application (Windows: use Windows Terminal/PowerShell)
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
| `1-4` | Jump to mode |
| `PageUp/Down` | Scroll history |
| **Input** | |
| `Enter` | Send message |
| `?` | Help (when chat focused) |
| **Sessions** | |
| `Ctrl+N` | New session |
| `Ctrl+S` | Sessions list |
| `J` / `K` | Navigate list |
| `D` | Delete session |
| **Settings** | |
| `F2` / `Ctrl+,` | Settings modal |
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
4. Streaming tokens sent via `BChan TapirEvent`
5. `AppEvent` handler updates state
6. brick re-renders UI

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

### "GetConsoleScreenBufferInfo: invalid argument"
- Running in Git Bash/mintty on Windows
- Solution: Use Windows Terminal or PowerShell

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

*Last Updated: January 22, 2026*
