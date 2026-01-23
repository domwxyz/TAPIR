# TAPIR Development Guide

**Project:** TAPIR (Translation API Router)
**Type:** Haskell + brick TUI for language learning
**Location:** ~/dev/TAPIR/
**Status:** Phase 5 - Integration (In Progress)

---

## Current State

### What's Working

- **Configuration**: YAML config loading from `~/.config/tapir/config.yaml`
- **Language Modules**: Spanish module loads from `~/.config/tapir/languages/spanish.yaml`
- **Database**: SQLite with full schema, repository pattern, tested
- **LLM Client**: OpenRouter integration with streaming support
- **TUI**: brick-based interface with chat, input, status bar, modals
- **Streaming**: Real-time token display via BChan
- **Mode Switching**: All four modes accessible (Chat, Correct, Translate, Card)

### Known Issues

- **Text wrapping**: Long messages don't wrap in the viewport
- **Windows terminals**: Must use Windows Terminal/PowerShell (not Git Bash/mintty)
- **Settings modal**: Placeholder only, edit config.yaml directly
- **Message persistence**: Not yet wired to database
- **Session management**: Basic implementation, needs completion

---

## Project Structure

```
TAPIR/
├── app/Main.hs                 # Entry point
├── src/Tapir/
│   ├── Types.hs               # Re-exports all types
│   ├── Types/                 # Domain types
│   │   ├── Core.hs           # Mode, Role, TapirError
│   │   ├── Language.hs       # LanguageInfo, LanguageModule
│   │   ├── Provider.hs       # ProviderType, ProviderConfig
│   │   ├── Message.hs        # Message type
│   │   ├── Session.hs        # Session, SessionSummary
│   │   └── Card.hs           # Card type for Anki
│   ├── Config/
│   │   ├── Types.hs          # AppConfig, UIConfig, etc.
│   │   ├── Loader.hs         # YAML loading, prompt interpolation
│   │   └── Defaults.hs       # Default configuration
│   ├── Client/
│   │   ├── LLM.hs            # Abstract LLM interface
│   │   └── LLM/
│   │       ├── Types.hs      # ChatMessage, ChatRequest, etc.
│   │       └── OpenRouter.hs # OpenRouter implementation
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
├── languages/                 # Template language modules
└── impl docs/                 # Original specifications
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

database:
  path: "~/.local/share/tapir/tapir.db"
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
```

---

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Enter` | Send message |
| `Tab` / `Shift+Tab` | Cycle modes |
| `1-4` | Jump to mode |
| `Ctrl+P` | Command menu |
| `F1` | Help modal |
| `F2` | Settings modal |
| `Ctrl+N` | New session |
| `Ctrl+S` | Sessions list |
| `Ctrl+A` | Show pending card |
| `Ctrl+Q` | Quit (with confirmation) |
| `PageUp/Down` | Scroll history |
| `Esc` | Close modal |

---

## Remaining Work

### Phase 5: Integration (Current)
- [ ] Wire message persistence to database
- [ ] Implement session save/load
- [ ] Fix text wrapping in chat viewport
- [ ] Add system prompt injection per mode

### Phase 6: Polish
- [ ] Implement settings modal functionality
- [ ] Card generation and preview
- [ ] Anki integration via AnkiConnect
- [ ] Error display improvements
- [ ] UI theming options

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

### "infinite-height widget in viewport"
- Using `txtWrap` in a viewport
- Solution: Use `txt` instead (wrapping needs manual implementation)

---

## Reference

### Implementation Documents (in `impl docs/`)
- `TAPIR_impl_Specification.md` - Full architecture spec
- `TAPIR_impl_Addendum.md` - Build config, schemas, API details
- `TAPIR_impl_Checklist.md` - Implementation roadmap
- `TAPIR_Scaffolding_Guide.md` - Project structure

### External Resources
- [brick documentation](https://github.com/jtdaugherty/brick)
- [OpenRouter API](https://openrouter.ai/docs)
- [AnkiConnect API](https://foosoft.net/projects/anki-connect/)

---

*Last Updated: January 22, 2026*
