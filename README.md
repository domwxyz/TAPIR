# TAPIR - Translation API Router

**Language-agnostic terminal-based language learning assistant**

## Status

**Phase 5 Complete: Integration** (January 22, 2026)

- Core TUI implemented and functional
- LLM streaming working with OpenRouter
- Database persistence operational
- Session management fully functional
- Settings modal with level cycling and prompt preview
- Card generation with robust JSON parsing
- Anki integration complete

## What is TAPIR?

A keyboard-driven TUI (Terminal User Interface) for language learning with:

- **Conversational practice** - Chat with an LLM in your target language
- **Grammar correction** - Get detailed corrections and explanations
- **Translation** - Bidirectional translation with nuance preservation
- **Anki flashcard generation** - Direct integration with Anki via AnkiConnect

### Key Features

- **Language-agnostic design**: All language-specific logic lives in YAML configs
- **Modal interface**: Vim-like keyboard shortcuts for efficient navigation
- **Streaming responses**: Real-time token display for natural interaction
- **Local persistence**: SQLite stores your learning history
- **Privacy-focused**: Works with OpenRouter, Anthropic, OpenAI, or local Ollama
- **Full session management**: Create, list, load, delete sessions with message history
- **Mode-specific prompts**: Each mode uses its own tailored system prompt

## Requirements

- **GHC** 9.6.3+ (tested with 9.8.2)
- **Cabal** 3.10+
- **OpenRouter API key** (or other LLM provider)
- **Windows**: Requires Windows Terminal, PowerShell, or Command Prompt (not Git Bash/mintty)

## Quick Start

```bash
# Clone the repository
git clone <your-repo-url>
cd TAPIR

# Build the project
cabal update
cabal build

# Run tests
cabal test

# Run the application (use Windows Terminal/PowerShell on Windows)
cabal run tapir
```

## Configuration

### Initial Setup

1. Create the config directory:
   ```bash
   mkdir -p ~/.config/tapir
   ```

2. Create `~/.config/tapir/config.yaml`:
   ```yaml
   active_language: spanish

   provider:
     type: openrouter
     api_key: "your-openrouter-api-key"
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
   ```

3. Ensure the Spanish language module exists at `~/.config/tapir/languages/spanish.yaml`

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| **Navigation** |
| `Tab` / `Shift+Tab` | Switch modes |
| `1` / `2` / `3` / `4` | Jump to mode (Chat/Correct/Translate/Card) |
| `PageUp/Down` | Scroll history |
| **Actions** |
| `Enter` | Send message |
| `?` | Open help (when chat history has focus) |
| **Sessions** |
| `Ctrl+N` | New session |
| `Ctrl+S` | Session list |
| **Settings** |
| `Ctrl+,` or `F2` | Open settings |
| `+` / `-` | Cycle learner level (in settings) |
| `E` | View system prompt for current mode (in settings) |
| **Modals** |
| `Esc` | Close modal |
| `J` / `K` | Navigate session list |
| `D` | Delete session (in session list) |
| `N` | Create new session (in session list) |
| **Quit** |
| `Ctrl+Q` | Quit (with confirmation) |
| `Ctrl+C` | Cancel / Quit |

### Modes

1. **Chat** - Free conversation practice
2. **Correct** - Grammar correction with explanations
3. **Translate** - Bidirectional translation
4. **Card** - Generate Anki flashcards

Each mode has a dedicated system prompt that instructs the LLM how to respond appropriately.

## Project Structure

```
TAPIR/
├── app/                    # Executable entry point
├── src/Tapir/             # Library source
│   ├── Types/             # Core domain types
│   ├── Config/            # Configuration system
│   ├── UI/                # Brick TUI components
│   ├── Client/            # LLM & Anki clients
│   ├── Db/                # Database layer
│   └── Util/              # Utilities
├── test/                  # Test suite
├── languages/             # Language module templates
└── impl docs/             # Implementation specifications
```

## Implementation Progress

### Phase 1: Foundation
- [x] Project scaffolding
- [x] Type system (Mode, Language, Provider, Config)
- [x] YAML configuration loading
- [x] Prompt interpolation
- [x] Spanish language module

### Phase 2: Core Types & Database
- [x] Complete domain types (Message, Session, Card)
- [x] SQLite schema and migrations
- [x] Repository pattern for CRUD operations
- [x] Database tests

### Phase 3: LLM Client
- [x] Abstract LLM client interface
- [x] OpenRouter implementation
- [x] SSE streaming support
- [x] Error handling and rate limiting

### Phase 4: Basic TUI
- [x] Brick application structure
- [x] Chat history viewport
- [x] Multi-line editor
- [x] Status bar with mode tabs
- [x] Modal dialogs (Help, Settings, Sessions)
- [x] Keyboard navigation

### Phase 5: Integration (Complete)
- [x] Wire LLM client to event loop
- [x] Async streaming via BChan
- [x] Message persistence to database
- [x] Session management (create, list, load, delete)
- [x] System prompt injection per mode
- [x] Text wrapping with dynamic width

### Phase 6: Features & Polish (Complete)
- [x] Full settings modal functionality (level cycling, prompt preview)
- [x] Card generation mode with robust JSON parsing
- [x] Anki integration (connection check, note push)
- [x] UI polish (compact layout, dark theme)
- [x] Error display (error modal, status bar notifications)

## Known Issues

**None currently** - All reported issues have been resolved:
- ✅ Text wrapping now works correctly with dynamic width calculation
- ✅ Question mark (`?`) input works in text editor
- ✅ Settings modal fully functional with level adjustment
- ✅ Session management fully implemented (delete, new, load)
- ✅ Card generation with robust JSON parsing and markdown fence handling

**Note**: On some terminals, `Ctrl+,` may not work reliably for opening settings. Use `F2` as an alternative shortcut.

## Development

```bash
# Interactive REPL
cabal repl

# Watch mode (if using ghcid)
ghcid --command="cabal repl"

# Clean build
cabal clean && cabal build

# Run tests with details
cabal test --test-show-details=streaming
```

## Architecture

TAPIR follows a clean architecture with:

- **Types layer**: Pure domain types with no dependencies
- **Config layer**: YAML parsing and validation
- **Client layer**: External service integrations (LLM, Anki)
- **Database layer**: Persistence with repository pattern
- **UI layer**: Brick TUI with event-driven updates

### Technology Stack

- **brick 2.4+** for TUI
- **vty-crossplatform** for terminal handling
- **http-client** for HTTP with streaming
- **sqlite-simple** for persistence
- **yaml/aeson** for configuration

## Documentation

See `impl docs/` for complete technical specifications:

- `TAPIR_impl_Specification.md` - Complete architecture and design
- `TAPIR_impl_Addendum.md` - Build config, schemas, API specs
- `TAPIR_impl_Checklist.md` - Implementation roadmap
- `TAPIR_Scaffolding_Guide.md` - Project structure guide

## License

MIT License - See LICENSE file for details

---

**Project Status**: Complete and Functional
**Last Updated**: January 22, 2026
**Phase**: 6/6 Complete
