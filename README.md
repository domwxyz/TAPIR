# TAPIR - Translation API Router

**Language-agnostic terminal-based language learning assistant**

## What is TAPIR?

A keyboard-driven TUI (Terminal User Interface) for language learning with:

- **Conversational practice** - Chat with an LLM in your target language
- **Grammar correction** - Get detailed corrections and explanations
- **Translation** - Bidirectional translation with nuance preservation
- **Anki flashcard generation** - Direct integration with Anki via AnkiConnect

### Key Features

- **Language-agnostic design**: All language-specific logic lives in YAML configs
- **Command menu**: Ctrl+P for quick access to all commands
- **Streaming responses**: Real-time token display for natural interaction
- **Local persistence**: SQLite stores your learning history
- **Privacy-focused**: Works with OpenRouter, Anthropic, OpenAI, or local Ollama
- **Full session management**: Create, list, load, delete sessions with message history
- **Mode-specific prompts**: Each mode uses its own tailored system prompt
- **Adjustable difficulty**: Change CEFR level (A1-C2) from settings

## Requirements

- **GHC** 9.6.3+ (tested with 9.8.2)
- **Cabal** 3.10+
- **OpenRouter API key** (or other LLM provider)
- **Anki** (optional): For flashcard integration, requires [AnkiConnect](https://ankiweb.net/shared/info/2055492159) plugin

## Quick Start

```bash
# Clone the repository
git clone https://github.com/domwxyz/TAPIR.git
cd TAPIR

# Build the project
cabal update
cabal build

# Run tests
cabal test

# Run the application
cabal run tapir
```

## Configuration

### Initial Setup

1. Create the config directory:
   ```bash
   mkdir -p ~/.config/tapir/languages
   ```

2. Copy `./config/config.yaml` to `~/.config/tapir/config.yaml`:
   ```bash
   cp config/config.yaml ~/.config/tapir/
   ```

3. Copy the Spanish language module:
   ```bash
   cp languages/spanish.yaml ~/.config/tapir/languages/
   ```

4. Set your API key (config file or environment variable):
   ```bash
   export OPENROUTER_API_KEY="sk-or-v1-..."
   ```

 ### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| | **Main Interface** | |
| `Ctrl+P` | Command menu |
| `Ctrl+N` | New session |
| `Ctrl+S` | Session list |
| `Ctrl+A` | Show pending card |
| `Ctrl+Q` | Quit (with confirmation) |
| `Ctrl+C` | Cancel request / Quit |
| `F1` | Help |
| `F2` | Settings |
| | **Modes** | |
| `Tab` / `Shift+Tab` | Next / Previous mode |
| `1` / `2` / `3` / `4` | Jump to Chat/Correct/Translate/Card |
| `PageUp` / `PageDown` | Scroll history |
| `Enter` | Send message |
| | **Modal Navigation** | |
| `Esc` | Close modal |
| `j` / `k` or `↑` / `↓` | Navigate list (command menu, sessions) |
| `Enter` | Select / Execute |
| | **Command Menu** | |
| | Lists all available commands (Ctrl+P) |
| | **Settings Modal** | |
| `+` / `-` | Cycle learner level |
| `E` | View system prompt |
| `S` | Save settings |
| `R` | Reload config |
| | **Session List** | |
| `D` | Delete session |
| `N` | New session |
| | **Card Preview** | |
| `Enter` | Push to Anki |
| `D` | Discard card |

### Modes

1. **Chat** (1) - Free conversation practice in your target language
2. **Correct** (2) - Grammar correction with detailed explanations
3. **Translate** (3) - Bidirectional translation between languages
4. **Card** (4) - Generate Anki flashcards from vocabulary

Each mode has a dedicated system prompt that instructs the LLM how to respond appropriately.

Press `Tab` to cycle through modes, or use `1-4` to jump directly.

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
└── languages/             # Language module templates
```

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

For AI agents working on this codebase, see:

- `CLAUDE.md` - Development guide and current state
- `AGENTS.md` - Comprehensive AI agent reference

## Troubleshooting

### "API key not configured"
Set `OPENROUTER_API_KEY` environment variable or add `api_key` to config.yaml.

### "Language module not found"
Ensure `~/.config/tapir/languages/spanish.yaml` exists and `active_language` matches.

### "Anki not running" or card push fails
Start Anki and ensure AnkiConnect plugin is installed (add-on code: 2055492159).

## License

MIT License - See LICENSE file for details

---

**Project Status**: Complete and Functional
**Last Updated**: January 22, 2026

