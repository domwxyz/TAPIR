# TAPIR - Translation API Router

**Language-agnostic terminal-based language learning assistant**

## What is TAPIR?

A keyboard-driven TUI (Terminal User Interface) for language learning with:

- **Conversational practice** - Chat with an LLM in your target language with inline corrections
- **Grammar correction** - Get detailed corrections with explanations and severity levels
- **Translation** - Bidirectional translation with cultural notes and alternatives
- **Anki flashcard generation** - Create rich flashcards with examples, pronunciation, and mnemonics

### Key Features

- **Structured responses with color-coded sections**: Tool calling ensures every response is properly formatted with distinct sections for corrections, vocabulary, grammar tips, and more
- **Beautiful terminal UI**: Organized display with syntax highlighting, making it easy to scan corrections, vocabulary, and explanations at a glance
- **Language-agnostic design**: All language-specific logic lives in YAML configs - easily add new languages
- **Command menu**: Ctrl+P for quick access to all commands
- **Local persistence**: SQLite stores your learning history with automatic backups
- **Privacy-focused**: Works with OpenRouter, OpenAI, or local Ollama
- **Full session management**: Create, list, load, delete sessions with complete message history
- **Mode-specific prompts**: Each mode uses its own tailored system prompt with variable interpolation
- **Adjustable difficulty**: Change CEFR level (A1-C2) on the fly from settings
- **Cross-platform**: Works on Linux and Windows

## Requirements

- **GHC** 9.6.3+ (tested with 9.8.2, 9.10.1)
- **Cabal** 3.10+
- **LLM API access**: OpenRouter, OpenAI, or local Ollama
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

### Easy Installation (Linux/macOS)
```bash
# Install to ~/.local/bin/tapir
./install.sh

# Uninstall
./install.sh uninstall
```

The install script:
- Builds an optimized binary
- Copies it to `~/.local/bin/tapir`
- Sets up config directory structure
- Preserves existing configurations

Make sure `~/.local/bin` is in your PATH.

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

4. Set your OpenRouter API key:
```bash
   export OPENROUTER_API_KEY="sk-or-v1-..."
```
   
   Or add it directly to `config.yaml`:
```yaml
   provider:
     api_key: "sk-or-v1-..."
```

### Advanced Configuration

The `config.yaml` supports:
- **Rate limiting**: Control requests per minute
- **Database backups**: Automatic backup rotation
- **Logging**: Configurable log levels and rotation
- **UI theming**: Dark/light themes
- **Model selection**: Choose from available OpenRouter models

See `config/config.yaml` for all available options.

## Keyboard Shortcuts

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

## Modes

### 1. Chat Mode
Free conversation practice with:
- **Inline corrections**: See errors highlighted as you chat
- **Vocabulary highlights**: 1-2 useful words per exchange with translations and gender
- **Grammar tips**: Contextual grammar notes when relevant
- **Part-of-speech tagging**: Know if words are nouns, verbs, adjectives, etc.

### 2. Correct Mode
Detailed grammar correction with:
- **Side-by-side comparison**: Original vs. corrected text
- **Categorized corrections**: Grammar, spelling, conjugation, accents, word choice
- **Severity levels**: Minor, moderate, or significant errors
- **Detailed explanations**: Understand *why* it was wrong and how to remember
- **Encouragement**: Positive feedback on your progress

### 3. Translate Mode
Professional translation with:
- **Bidirectional**: Spanish↔English or any language pair
- **Formality detection**: Formal, informal, or neutral register
- **Literal meanings**: See word-for-word translations of idioms
- **Cultural notes**: Context about cultural expressions
- **Alternatives**: Multiple translation options for ambiguous phrases

### 4. Card Mode  
Generate rich Anki flashcards with:
- **Front/back**: Target language → Native language
- **Example sentences**: See the word in context
- **Pronunciation**: IPA phonetic transcription
- **Mnemonics**: Memory aids to help retention
- **Related words**: Synonyms, antonyms, word families
- **Usage notes**: Grammar patterns, common collocations
- **Smart tags**: Automatic categorization (noun, verb, A1-C2 level)

## How It Works

### Tool-Based Structured Output

TAPIR uses **OpenAI-compatible tool/function calling** to guarantee structured responses:

1. User sends a message in Spanish (e.g., "Hola como estas")
2. TAPIR calls the LLM with mode-specific tools that define exact JSON schemas
3. LLM responds using the tool, ensuring properly formatted output
4. TAPIR parses the JSON and renders it with color-coded sections

This approach eliminates:
- ❌ Unparseable responses
- ❌ Inconsistent formatting
- ❌ Missing fields
- ❌ Hallucinated structure

And provides:
- ✅ Guaranteed JSON parsing
- ✅ Type-safe response handling
- ✅ Beautiful, consistent UI rendering
- ✅ Reliable data extraction for database storage

### Prompt Templating

Language modules support variable interpolation in prompts:
```yaml
system_prompt: |
  You are a {{language}} learning companion for {{level}} learners.
  The learner speaks {{native_language}} natively.
  Use {{variant}} {{language}} dialect.
```

Variables automatically filled:
- `{{language}}` → "Spanish"
- `{{level}}` → "A1" (or current learner level)
- `{{native_language}}` → "English"
- `{{variant}}` → "Latin American"
- `{{level_description}}` → "beginner"

## Database & Persistence

All data is stored locally in SQLite (`~/.local/share/tapir/tapir.db`):

- **Sessions**: Conversation sessions with metadata
- **Messages**: Full message history with timestamps, models, token counts
- **Cards**: Generated flashcards with push status
- **Automatic backups**: Configurable backup rotation
- **Views**: Pre-computed session statistics and recent sessions

Schema supports:
- Foreign key cascades (delete session → delete messages)
- Full-text search (future feature)
- Metadata tracking (app version, schema version)

## Project Structure
```
TAPIR/
├── app/                    # Executable entry point
├── src/Tapir/             # Library source
│   ├── Core/              # Core utilities
│   │   ├── Constants.hs   # API endpoints, provider names
│   │   ├── Error.hs       # Safe list operations, error handling
│   │   └── Logging.hs     # Logging placeholder
│   ├── Types/             # Core domain types
│   │   ├── Mode.hs        # Learning modes
│   │   ├── Language.hs    # Language module types
│   │   ├── Provider.hs    # LLM provider config
│   │   └── Response.hs    # Structured response types
│   ├── Config/            # Configuration system
│   │   ├── Types.hs       # Config types
│   │   ├── Loader.hs      # YAML loading & validation
│   │   └── Defaults.hs    # Default configurations
│   ├── Service/           # Business logic layer
│   │   ├── LLM.hs         # LLM request orchestration
│   │   ├── Card.hs        # Card generation & Anki export
│   │   └── Message.hs     # Message persistence
│   ├── UI/                # Brick TUI components
│   │   ├── App.hs         # Main app definition
│   │   ├── Types.hs       # UI state types
│   │   ├── Draw.hs        # Render UI to screen
│   │   ├── Event.hs       # Main event dispatcher
│   │   ├── Event/         # Event handlers
│   │   │   ├── Main.hs        # Keyboard input
│   │   │   ├── Custom.hs      # Async responses
│   │   │   ├── Message.hs     # Message events
│   │   │   ├── Session.hs     # Session events
│   │   │   ├── Card.hs        # Card events
│   │   │   ├── Modal.hs       # Modal events
│   │   │   └── Settings.hs    # Settings events
│   │   ├── Attrs.hs       # Color schemes
│   │   ├── Chat.hs        # Chat history display
│   │   ├── Input.hs       # Text input widget
│   │   ├── StatusBar.hs   # Bottom status bar
│   │   ├── Modals.hs      # Modal dialogs
│   │   ├── Structured.hs  # Structured response rendering
│   │   ├── Widgets.hs     # Reusable widgets
│   │   └── Command.hs     # Command parsing/execution
│   ├── Client/            # LLM & Anki clients
│   │   ├── LLM/           # LLM abstraction
│   │   │   ├── Base.hs        # Generic OpenAI-compatible client
│   │   │   ├── Types.hs       # Request/response types
│   │   │   ├── OpenRouter.hs  # OpenRouter implementation
│   │   │   ├── OpenAI.hs      # OpenAI implementation
│   │   │   ├── Ollama.hs      # Ollama implementation (local)
│   │   │   ├── Tools.hs       # Tool/function definitions
│   │   │   ├── Request.hs     # Request building
│   │   │   ├── Response.hs    # Response parsing
│   │   │   └── SSE.hs         # Server-Sent Events parser
│   │   └── Anki.hs        # AnkiConnect integration
│   ├── Db/                # Database layer
│   │   ├── Schema.hs      # DDL & migrations
│   │   ├── Repository.hs  # CRUD operations
│   │   └── Instances.hs   # SQLite type instances
│   └── Types.hs           # Re-export hub
├── test/                  # Test suite
├── config/                # Default configuration
├── languages/             # Language module templates
└── install.sh             # Installation script
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

# Format code (if using ormolu)
find src -name "*.hs" -exec ormolu --mode inplace {} \;
```

## Architecture

TAPIR follows a clean architecture with:

- **Types layer**: Pure domain types with no dependencies
- **Config layer**: YAML parsing, validation, and prompt templating
- **Client layer**: External service integrations (LLM, Anki) with proper abstractions
- **Database layer**: Persistence with repository pattern and type-safe queries
- **UI layer**: Brick TUI with event-driven updates and functional composition

### Technology Stack

- **brick 2.4+** for TUI framework
- **vty-crossplatform** for cross-platform terminal handling
- **http-client** for HTTP with streaming support
- **sqlite-simple** for persistence with type-safe queries
- **yaml/aeson** for configuration and JSON handling
- **microlens** for functional record updates
- **stm** for concurrent state management

## Current Limitations

- **Provider support**: OpenRouter, OpenAI, and Ollama are implemented
  - Anthropic support is planned
- **Language modules**: Only Spanish is included
  - Community contributions for other languages welcome!

## Creating Language Modules

TAPIR makes it easy to add new languages. Copy `languages/spanish.yaml` and modify:
```yaml
language:
  id: french
  name: "French"
  native_name: "Français"
  code: fr
  variant: france  # or: québec, neutral

modes:
  conversation:
    system_prompt: |
      Vous êtes un compagnon d'apprentissage du {{language}}...
```

All prompts support templating with `{{language}}`, `{{level}}`, `{{native_language}}`, etc.

## Documentation

For AI agents working on this codebase, see:

- `CLAUDE.md` - Development guide and current state
- `AGENTS.md` - Comprehensive AI agent reference

## Troubleshooting

### "API key not configured"
Set `OPENROUTER_API_KEY` environment variable or add `api_key` to config.yaml.

### "Language module not found"
Ensure `~/.config/tapir/languages/spanish.yaml` exists and `active_language` in config.yaml matches the filename (without `.yaml`).

### "Anki not running" or card push fails
1. Start Anki desktop application
2. Ensure AnkiConnect plugin is installed (Tools → Add-ons → Get Add-ons → Code: 2055492159)
3. Check that AnkiConnect is listening on localhost:8765

### Database errors
The database will auto-initialize. If corrupted:
```bash
rm ~/.local/share/tapir/tapir.db
# Restart TAPIR to recreate
```

## Contributing

Contributions welcome! Areas of interest:

- **Language modules**: Add support for more languages
- **Provider implementations**: Anthropic
- **UI improvements**: Themes, layouts, widgets
- **Features**: Spaced repetition, grammar drills, pronunciation practice
- **Documentation**: Tutorials, screencasts, guides

## License

MIT License - See LICENSE file for details

---

**Last Updated**: January 25, 2026
**Tested On**: Linux (Debian 13), Windows 10
