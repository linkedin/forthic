# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Testing
- `rake test` - Run all tests using Minitest
- `rake test TEST=test/test_specific_file.rb` - Run a specific test file
- `bundle exec guard` - Start Guard for continuous testing (watches file changes)
- `rake guard` - Alternative command to start Guard

### Code Quality
- `rake standard` - Run StandardRB linter for code formatting and style
- `rake standard:fix` - Auto-fix StandardRB issues where possible
- `rake` - Run default task (both tests and linting)

### Development
- `bundle install` - Install gem dependencies
- `bundle exec rake build` - Build the gem
- `bundle exec rake install` - Install the gem locally
- `bundle exec rake release` - Release the gem (requires proper credentials)

## Architecture Overview

This is a Ruby implementation of the Forthic programming language - a stack-based language inspired by Forth. The codebase follows a clean modular architecture:

### Core Components

**Interpreter (`lib/forthic/interpreter.rb`)**: The main execution engine that:
- Manages the execution stack and module stack
- Handles tokenization and parsing of Forthic code
- Provides word lookup and execution
- Supports profiling and debugging capabilities
- Manages compilation of word definitions

**Tokenizer (`lib/forthic/tokenizer.rb`)**: Lexical analyzer that breaks Forthic code into tokens (strings, words, arrays, modules, definitions, comments).

**Word System (`lib/forthic/words/`)**: Polymorphic word execution system where each word type implements an `execute` method:
- `Word` - Base class for all executable words
- `PushValueWord` - Pushes values onto the stack
- `DefinitionWord` - User-defined words compiled from Forthic code
- `EndArrayWord` - Handles array construction
- `ModuleWord` variants - Handle module operations and memoization
- `StartModuleWord`/`EndModuleWord` - Module boundary management

**Module System**: 
- `GlobalModule` - Built-in words and core functionality
- `ForthicModule` - User-defined modules with isolated namespaces
- Module stack for hierarchical word resolution

### Key Design Patterns

- **Stack-based execution**: All operations work with a central execution stack
- **Autoloading**: Classes are autoloaded for better startup performance
- **Error handling**: Custom `ForthicError` with location tracking for debugging
- **Compilation**: Words can be compiled into definition words for reuse
- **Memoization**: Special memo words for caching expensive computations

### Testing Strategy

Uses Minitest for testing with:
- `test_helper.rb` sets up Minitest with SpecReporter for better output
- Guard for continuous testing during development
- Tests cover tokenizer, interpreter, and module functionality
- StandardRB enforces consistent code style

### File Organization

- `lib/forthic.rb` - Main entry point with autoload declarations
- `lib/forthic/` - Core implementation files
- `lib/forthic/words/` - Word implementation classes
- `test/` - Test files following Minitest conventions
- `sig/` - RBS type signatures for static analysis