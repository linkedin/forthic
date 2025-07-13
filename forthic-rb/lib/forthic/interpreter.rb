# frozen_string_literal: true

require_relative "forthic_error"
require_relative "tokenizer"
require_relative "token"
require_relative "code_location"
require_relative "positioned_string"
require_relative "words/word"
require_relative "words/push_value_word"
require_relative "words/start_module_word"
require_relative "words/end_module_word"
require_relative "words/end_array_word"
require_relative "words/definition_word"
require_relative "global_module"

module Forthic
  # Error codes used throughout the interpreter
  module ErrorCodes
    SCREEN_NOT_FOUND = "screen-not-found"
    EXECUTION_ERROR = "execution-error"
    MODULE_NOT_FOUND = "module-not-found"
    STACK_UNDERFLOW = "stack-underflow"
    MODULE_EXECUTION_ERROR = "module-execution-error"
    UNKNOWN_TOKEN = "unknown-token"
    NESTED_DEFINITION = "nested-definition"
    NESTED_MEMO_DEFINITION = "nested-memo-definition"
    DEFINITION_WITHOUT_START = "definition-without-start"
    MISSING_DEFINITION = "missing-definition"
    WORD_NOT_FOUND = "word-not-found"
  end

  # Manages execution state for the interpreter
  class ExecutionState
    attr_accessor :stack, :module_stack, :is_compiling, :should_stop,
      :is_memo_definition, :cur_definition, :string_location

    def initialize(app_module)
      @stack = []
      @module_stack = [app_module]
      @is_compiling = false
      @should_stop = false
      @is_memo_definition = false
      @cur_definition = nil
      @string_location = nil
    end

    def reset(app_module)
      @stack = []
      @module_stack = [app_module]
      @is_compiling = false
      @is_memo_definition = false
      @cur_definition = nil
      @string_location = nil
    end
  end

  # Manages profiling state and operations
  class ProfilingState
    attr_reader :word_counts, :is_profiling, :start_profile_time, :timestamps

    def initialize
      @word_counts = {}
      @is_profiling = false
      @start_profile_time = nil
      @timestamps = []
    end

    def start_profiling
      @is_profiling = true
      @timestamps = []
      @start_profile_time = Time.now
      add_timestamp("START")
      @word_counts = {}
    end

    def stop_profiling
      add_timestamp("END")
      @is_profiling = false
    end

    def count_word(word)
      return unless @is_profiling
      @word_counts[word.name] ||= 0
      @word_counts[word.name] += 1
    end

    def add_timestamp(label)
      return unless @is_profiling
      timestamp = {label: label, time_ms: (Time.now - @start_profile_time) * 1000}
      @timestamps.push(timestamp)
    end

    def word_histogram
      @word_counts.map { |name, count| {word: name, count: count} }.sort_by { |item| -item[:count] }
    end
  end

  class Interpreter
    # Core interpreter components
    attr_reader :global_module, :app_module, :registered_modules
    # Screen and module management
    attr_accessor :screens, :default_module_flags, :module_flags
    # State objects
    attr_reader :execution_state, :profiling_state

    # Token handler lookup table
    TOKEN_HANDLERS = {
      TokenType::STRING => :handle_string_token,
      TokenType::COMMENT => :handle_comment_token,
      TokenType::START_ARRAY => :handle_start_array_token,
      TokenType::END_ARRAY => :handle_end_array_token,
      TokenType::START_MODULE => :handle_start_module_token,
      TokenType::END_MODULE => :handle_end_module_token,
      TokenType::START_DEF => :handle_start_definition_token,
      TokenType::START_MEMO => :handle_start_memo_token,
      TokenType::END_DEF => :handle_end_definition_token,
      TokenType::WORD => :handle_word_token
    }.freeze

    def initialize
      @registered_modules = {}
      @screens = {}
      @default_module_flags = {}
      @module_flags = {}

      @global_module = GlobalModule.new(self)
      @app_module = ForthicModule.new("", self)

      @execution_state = ExecutionState.new(@app_module)
      @profiling_state = ProfilingState.new
    end

    def halt
      @execution_state.should_stop = true
    end

    # @return [ForthicModule]
    def get_app_module
      @app_module
    end

    # @return [CodeLocation, nil]
    def get_string_location
      @execution_state.string_location
    end

    # Delegation methods for execution state
    def stack
      @execution_state.stack
    end

    def stack=(new_stack)
      @execution_state.stack = new_stack
    end

    def module_stack
      @execution_state.module_stack
    end

    def is_compiling
      @execution_state.is_compiling
    end

    def cur_definition
      @execution_state.cur_definition
    end

    # @param [String] module_id
    # @param [Hash] flags
    def set_flags(module_id, flags)
      @default_module_flags[module_id] = flags
      @module_flags[module_id] = flags
    end

    # @param [String] module_id
    # @return [Hash]
    def get_flags(module_id)
      module_flags = @module_flags[module_id] || {}
      result = module_flags.dup
      @module_flags[module_id] = @default_module_flags[module_id].dup
      result
    end

    # @param [String] module_id
    # @param [Hash] flags
    def modify_flags(module_id, flags)
      module_flags = @module_flags[module_id] || {}
      @module_flags[module_id] = module_flags.merge(flags)
    end

    def reset
      @app_module.variables = {}
      @execution_state.reset(@app_module)
    end

    # @param [String] screen_name
    # @return [String]
    def get_screen_forthic(screen_name)
      screen = @screens[screen_name]
      raise ForthicError.new(ErrorCodes::SCREEN_NOT_FOUND, "Unable to find screen \"#{screen_name}\"", "Screen not found. Check the screen name for typos or ensure it has been properly registered.") unless screen
      screen
    end

    # @param [String] string
    # @param [CodeLocation, nil] reference_location
    # @return [Boolean]
    def run(string, reference_location = nil)
      tokenizer = Tokenizer.new(string, reference_location)
      run_with_tokenizer(tokenizer)
    end

    # @param [Tokenizer] tokenizer
    # @return [Boolean]
    def run_with_tokenizer(tokenizer)
      token = nil
      loop do
        token = tokenizer.next_token
        handle_token(token)
        break if token.type == TokenType::EOS || @execution_state.should_stop
        next if [TokenType::START_DEF, TokenType::END_DEF, TokenType::COMMENT].include?(token.type) || @execution_state.is_compiling
      end
      true
    rescue => e
      error = ForthicError.new(ErrorCodes::EXECUTION_ERROR, "Error executing token '#{token&.string}'", "An unexpected error occurred during execution. Check the token syntax and try again.", token&.location)
      error.set_caught_error(e)
      raise error
    end

    # @return [ForthicModule]
    def cur_module
      @execution_state.module_stack.last
    end

    # @param [String] name
    # @return [ForthicModule]
    def find_module(name)
      raise ArgumentError, "Module name cannot be nil" if name.nil?
      raise ArgumentError, "Module name cannot be empty" if name.empty?

      result = @registered_modules[name]
      raise ForthicError.new(ErrorCodes::MODULE_NOT_FOUND, "Module '#{name}' not found", "Check the module name for typos and ensure it has been properly registered.") unless result
      result
    end

    # @param [Object] val
    def stack_push(val)
      @execution_state.stack.push(val)
    end

    # @return [Object]
    def stack_pop
      raise ForthicError.new(ErrorCodes::STACK_UNDERFLOW, "Stack underflow", "Attempted to pop from an empty stack. This indicates a logical error in the Forthic code.") if @execution_state.stack.empty?
      result = @execution_state.stack.pop
      @execution_state.string_location = result.is_a?(PositionedString) ? result.location : nil
      result.is_a?(PositionedString) ? result.value_of : result
    end

    # @param [ForthicModule] mod
    def module_stack_push(mod)
      raise ArgumentError, "Module cannot be nil" if mod.nil?
      @execution_state.module_stack.push(mod)
    end

    def module_stack_pop
      @execution_state.module_stack.pop
    end

    # @param [ForthicModule] mod
    def register_module(mod)
      raise ArgumentError, "Module cannot be nil" if mod.nil?
      raise ArgumentError, "Module must respond to :name" unless mod.respond_to?(:name)
      @registered_modules[mod.name] = mod
    end

    # @param [ForthicModule] mod
    # @param [String] prefix
    def import_module(mod, prefix = "")
      raise ArgumentError, "Module cannot be nil" if mod.nil?
      register_module(mod)
      @app_module.import_module(prefix, mod, self)
    end

    # @param [ForthicModule] mod
    def run_module_code(mod)
      raise ArgumentError, "Module cannot be nil" if mod.nil?
      module_stack_push(mod)
      run(mod.forthic_code)
      module_stack_pop
    rescue => e
      error = ForthicError.new(ErrorCodes::MODULE_EXECUTION_ERROR, "Error executing module '#{mod.name}'", "An error occurred while running the module code. Check the module implementation for syntax errors.")
      error.set_caught_error(e)
      raise error
    end

    # @param [String] name
    # @return [Word, nil]
    def find_word(name)
      result = nil
      @execution_state.module_stack.reverse_each do |m|
        result = m.find_word(name)
        break if result
      end
      result ||= @global_module.find_word(name)
      result
    end

    # Delegation methods for profiling
    def start_profiling
      @profiling_state.start_profiling
    end

    def stop_profiling
      @profiling_state.stop_profiling
    end

    def count_word(word)
      @profiling_state.count_word(word)
    end

    def add_timestamp(label)
      @profiling_state.add_timestamp(label)
    end

    def word_histogram
      @profiling_state.word_histogram
    end

    def profile_timestamps
      @profiling_state.timestamps
    end

    # @param [Token] token
    def handle_token(token)
      return if token.type == TokenType::EOS

      handler = TOKEN_HANDLERS[token.type]
      if handler
        send(handler, token)
      else
        raise ForthicError.new(ErrorCodes::UNKNOWN_TOKEN, "Unknown token type '#{token.string}'", "This token type is not recognized. Check for typos or unsupported syntax.", token.location)
      end
    end

    # @param [Token] token
    def handle_string_token(token)
      value = PositionedString.new(token.string, token.location)
      handle_word(PushValueWord.new("<string>", value))
    end

    # @param [Token] token
    def handle_start_module_token(token)
      word = StartModuleWord.new(token.string)
      @execution_state.cur_definition.add_word(word) if @execution_state.is_compiling
      count_word(word)
      word.execute(self)
    end

    # @param [Token] _token
    def handle_end_module_token(_token)
      word = EndModuleWord.new
      @execution_state.cur_definition.add_word(word) if @execution_state.is_compiling
      count_word(word)
      word.execute(self)
    end

    # @param [Token] token
    def handle_start_array_token(token)
      handle_word(PushValueWord.new("<start_array_token>", token))
    end

    # @param [Token] _token
    def handle_end_array_token(_token)
      handle_word(EndArrayWord.new)
    end

    # @param [Token] _token
    def handle_comment_token(_token)
      # Handle comment token (no-op)
    end

    # @param [Token] token
    def handle_start_definition_token(token)
      raise ForthicError.new(ErrorCodes::NESTED_DEFINITION, "Nested definition not allowed", "A definition was started while another definition is active. Ensure all definitions end with semicolons.", token.location) if @execution_state.is_compiling
      @execution_state.cur_definition = DefinitionWord.new(token.string)
      @execution_state.is_compiling = true
      @execution_state.is_memo_definition = false
    end

    # @param [Token] token
    def handle_start_memo_token(token)
      raise ForthicError.new(ErrorCodes::NESTED_MEMO_DEFINITION, "Nested memo definition not allowed", "A memo definition was started while another definition is active. Ensure all definitions end with semicolons.", token.location) if @execution_state.is_compiling
      @execution_state.cur_definition = DefinitionWord.new(token.string)
      @execution_state.is_compiling = true
      @execution_state.is_memo_definition = true
    end

    # @param [Token] token
    def handle_end_definition_token(token)
      raise ForthicError.new(ErrorCodes::DEFINITION_WITHOUT_START, "Definition ended without start", "A definition was ended when none was active. Check for extra semicolons.", token.location) unless @execution_state.is_compiling
      raise ForthicError.new(ErrorCodes::MISSING_DEFINITION, "No current definition to end", "Internal error: definition state is inconsistent.", token.location) unless @execution_state.cur_definition
      if @execution_state.is_memo_definition
        cur_module.add_memo_words(@execution_state.cur_definition)
      else
        cur_module.add_word(@execution_state.cur_definition)
      end
      @execution_state.is_compiling = false
    end

    # @param [Token] token
    def handle_word_token(token)
      word = find_word(token.string)
      raise ForthicError.new(ErrorCodes::WORD_NOT_FOUND, "Word '#{token.string}' not found", "Check for typos in the word name or ensure the word has been defined.", token.location) unless word
      handle_word(word, token.location)
    end

    # @param [Word] word
    # @param [CodeLocation, nil] location
    def handle_word(word, location = nil)
      if @execution_state.is_compiling
        word.set_location(location)
        @execution_state.cur_definition.add_word(word)
      else
        count_word(word)
        word.execute(self)
      end
    end
  end
end
