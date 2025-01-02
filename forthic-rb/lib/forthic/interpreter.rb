# frozen_string_literal: true

require_relative 'forthic_error'
require_relative 'tokenizer'
require_relative 'token'
require_relative 'code_location'
require_relative 'positioned_string'
require_relative 'words/word'
require_relative 'words/push_value_word'
require_relative 'words/start_module_word'
require_relative 'words/end_module_word'
require_relative 'words/end_array_word'
require_relative 'words/definition_word'
require_relative 'global_module'

module Forthic
  class Interpreter
    attr_accessor :stack, :global_module, :app_module, :module_stack, :registered_modules,
                  :is_compiling, :should_stop, :is_memo_definition, :cur_definition,
                  :screens, :default_module_flags, :module_flags, :string_location,
                  :word_counts, :is_profiling, :start_profile_time, :timestamps

    def initialize
      @stack = []
      @registered_modules = {}
      @is_compiling = false
      @should_stop = false
      @is_memo_definition = false
      @cur_definition = nil
      @screens = {}
      @default_module_flags = {}
      @module_flags = {}
      @string_location = nil

      @global_module = GlobalModule.new(self)
      @app_module = ForthicModule.new("", self)
      @module_stack = [@app_module]

      @word_counts = {}
      @is_profiling = false
      @start_profile_time = nil
      @timestamps = []
    end

    def halt
      @should_stop = true
    end

    # @return [ForthicModule]
    def get_app_module
      @app_module
    end

    # @return [CodeLocation, nil]
    def get_string_location
      @string_location
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
      @stack = []
      @app_module.variables = {}
      @module_stack = [@app_module]
      @is_compiling = false
      @is_memo_definition = false
      @cur_definition = nil
      @string_location = nil
    end

    # @param [String] screen_name
    # @return [String]
    def get_screen_forthic(screen_name)
      screen = @screens[screen_name]
      raise ForthicError.new("interpreter-199", "Unable to find screen \"#{screen_name}\"", "Hmmm...something went wrong. Please file a ticket if this continues to happen") unless screen
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
        break if token.type == TokenType::EOS || @should_stop
        next if [TokenType::START_DEF, TokenType::END_DEF, TokenType::COMMENT].include?(token.type) || @is_compiling
      end
      true
    # rescue => e
    #   error = ForthicError.new("interpreter-213", "Ran into an error executing this '#{token.string}'", "If there is an unknown error in the stack details, please file a ticket so we can resolve it.", token.location)
    #   error.set_caught_error(e)
    #   raise error
    end

    # @return [ForthicModule]
    def cur_module
      @module_stack.last
    end

    # @param [String] name
    # @return [ForthicModule]
    def find_module(name)
      result = @registered_modules[name]
      raise ForthicError.new("interpreter-236", "Couldn't find '#{name}' module", "This is most likely a typo in your Forthic code. Please check to see if '#{name}' is properly spelled and that you have permission to access it") unless result
      result
    end

    # @param [Object] val
    def stack_push(val)
      @stack.push(val)
    end

    # @return [Object]
    def stack_pop
      raise ForthicError.new("interpreter-251", "Stack underflow", "This happens when we expect something to be on the stack, but it's empty. This is caused by a logical error in the Forthic and can be resolved through debugging.") if @stack.empty?
      result = @stack.pop
      @string_location = result.is_a?(PositionedString) ? result.location : nil
      result.is_a?(PositionedString) ? result.value_of : result
    end

    # @param [ForthicModule] mod
    def module_stack_push(mod)
      @module_stack.push(mod)
    end

    def module_stack_pop
      @module_stack.pop
    end

    # @param [ForthicModule] mod
    def register_module(mod)
      @registered_modules[mod.name] = mod
    end

    # @param [ForthicModule] mod
    def run_module_code(mod)
      module_stack_push(mod)
      run(mod.forthic_code)
      module_stack_pop
    rescue => e
      error = ForthicError.new("interpreter-278", "Something went wrong when running the module #{mod.name}", "TODO: File a ticket")
      error.set_caught_error(e)
      raise error
    end

    # @param [String] name
    # @return [Word, nil]
    def find_word(name)
      result = nil
      @module_stack.reverse_each do |m|
        result = m.find_word(name)
        break if result
      end
      result ||= @global_module.find_word(name)
      result
    end

    def start_profiling
      @is_profiling = true
      @timestamps = []
      @start_profile_time = Time.now
      add_timestamp("START")
      @word_counts = {}
    end

    # @param [Word] word
    def count_word(word)
      return unless @is_profiling
      @word_counts[word.name] ||= 0
      @word_counts[word.name] += 1
    end

    def stop_profiling
      add_timestamp("END")
      @is_profiling = false
    end

    # @param [String] label
    def add_timestamp(label)
      return unless @is_profiling
      timestamp = { label: label, time_ms: (Time.now - @start_profile_time) * 1000 }
      @timestamps.push(timestamp)
    end

    # @return [Array<Hash>]
    def word_histogram
      @word_counts.map { |name, count| { word: name, count: count } }.sort_by { |item| -item[:count] }
    end

    # @return [Array<Hash>]
    def profile_timestamps
      @timestamps
    end

    # @param [Token] token
    def handle_token(token)
      case token.type
      when TokenType::STRING then handle_string_token(token)
      when TokenType::COMMENT then handle_comment_token(token)
      when TokenType::START_ARRAY then handle_start_array_token(token)
      when TokenType::END_ARRAY then handle_end_array_token(token)
      when TokenType::START_MODULE then handle_start_module_token(token)
      when TokenType::END_MODULE then handle_end_module_token(token)
      when TokenType::START_DEF then handle_start_definition_token(token)
      when TokenType::START_MEMO then handle_start_memo_token(token)
      when TokenType::END_DEF then handle_end_definition_token(token)
      when TokenType::WORD then handle_word_token(token)
      when TokenType::EOS then return
      else
        raise ForthicError.new("interpreter-362", "Hmmm...the interpreter doesn't know what to make of '#{token.string}'", "This is most likely caused by a typo in the Forthic code and can be resolved by debugging.", token.location)
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
      @cur_definition.add_word(word) if @is_compiling
      count_word(word)
      word.execute(self)
    end

    # @param [Token] _token
    def handle_end_module_token(_token)
      word = EndModuleWord.new
      @cur_definition.add_word(word) if @is_compiling
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
      raise ForthicError.new("interpreter-407", "A definition was started while an existing definition was not ended", "This is probably caused by a missing semicolon. To resolve, ensure that all word definitions end with semicolons.", token.location) if @is_compiling
      @cur_definition = DefinitionWord.new(token.string)
      @is_compiling = true
      @is_memo_definition = false
    end

    # @param [Token] token
    def handle_start_memo_token(token)
      raise ForthicError.new("interpreter-420", "A memo definition was started while an existing definition was not ended", "This is probably caused by a missing semicolon. To resolve, ensure that all word definitions end with semicolons.", token.location) if @is_compiling
      @cur_definition = DefinitionWord.new(token.string)
      @is_compiling = true
      @is_memo_definition = true
    end

    # @param [Token] token
    def handle_end_definition_token(token)
      raise ForthicError.new("interpreter-433", "A definition was ended when one hadn't been started yet", "This is probably caused by an extra semicolon. To resolve, ensure that there are no spurious semicolons in the Forthic code.", token.location) unless @is_compiling
      raise ForthicError.new("interpreter-440", "Cannot finish definition because there is no current definition", "Please file a ticket", token.location) unless @cur_definition
      if @is_memo_definition
        cur_module.add_memo_words(@cur_definition)
      else
        cur_module.add_word(@cur_definition)
      end
      @is_compiling = false
    end

    # @param [Token] token
    def handle_word_token(token)
      word = find_word(token.string)
      raise ForthicError.new("interpreter-458", "Could not find word: #{token.string}", "Check to see if you have a typo in your word or the definition of that word", token.location) unless word
      handle_word(word, token.location)
    end

    # @param [Word] word
    # @param [CodeLocation, nil] location
    def handle_word(word, location = nil)
      if @is_compiling
        word.set_location(location)
        @cur_definition.add_word(word)
      else
        count_word(word)
        word.execute(self)
      end
    end
  end
end