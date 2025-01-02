# frozen_string_literal: true

require_relative 'words/word'
require_relative 'words/module_word'
require_relative 'words/module_memo_word'
require_relative 'words/module_memo_bang_word'
require_relative 'words/module_memo_bang_at_word'
require_relative 'words/imported_word'
require_relative 'variable'

module Forthic
  class ForthicModule
    attr_accessor :words, :exportable, :variables, :modules, :module_prefixes, :required_modules, :name, :forthic_code, :module_id

    # @param [String] name
    # @param [Interpreter, nil] interp
    # @param [String] forthic_code
    def initialize(name, interp = nil, forthic_code = "")
      @words = []
      @exportable = []
      @variables = {}
      @modules = {}
      @module_prefixes = {}
      @required_modules = []
      @name = name
      @forthic_code = forthic_code
      @module_id = "#{name}-#{rand(1_000_000)}"
    end

    # @return [ForthicModule]
    def dup
      result = ForthicModule.new(@name, @interp, @forthic_code)
      result.words = @words.dup
      result.exportable = @exportable.dup
      @variables.each { |key, var| result.variables[key] = var.dup }
      @modules.each { |key, mod| result.modules[key] = mod }
      result.required_modules = @required_modules.dup
      result.forthic_code = @forthic_code
      result
    end

    # @param [String] prefix
    # @param [ForthicModule] mod
    def require_module(prefix, mod)
      @required_modules << { prefix: prefix, module: mod }
    end

    # @param [String] name
    # @return [ForthicModule, nil]
    def find_module(name)
      @modules[name]
    end

    # @param [String] word_name
    # @param [Proc] word_func
    def add_module_word(word_name, word_func)
      add_exportable_word(ModuleWord.new(word_name, word_func))
    end

    # @param [Word] word
    def add_word(word)
      @words << word
    end

    # @param [Word] word
    # @return [ModuleMemoWord]
    def add_memo_words(word)
      memo_word = ModuleMemoWord.new(word)
      @words << memo_word
      @words << ModuleMemoBangWord.new(memo_word)
      @words << ModuleMemoBangAtWord.new(memo_word)
      memo_word
    end

    # @param [Array<String>] names
    def add_exportable(names)
      @exportable.concat(names)
    end

    # @return [Array<Word>]
    def exportable_words
      @words.select { |word| @exportable.include?(word.name) }
    end

    # @param [Word] word
    def add_exportable_word(word)
      @words << word
      @exportable << word.name
    end

    # @param [String] name
    # @param [Object, nil] value
    def add_variable(name, value = nil)
      @variables[name] ||= Variable.new(name, value)
    end

    # @param [Interpreter] interp
    def initialize_modules(interp)
      @required_modules.each do |rec|
        import_module(rec[:prefix], rec[:module], interp)
      end
    end

    # @param [String] module_name
    # @param [String] prefix
    # @param [ForthicModule] mod
    def register_module(module_name, prefix, mod)
      @modules[module_name] = mod
      @module_prefixes[module_name] ||= Set.new
      @module_prefixes[module_name] << prefix
    end

    # @param [String] prefix
    # @param [ForthicModule] mod
    # @param [Interpreter] interp
    def import_module(prefix, mod, interp)
      new_module = mod.dup
      new_module.initialize_modules(interp)

      new_module.exportable_words.each do |word|
        add_word(ImportedWord.new(word, prefix, new_module))
      end
      register_module(mod.name, prefix, new_module)
    end

    # @param [String] name
    # @return [Word, nil]
    def find_word(name)
      find_dictionary_word(name) || find_variable(name)
    end

    # @param [String] word_name
    # @return [Word, nil]
    def find_dictionary_word(word_name)
      @words.reverse.find { |w| w.name == word_name }
    end

    # @param [String] varname
    # @return [PushValueWord, nil]
    def find_variable(varname)
      var_result = @variables[varname]
      var_result ? PushValueWord.new(varname, var_result) : nil
    end
  end
end