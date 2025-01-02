# # frozen_string_literal: true

require_relative 'word'
require_relative 'module_word'

module Forthic
  class ImportedWord < Word
    attr_accessor :module_word, :imported_module

    # @param [Word] module_word
    # @param [String] prefix
    # @param [ModuleWord] imported_module
    def initialize(module_word, prefix, imported_module)
      prefix = prefix.empty? ? "" : "#{prefix}."
      super("#{prefix}#{module_word.name}")
      @module_word = module_word
      @imported_module = imported_module
    end

    # @param [Interpreter] interp
    def execute(interp)
      interp.module_stack_push(@imported_module)
      @module_word.execute(interp)
      interp.module_stack_pop
    end
  end
end