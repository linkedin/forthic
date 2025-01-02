# frozen_string_literal: true

require_relative 'word'
require_relative 'module_memo_word'

module Forthic
  class ModuleMemoBangWord < Word
    attr_accessor :memo_word

    # @param [ModuleMemoWord] memo_word
    def initialize(memo_word)
      super("#{memo_word.name}!")
      @memo_word = memo_word
    end

    # @param [Interpreter] interp
    def execute(interp)
      @memo_word.refresh(interp)
    end
  end
end