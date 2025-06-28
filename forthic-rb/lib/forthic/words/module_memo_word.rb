# # frozen_string_literal: true

require_relative "word"

module Forthic
  class ModuleMemoWord < Word
    attr_accessor :word, :has_value, :value

    # @param [Word] word
    def initialize(word)
      super(word.name)
      @word = word
      @has_value = false
      @value = nil
    end

    # @param [Interpreter] interp
    def refresh(interp)
      @word.execute(interp)
      @value = interp.stack_pop
      @has_value = true
    end

    # @param [Interpreter] interp
    def execute(interp)
      refresh(interp) unless @has_value
      interp.stack_push(@value)
    end

    def reset
      @has_value = false
      @value = nil
    end
  end
end
