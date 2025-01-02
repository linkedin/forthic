# # frozen_string_literal: true

require_relative 'word'
require_relative '../forthic_error'

module Forthic
  class DefinitionWord < Word
    attr_accessor :words, :cur_index

    # @param [String] name
    def initialize(name)
      super(name)
      @words = []
      @cur_index = 0
    end

    # @param [Word] word
    def add_word(word)
      @words.push(word)
    end

    # @param [Interpreter] interp
    def execute(interp)
      @words.each do |word|
        begin
          word.execute(interp)
        rescue => e
          error = ForthicError.new(
            "definition_word-29",
            "Error executing word #{word.name}",
            "Error in #{self.name} definition",
            interp.get_string_location
          )
          error.set_caught_error(e)
          raise error
        end
      end
    end
  end
end