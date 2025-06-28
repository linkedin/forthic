# # frozen_string_literal: true

require_relative "word"
require_relative "../token"

module Forthic
  class EndArrayWord < Word
    def initialize
      super("]")
    end

    # @param [Interpreter] interp
    def execute(interp)
      items = []
      item = interp.stack_pop

      # NOTE: This won't infinite loop because interp.stack_pop() will eventually fail
      loop do
        break if item.is_a?(Token) && item.type == TokenType::START_ARRAY
        items.push(item)
        item = interp.stack_pop
      end

      items.reverse!
      interp.stack_push(items)
    end
  end
end
