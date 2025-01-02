# # frozen_string_literal: true

require_relative 'word'

module Forthic
  class EndModuleWord < Word
    def initialize
      super("}")
    end

    # @param [Interpreter] interp
    def execute(interp)
      interp.module_stack_pop
    end
  end
end