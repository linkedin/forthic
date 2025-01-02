# # frozen_string_literal: true

require_relative 'word'

module Forthic
  class PushValueWord < Word
    attr_accessor :value

    # @param [String] name
    # @param [Object] value
    def initialize(name, value)
      super(name)
      @value = value
    end

    # @param [Interpreter] interp
    def execute(interp, _options = {})
      interp.stack_push(@value)
    end
  end
end