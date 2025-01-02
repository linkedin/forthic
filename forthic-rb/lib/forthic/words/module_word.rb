# # frozen_string_literal: true

require_relative 'word'

module Forthic
  class ModuleWord < Word
    attr_accessor :handler

    # @param [String] name
    # @param [Proc] handler
    def initialize(name, handler)
      super(name)
      @handler = handler
    end

    # @param [Interpreter] interp
    def execute(interp)
      @handler.call(interp)
    end
  end
end