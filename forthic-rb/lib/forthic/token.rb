# frozen_string_literal: true

module Forthic
  module TokenType
    STRING = 1
    COMMENT = 2
    START_ARRAY = 3
    END_ARRAY = 4
    START_MODULE = 5
    END_MODULE = 6
    START_DEF = 7
    END_DEF = 8
    START_MEMO = 9
    WORD = 10
    EOS = 11
  end
end

module Forthic
  class Token
    attr_reader :type, :value, :location

    # @param [TokenType] type
    # @param [String] value
    # @param [CodeLocation] location
    def initialize(type, value, location)
      @type = type
      @value = value
      @location = location
    end

    # @return [String]
    def string
      @value
    end
  end
end
