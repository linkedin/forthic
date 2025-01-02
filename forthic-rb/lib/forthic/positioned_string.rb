# frozen_string_literal: true

module Forthic
  class PositionedString
    attr_accessor :string, :location

    # @string [String] the string value
    # @location [CodeLocation] the location of the string in the code
    def initialize(string, location)
      @string = string
      @location = location
    end

    # @return [String]
    def value_of
      @string
    end
  end
end
