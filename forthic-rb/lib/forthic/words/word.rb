# frozen_string_literal: true

module Forthic
  class Word
    attr_accessor :name, :string, :location

    # @param [String] name
    def initialize(name)
      @name = name
      @string = name
      @location = nil
    end

    # @param [CodeLocation] location
    def set_location(location)
      @location = location
    end

    # @return [CodeLocation, nil]
    def get_location
      @location
    end

    # @param [Interpreter] _interp
    # @param [Hash] _options
    def execute(_interp, _options = {})
      raise "Must override Word.execute"
    end
  end
end
