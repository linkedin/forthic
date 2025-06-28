# frozen_string_literal: true

module Forthic
  class ForthicError < StandardError
    attr_accessor :error_key, :title, :description, :location, :caught_error

    # @param [String] error_key
    # @param [String] title
    # @param [String] description
    # @param [CodeLocation, nil] location
    def initialize(error_key, title, description, location = nil)
      @error_key = error_key
      @title = title
      @description = description
      @location = location
      @caught_error = nil
    end

    # @param [ForthicError] error
    def set_caught_error(error)
      @caught_error = error
    end

    # @return [String]
    def get_title
      @title
    end

    # @return [String]
    def get_description
      @description
    end

    # @return [Array<ForthicError>]
    def get_error_stack
      max_depth = 100
      cur_error = self
      result = [cur_error]

      max_depth.times do
        break unless cur_error.caught_error

        result << cur_error.caught_error
        cur_error = cur_error.caught_error
      end

      result.reverse
    end
  end
end
