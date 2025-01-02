# frozen_string_literal: true

module Forthic
  class CodeLocation
    attr_accessor :screen_name, :line, :column, :start_pos, :end_pos

    # @param [String] screen_name
    # @param [Integer] line
    # @param [Integer] column
    # @param [Integer] start_pos
    # @param [Integer] end_pos
    def initialize(screen_name: "<ad-hoc>", line: 1, column: 1, start_pos: 0, end_pos: 0)
      @screen_name = screen_name
      @line = line
      @column = column
      @start_pos = start_pos
      @end_pos = end_pos
    end
  end
end
