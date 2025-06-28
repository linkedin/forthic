# # frozen_string_literal: true

module Forthic
  class Variable
    attr_accessor :name, :value

    # @param [String] name
    # @param [Object] value
    def initialize(name, value = nil)
      @name = name
      @value = value
    end

    # @return [String]
    def get_name
      @name
    end

    # @param [Object] val
    def set_value(val)
      @value = val
    end

    # @return [Object]
    def get_value
      @value
    end

    # @return [Variable]
    def dup
      Variable.new(@name, @value)
    end
  end
end
