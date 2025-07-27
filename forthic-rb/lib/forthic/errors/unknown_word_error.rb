# frozen_string_literal: true

require_relative "../forthic_error"

module Forthic
  module Errors
    class UnknownWordError < ForthicError
      attr_reader :word_name

      # @param [String] word_name The name of the unknown word
      # @param [CodeLocation, nil] location Where the error occurred
      # @param [Array<String>] suggested_words Optional list of similar words for suggestions
      def initialize(word_name, location = nil, suggested_words = [])
        @word_name = word_name
        @suggested_words = suggested_words

        description = build_description(suggested_words)
        super(ErrorCodes::WORD_NOT_FOUND, "Word '#{word_name}' not found", description, location)
      end

      # @return [Array<String>] List of suggested similar words
      def suggested_words
        @suggested_words.dup
      end

      private

      # @param [Array<String>] suggested_words
      # @return [String]
      def build_description(suggested_words)
        base_description = "Check for typos in the word name or ensure the word has been defined."
        return base_description if suggested_words.empty?

        suggestions = suggested_words.take(3).join(", ")
        "#{base_description} Did you mean: #{suggestions}?"
      end
    end
  end
end
