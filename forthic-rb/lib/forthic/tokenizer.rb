# frozen_string_literal: true

module Forthic
  class Tokenizer
    attr_accessor :reference_location, :line, :column, :input_string, :input_pos,
                  :whitespace, :quote_chars, :token_start_pos, :token_end_pos,
                  :token_line, :token_column, :token_string

    # @param [String] string
    # @param [CodeLocation, nil] reference_location
    def initialize(string, reference_location = nil)
      reference_location ||= CodeLocation.new(screen_name: "<ad-hoc>")
      @reference_location = reference_location
      @line = reference_location.line
      @column = reference_location.column
      @input_string = unescape_string(string)
      @input_pos = 0
      @whitespace = [" ", "\t", "\n", "\r", "(", ")", ","]
      @quote_chars = ['"', "'"]

      # Token info
      @token_start_pos = 0
      @token_end_pos = 0
      @token_line = 0
      @token_column = 0
      @token_string = ""
    end

    # @return [Token]
    def next_token
      clear_token_string
      transition_from_START
    end

    # @param [String] string
    # @return [String]
    def unescape_string(string)
      string
    end

    def clear_token_string
      @token_string = ""
    end

    def note_start_token
      @token_start_pos = @input_pos + @reference_location.start_pos
      @token_line = @line
      @token_column = @column
    end

    # @param [String] char
    # @return [Boolean]
    def is_whitespace(char)
      @whitespace.include?(char)
    end

    # @param [String] char
    # @return [Boolean]
    def is_quote(char)
      @quote_chars.include?(char)
    end

    # @param [Integer] index
    # @param [String] char
    # @return [Boolean]
    def is_triple_quote(index, char)
      return false unless is_quote(char)
      return false if index + 2 >= @input_string.length
      @input_string[index + 1] == char && @input_string[index + 2] == char
    end

    # @param [Integer] index
    # @return [Boolean]
    def is_start_memo(index)
      return false if index + 1 >= @input_string.length
      @input_string[index] == "@" && @input_string[index + 1] == ":"
    end

    # @param [Integer] num_chars
    def advance_position(num_chars)
      if num_chars >= 0
        num_chars.times do
          if @input_string[@input_pos] == "\n"
            @line += 1
            @column = 1
          else
            @column += 1
          end
          @input_pos += 1
        end
      else
        (-num_chars).times do
          @input_pos -= 1
          raise Forthic::Error, "Invalid position" if @input_pos < 0 || @column < 0
          if @input_string[@input_pos] == "\n"
            @line -= 1
            @column = 1
          else
            @column -= 1
          end
        end
      end
    end

    # @return [CodeLocation]
    def get_token_location
      CodeLocation.new(
        screen_name: @reference_location.screen_name,
        line: @token_line,
        column: @token_column,
        start_pos: @token_start_pos,
        end_pos: @token_start_pos + @token_string.length
      )
    end

    # @return [Token]
    def transition_from_START
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        note_start_token
        advance_position(1)

        next if is_whitespace(char)
        case char
        when "#"
          return transition_from_COMMENT
        when ":"
          return transition_from_START_DEFINITION
        when ";"
          @token_string = char
          return Token.new(TokenType::END_DEF, char, get_token_location)
        when "["
          @token_string = char
          return Token.new(TokenType::START_ARRAY, char, get_token_location)
        when "]"
          @token_string = char
          return Token.new(TokenType::END_ARRAY, char, get_token_location)
        when "{"
          return transition_from_GATHER_MODULE
        when "}"
          @token_string = char
          return Token.new(TokenType::END_MODULE, char, get_token_location)
        else
          if is_start_memo(@input_pos - 1)
            advance_position(1)
            return transition_from_START_MEMO
          elsif is_triple_quote(@input_pos - 1, char)
            advance_position(2)
            return transition_from_GATHER_TRIPLE_QUOTE_STRING(char)
          elsif is_quote(char)
            return transition_from_GATHER_STRING(char)
          else
            advance_position(-1)
            return transition_from_GATHER_WORD
          end
        end
      end
      Token.new(TokenType::EOS, "", get_token_location)
    end

    # @return [Token]
    def transition_from_COMMENT
      note_start_token
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        @token_string += char
        advance_position(1)
        break if char == "\n"
      end
      Token.new(TokenType::COMMENT, @token_string, get_token_location)
    end

    # @return [Token]
    def transition_from_START_DEFINITION
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        next if is_whitespace(char)
        if is_quote(char)
          raise Forthic::Error, "Definition names can't have quotes in them"
        else
          advance_position(-1)
          return transition_from_GATHER_DEFINITION_NAME
        end
      end
      raise Forthic::Error, "Got EOS in START_DEFINITION"
    end

    # @return [Token]
    def transition_from_START_MEMO
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        next if is_whitespace(char)
        if is_quote(char)
          raise Forthic::Error, "Definitions shouldn't have quotes in them"
        else
          advance_position(-1)
          return transition_from_GATHER_MEMO_NAME
        end
      end
      raise Forthic::Error, "Got EOS in START_MEMO"
    end

    def gather_definition_name
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        break if is_whitespace(char)
        if is_quote(char)
          raise Forthic::Error, "Definition names can't have quotes in them"
        elsif ["[", "]", "{", "}"].include?(char)
          raise Forthic::Error, "Definitions can't have '#{char}' in them"
        else
          @token_string += char
        end
      end
    end

    # @return [Token]
    def transition_from_GATHER_DEFINITION_NAME
      note_start_token
      gather_definition_name
      Token.new(TokenType::START_DEF, @token_string, get_token_location)
    end

    # @return [Token]
    def transition_from_GATHER_MEMO_NAME
      note_start_token
      gather_definition_name
      Token.new(TokenType::START_MEMO, @token_string, get_token_location)
    end

    # @return [Token]
    def transition_from_GATHER_MODULE
      note_start_token
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        break if is_whitespace(char)
        if char == "}"
          advance_position(-1)
          break
        else
          @token_string += char
        end
      end
      Token.new(TokenType::START_MODULE, @token_string, get_token_location)
    end

    # @param [String] delim
    # @return [Token]
    def transition_from_GATHER_TRIPLE_QUOTE_STRING(delim)
      note_start_token
      string_delimiter = delim

      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        if char == string_delimiter && is_triple_quote(@input_pos, char)
          advance_position(3)
          return Token.new(TokenType::STRING, @token_string, get_token_location)
        else
          advance_position(1)
          @token_string += char
        end
      end
      raise Forthic::Error, "Unterminated string: #{delim * 3}#{@token_string}"
    end

    # @param [String] delim
    # @return [Token]
    def transition_from_GATHER_STRING(delim)
      note_start_token
      string_delimiter = delim

      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        if char == string_delimiter
          return Token.new(TokenType::STRING, @token_string, get_token_location)
        else
          @token_string += char
        end
      end
      raise Forthic::Error, "Unterminated string: #{delim}#{@token_string}"
    end

    # @return [Token]
    def transition_from_GATHER_WORD
      note_start_token
      while @input_pos < @input_string.length
        char = @input_string[@input_pos]
        advance_position(1)
        break if is_whitespace(char)
        if [";", "[", "]", "{", "}", "#"].include?(char)
          advance_position(-1)
          break
        else
          @token_string += char
        end
      end
      Token.new(TokenType::WORD, @token_string, get_token_location)
    end
  end
end