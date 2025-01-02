# frozen_string_literal: true

require "test_helper"
require "forthic"

class TestTokenizer < Minitest::Test
  def test_knows_token_positions
    @main_forthic = <<~FORTHIC

      : ADD-ONE   1 23 +;
      {module
          # 2 ADD-ONE
      }
      @: MY-MEMO   [ "hello" '''triple-single-quoted-string'''];
    FORTHIC

    @reference_location = Forthic::CodeLocation.new(
      screen_name: "main",
      line: 1,
      column: 1,
      start_pos: 0
    )

    @tokenizer = Forthic::Tokenizer.new(@main_forthic, @reference_location)

    # TOK_START_DEF
    begin_def = @tokenizer.next_token
    assert_equal Forthic::TokenType::START_DEF, begin_def.type
    assert_equal 2, begin_def.location.line
    assert_equal 3, begin_def.location.column
    assert_equal "main", begin_def.location.screen_name
    assert_equal 3, begin_def.location.start_pos
    assert_equal 10, begin_def.location.end_pos

    # TOK_WORD: 1
    one_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::WORD, one_token.type
    assert_equal 2, one_token.location.line
    assert_equal 13, one_token.location.column
    assert_equal "main", one_token.location.screen_name
    assert_equal 13, one_token.location.start_pos
    assert_equal 14, one_token.location.end_pos

    # TOK_WORD: 23
    token_23 = @tokenizer.next_token
    assert_equal Forthic::TokenType::WORD, token_23.type
    assert_equal 2, token_23.location.line
    assert_equal 15, token_23.location.column
    assert_equal "main", token_23.location.screen_name
    assert_equal 15, token_23.location.start_pos
    assert_equal 17, token_23.location.end_pos

    # TOK_WORD: +
    plus_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::WORD, plus_token.type
    assert_equal 2, plus_token.location.line
    assert_equal 18, plus_token.location.column
    assert_equal "main", plus_token.location.screen_name
    assert_equal 18, plus_token.location.start_pos
    assert_equal 19, plus_token.location.end_pos

    # TOK_END_DEF
    end_def_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::END_DEF, end_def_token.type
    assert_equal 2, end_def_token.location.line
    assert_equal 19, end_def_token.location.column
    assert_equal "main", end_def_token.location.screen_name
    assert_equal 19, end_def_token.location.start_pos
    assert_equal 20, end_def_token.location.end_pos

    # TOK_START_MODULE
    module_start_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::START_MODULE, module_start_token.type
    assert_equal 3, module_start_token.location.line
    assert_equal 2, module_start_token.location.column
    assert_equal "main", module_start_token.location.screen_name
    assert_equal 22, module_start_token.location.start_pos
    assert_equal 28, module_start_token.location.end_pos

    # TOK_COMMENT
    comment_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::COMMENT, comment_token.type
    assert_equal 4, comment_token.location.line
    assert_equal 6, comment_token.location.column
    assert_equal "main", comment_token.location.screen_name
    assert_equal 34, comment_token.location.start_pos
    assert_equal 45, comment_token.location.end_pos

    # TOK_END_MODULE
    end_module_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::END_MODULE, end_module_token.type
    assert_equal 5, end_module_token.location.line
    assert_equal 1, end_module_token.location.column
    assert_equal "main", end_module_token.location.screen_name
    assert_equal 45, end_module_token.location.start_pos
    assert_equal 46, end_module_token.location.end_pos

    # TOK_START_MEMO
    start_memo_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::START_MEMO, start_memo_token.type
    assert_equal 6, start_memo_token.location.line
    assert_equal 4, start_memo_token.location.column
    assert_equal "main", start_memo_token.location.screen_name
    assert_equal 50, start_memo_token.location.start_pos
    assert_equal 57, start_memo_token.location.end_pos

    # TOK_START_ARRAY
    start_array_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::START_ARRAY, start_array_token.type
    assert_equal 6, start_array_token.location.line
    assert_equal 14, start_array_token.location.column
    assert_equal "main", start_array_token.location.screen_name
    assert_equal 60, start_array_token.location.start_pos
    assert_equal 61, start_array_token.location.end_pos

    # TOK_STRING: "hello"
    start_string_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::STRING, start_string_token.type
    assert_equal 6, start_string_token.location.line
    assert_equal 17, start_string_token.location.column
    assert_equal "main", start_string_token.location.screen_name
    assert_equal 63, start_string_token.location.start_pos
    assert_equal 68, start_string_token.location.end_pos

    # TOK_STRING: '''triple-single-quoted-string'''
    start_triple_string_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::STRING, start_triple_string_token.type
    assert_equal 6, start_triple_string_token.location.line
    assert_equal 27, start_triple_string_token.location.column
    assert_equal "main", start_triple_string_token.location.screen_name
    assert_equal 73, start_triple_string_token.location.start_pos
    assert_equal 100, start_triple_string_token.location.end_pos

    # TOK_END_ARRAY
    end_array_token = @tokenizer.next_token
    assert_equal Forthic::TokenType::END_ARRAY, end_array_token.type
    assert_equal 6, end_array_token.location.line
    assert_equal 57, end_array_token.location.column
    assert_equal "main", end_array_token.location.screen_name
    assert_equal 103, end_array_token.location.start_pos
    assert_equal 104, end_array_token.location.end_pos
  end


  def test_knows_token_location_in_ad_hoc_string_given_reference
    reference_location = Forthic::CodeLocation.new(
      screen_name: "main",
      line: 21,
      column: 15,
      start_pos: 67
    )
    @tokenizer = Forthic::Tokenizer.new("'key' REC@ LOWERCASE", reference_location)

    # 'key'
    token = @tokenizer.next_token
    assert_equal "key", token.string
    assert_equal 21, token.location.line
    assert_equal 16, token.location.column
    assert_equal "main", token.location.screen_name
    assert_equal 68, token.location.start_pos
    assert_equal 71, token.location.end_pos

    # REC@
    token = @tokenizer.next_token
    assert_equal "REC@", token.string
    assert_equal 21, token.location.line
    assert_equal 21, token.location.column
    assert_equal "main", token.location.screen_name
    assert_equal 73, token.location.start_pos
    assert_equal 77, token.location.end_pos

    # LOWERCASE
    token = @tokenizer.next_token
    assert_equal "LOWERCASE", token.string
    assert_equal 21, token.location.line
    assert_equal 26, token.location.column
    assert_equal "main", token.location.screen_name
    assert_equal 78, token.location.start_pos
    assert_equal 87, token.location.end_pos
  end
end
