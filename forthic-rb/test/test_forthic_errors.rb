# frozen_string_literal: true

require "minitest/autorun"
require "forthic"

class TestForthicErrors < Minitest::Test
  def setup
    @interp = Forthic::Interpreter.new
  end

  def test_enhanced_forthic_error_to_s
    location = Forthic::CodeLocation.new(screen_name: "test.forthic", line: 1, column: 5)
    error = Forthic::ForthicError.new("test-error", "Test error occurred", "This is a test error", location)

    expected = "Test error occurred at test.forthic:1:5: This is a test error"
    assert_equal expected, error.to_s
    assert_equal expected, error.message
  end

  def test_forthic_error_to_s_without_location
    error = Forthic::ForthicError.new("test-error", "Test error occurred", "This is a test error")

    expected = "Test error occurred: This is a test error"
    assert_equal expected, error.to_s
  end

  def test_forthic_error_to_s_without_description
    location = Forthic::CodeLocation.new(screen_name: "test.forthic", line: 1, column: 5)
    error = Forthic::ForthicError.new("test-error", "Test error occurred", "", location)

    expected = "Test error occurred at test.forthic:1:5"
    assert_equal expected, error.to_s
  end

  def test_forthic_error_to_s_minimal
    error = Forthic::ForthicError.new("test-error", "Test error occurred", "")

    expected = "Test error occurred"
    assert_equal expected, error.to_s
  end

  def test_unknown_word_error_creation
    location = Forthic::CodeLocation.new(screen_name: "test.forthic", line: 1, column: 5)
    error = Forthic::Errors::UnknownWordError.new("UNKNOWN_WORD", location, ["KNOWN_WORD", "ANOTHER_WORD"])

    assert_equal "UNKNOWN_WORD", error.word_name
    assert_equal ["KNOWN_WORD", "ANOTHER_WORD"], error.suggested_words
    assert_equal Forthic::ErrorCodes::WORD_NOT_FOUND, error.error_key
    assert_equal location, error.location
  end

  def test_unknown_word_error_without_suggestions
    error = Forthic::Errors::UnknownWordError.new("UNKNOWN_WORD")

    assert_equal "UNKNOWN_WORD", error.word_name
    assert_equal [], error.suggested_words
    expected_description = "Check for typos in the word name or ensure the word has been defined."
    assert_equal expected_description, error.get_description
  end

  def test_unknown_word_error_with_suggestions
    error = Forthic::Errors::UnknownWordError.new("UNKNOWN_WORD", nil, ["KNOWN_WORD", "ANOTHER_WORD", "THIRD_WORD"])

    expected_description = "Check for typos in the word name or ensure the word has been defined. Did you mean: KNOWN_WORD, ANOTHER_WORD, THIRD_WORD?"
    assert_equal expected_description, error.get_description
  end

  def test_unknown_word_error_limits_suggestions
    # Test that only first 3 suggestions are included
    many_suggestions = %w[WORD1 WORD2 WORD3 WORD4 WORD5]
    error = Forthic::Errors::UnknownWordError.new("UNKNOWN_WORD", nil, many_suggestions)

    expected_description = "Check for typos in the word name or ensure the word has been defined. Did you mean: WORD1, WORD2, WORD3?"
    assert_equal expected_description, error.get_description
  end

  def test_unknown_word_error_to_s
    location = Forthic::CodeLocation.new(screen_name: "test.forthic", line: 2, column: 10)
    error = Forthic::Errors::UnknownWordError.new("TYPO_WORD", location, ["TYPE_WORD"])

    expected = "Word 'TYPO_WORD' not found at test.forthic:2:10: Check for typos in the word name or ensure the word has been defined. Did you mean: TYPE_WORD?"
    assert_equal expected, error.to_s
  end

  def test_unknown_word_error_raised_by_interpreter
    # Test that the interpreter raises UnknownWordError for unknown words
    assert_raises(Forthic::Errors::UnknownWordError) do
      @interp.run("DEFINITELY_NOT_A_WORD")
    end
  end

  def test_unknown_word_error_contains_word_name
    # Test that the error contains the actual unknown word name

    @interp.run("UNDEFINED_WORD")
    flunk "Expected UnknownWordError to be raised"
  rescue Forthic::Errors::UnknownWordError => e
    assert_equal "UNDEFINED_WORD", e.word_name
    assert_includes e.to_s, "UNDEFINED_WORD"
  end

  def test_unknown_word_error_with_suggestions_from_interpreter
    # Define a word with a similar name to test suggestions
    @interp.run(": TEST_WORD 'hello' ;")

    begin
      @interp.run("TEST_WROD")  # Typo in "TEST_WORD"
      flunk "Expected UnknownWordError to be raised"
    rescue Forthic::Errors::UnknownWordError => e
      assert_equal "TEST_WROD", e.word_name
      # Should suggest the similar word we defined
      assert_includes e.suggested_words, "TEST_WORD"
    end
  end

  def test_unknown_word_error_suggestions_from_global_words
    @interp.run("PP")  # Typo for "POP"
    flunk "Expected UnknownWordError to be raised"
  rescue Forthic::Errors::UnknownWordError => e
    assert_equal "PP", e.word_name
    # Should suggest similar global words
    suggested_words = e.suggested_words
    refute_empty suggested_words, "Should provide suggestions for typos of global words"
  end

  def test_backwards_compatibility_with_generic_error_handling
    # Test that code expecting generic ForthicError still works

    @interp.run("UNKNOWN_WORD")
    flunk "Expected error to be raised"
  rescue Forthic::ForthicError => e
    # Should catch UnknownWordError as ForthicError
    assert_kind_of Forthic::Errors::UnknownWordError, e
    assert_includes e.to_s, "UNKNOWN_WORD"
  end

  def test_error_location_preserved
    # Test that location information is preserved in errors
    # First define a valid word, then use it with an invalid word
    @interp.run(": VALID_WORD 'test' ;")
    forthic_code = "VALID_WORD INVALID_WORD"

    begin
      @interp.run(forthic_code)
      flunk "Expected UnknownWordError to be raised"
    rescue Forthic::Errors::UnknownWordError => e
      assert_equal "INVALID_WORD", e.word_name
      refute_nil e.location, "Error should have location information"
      # Location should point to the invalid word, not the valid one
      assert_equal 1, e.location.line_number
      assert e.location.column_number > 10, "Column should be after VALID_WORD"
    end
  end

  def test_suggested_words_immutable
    error = Forthic::Errors::UnknownWordError.new("UNKNOWN", nil, ["SUGGESTION"])
    suggestions = error.suggested_words
    suggestions << "MODIFIED"

    # Original error should not be modified
    assert_equal ["SUGGESTION"], error.suggested_words
  end
end
