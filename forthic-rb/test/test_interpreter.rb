# frozen_string_literal: true

require "minitest/autorun"
require "forthic"

class TestInterpreter < Minitest::Test
  def test_initial_state
    interp = Forthic::Interpreter.new
    assert_equal [], interp.stack
    assert_equal "", interp.cur_module.name
  end

  def test_push_string
    interp = Forthic::Interpreter.new
    interp.run("'Howdy'")
    assert_equal "Howdy", interp.stack_pop
  end

  def test_comment
    interp = Forthic::Interpreter.new
    interp.run("# A comment")
    interp.run("#A comment")
    assert_equal 0, interp.stack.length
  end

  def test_empty_array
    interp = Forthic::Interpreter.new
    interp.run("[]")
    assert_equal [], interp.stack_pop
  end

  def test_start_module
    interp = Forthic::Interpreter.new

    # Push application module onto module stack
    interp.run("{")
    assert_equal 2, interp.module_stack.length
    assert_equal interp.module_stack[0], interp.module_stack[1]

    # Push module-A onto module stack
    interp = Forthic::Interpreter.new
    interp.run("{module-A")
    assert_equal 2, interp.module_stack.length
    assert_equal "module-A", interp.module_stack[1].name
    refute_nil interp.app_module.modules["module-A"]

    # Push module-A and then module-B onto module stack
    interp = Forthic::Interpreter.new
    interp.run("{module-A {module-B")
    assert_equal 3, interp.module_stack.length
    assert_equal "module-A", interp.module_stack[1].name
    assert_equal "module-B", interp.module_stack[2].name

    module_a = interp.app_module.modules["module-A"]
    refute_nil module_a.modules["module-B"]

    interp.run("}}")
    assert_equal 1, interp.module_stack.length
    assert_equal interp.app_module, interp.module_stack[0]
  end

  def test_definition
    # Can define and find a word in the app module
    interp = Forthic::Interpreter.new
    interp.run(": NOTHING   ;")
    word = interp.app_module.find_word("NOTHING")
    refute_nil word

    # Words defined in other modules aren't automatically available in the app module
    interp = Forthic::Interpreter.new
    interp.run("{module-A   : NOTHING   ;}")
    word = interp.app_module.find_word("NOTHING")
    assert_nil word

    module_a = interp.app_module.modules["module-A"]
    word = module_a.find_word("NOTHING")
    refute_nil word
  end

  def test_memo
    interp = Forthic::Interpreter.new

    interp.run("@: MY-MEMO   ;")
    ["MY-MEMO", "MY-MEMO!", "MY-MEMO!@"].each do |name|
      word = interp.app_module.find_word(name)
      refute_nil word
    end

    # Test storing a value and retrieving it
    interp.run("41 MY-MEMO")
    interp.run("MY-MEMO")
    assert_equal 41, interp.stack_pop

    # Test refreshing a value
    interp.stack = []
    interp.run("81 MY-MEMO!")
    assert_equal 0, interp.stack.length
    interp.run("MY-MEMO")
    assert_equal 81, interp.stack_pop

    # Test !@
    interp.stack = []
    interp.run("101 MY-MEMO!@")
    assert_equal 1, interp.stack.length
    assert_equal 101, interp.stack_pop
    interp.stack = []
    interp.run("MY-MEMO")
    assert_equal 101, interp.stack_pop
  end

  def test_word_scope
    interp = Forthic::Interpreter.new
    interp.run(%(
      : APP-MESSAGE   "Hello (from app)";
      {module1
          APP-MESSAGE
      }
    ))
    assert_equal "Hello (from app)", interp.stack_pop
  end

  def test_open_module_test_word
    interp = Forthic::Interpreter.new
    interp.run(%(
      {mymodule
         : MESSAGE   "Hello (from mymodule)";
      }
      : MESSAGE   {mymodule MESSAGE };
      MESSAGE
    ))
    assert_equal "Hello (from mymodule)", interp.stack_pop
  end

  def test_open_module_test_memo
    interp = Forthic::Interpreter.new
    interp.run(%(
      {mymodule
         @: MESSAGE-MEMO   "Hello (from mymodule memo)";
      }
      : MESSAGE   {mymodule MESSAGE-MEMO };
      MESSAGE
    ))
    assert_equal "Hello (from mymodule memo)", interp.stack_pop
  end

  def test_word
    interp = Forthic::Interpreter.new
    interp.run(": MESSAGE   'Howdy' ;")
    interp.run("MESSAGE")
    assert_equal "Howdy", interp.stack_pop

    interp = Forthic::Interpreter.new
    interp.run("{module-A {module-B   : MESSAGE   'In module-B' ;}}")
    interp.run("{module-A {module-B   MESSAGE}}")
    assert_equal "In module-B", interp.stack_pop
  end

  def test_search_global_module
    interp = Forthic::Interpreter.new
    interp.run("'Hi'")
    assert_equal 1, interp.stack.length
    interp.run("POP")
    assert_equal 0, interp.stack.length
  end
end
