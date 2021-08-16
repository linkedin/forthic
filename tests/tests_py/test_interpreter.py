import unittest
import datetime
from forthic.interpreter import Interpreter
from forthic.module import Module, ModuleWord
from tests.tests_py.sample_date_module import SampleDateModule

class TestInterpreter(unittest.TestCase):

    def test_initial_state(self):
        interp = Interpreter()
        self.assertEqual(0, len(interp.stack))
        self.assertEqual("", interp.module_stack[0].name)


    def test_push_string(self):
        interp = Interpreter()
        interp.run("'Howdy'")
        self.assertEqual("Howdy", interp.stack[0])


    def test_comment(self):
        interp = Interpreter()
        interp.run("# A comment")
        interp.run("#A comment")
        self.assertEqual(0, len(interp.stack))


    def test_empty_array(self):
        interp = Interpreter()
        interp.run("[]")
        self.assertEqual([], interp.stack[0])


    def test_start_module(self):
        interp = Interpreter()

        # Push application module onto module stack
        interp.run("{")
        self.assertEqual(2, len(interp.module_stack))
        self.assertEqual(interp.module_stack[0], interp.module_stack[1])
        if interp.module_stack[0] != interp.module_stack[1]:
            return False

        # Push module-A onto module stack
        interp = Interpreter()
        interp.run("{module-A")
        self.assertEqual(2, len(interp.module_stack))
        self.assertEqual("module-A", interp.module_stack[1].name)
        self.assertIsNotNone(interp.app_module.modules.get("module-A"))

        # Push module-A and then module-B onto module stack
        interp = Interpreter()
        interp.run("{module-A {module-B")
        self.assertEqual(3, len(interp.module_stack))
        self.assertEqual("module-A", interp.module_stack[1].name)
        self.assertEqual("module-B", interp.module_stack[2].name)

        module_A = interp.app_module.modules["module-A"]
        self.assertIsNotNone(module_A.modules.get("module-B"))

        interp.run("}}")
        self.assertEqual(1, len(interp.module_stack))
        self.assertEqual(interp.module_stack[0], interp.app_module)


    def test_definition(self):
        # Can define and find a word in the app module
        interp = Interpreter()
        interp.run(": NOTHING   ;")
        word = interp.app_module.find_word("NOTHING")
        self.assertIsNotNone(word)

        # Words defined in other modules aren't automatically available in the app module
        interp = Interpreter()
        interp.run("{module-A   : NOTHING   ;}")
        word = interp.app_module.find_word("NOTHING")
        self.assertIsNone(word)

        module_A = interp.app_module.modules["module-A"]
        word = module_A.find_word("NOTHING")
        self.assertIsNotNone(word)

    def test_word_scope(self):
        interp = Interpreter()
        interp.run("""
        : APP-MESSAGE   "Hello (from app)";
        {module1
            APP-MESSAGE
        }
        """)
        self.assertEqual("Hello (from app)", interp.stack[0])

    def test_open_module(self):
        # Test word
        interp = Interpreter()
        interp.run("""
        {mymodule
           : MESSAGE   "Hello (from mymodule)";
        }
        : MESSAGE   {mymodule MESSAGE };
        MESSAGE
        """)
        self.assertEqual("Hello (from mymodule)", interp.stack[0])

        # Test memo
        interp = Interpreter()
        interp.run("""
        {mymodule
           'MESSAGE-MEMO'   '"Hello (from mymodule memo)"'   MEMO
        }
        : MESSAGE   {mymodule MESSAGE-MEMO };
        MESSAGE
        """)
        self.assertEqual("Hello (from mymodule memo)", interp.stack[0])

    def test_word(self):
        interp = Interpreter()
        interp.run(": MESSAGE   'Howdy' ;")
        interp.run("MESSAGE")
        self.assertEqual("Howdy", interp.stack[0])

        interp = Interpreter()
        interp.run("{module-A {module-B   : MESSAGE   'In module-B' ;}}")
        interp.run("{module-A {module-B   MESSAGE}}")
        self.assertEqual("In module-B", interp.stack[0])

    def test_search_global_module(self):
        interp = Interpreter()
        interp.run("'Hi'")
        if len(interp.stack) != 1:
            return False
        interp.run("POP")
        if len(interp.stack) != 0:
            return False
        return True


    def test_use_module(self):
        interp = Interpreter()
        interp.register_module(SampleDateModule)
        interp.run("['date'] USE-MODULES")
        interp.run("date.TODAY")
        today = datetime.date.today()
        self.assertEqual(today, interp.stack[0])

        interp.run("{date TODAY}")
        self.assertEqual(today, interp.stack[1])

        interp.run("[['date' '']] USE-MODULES")
        interp.run("TODAY")
        self.assertEqual(today, interp.stack[2])

    def test_builtin_import_builtin(self):
        class ModuleA(Module):
            def __init__(self, interp):
                super().__init__("module-a", interp)
                self.add_exportable_word(ModuleWord("MY-TODAY", self.word_MY_TODAY))
                self.import_module("date1", SampleDateModule(interp), interp)

            # ( -- today )
            def word_MY_TODAY(self, interp):
                interp.run("date1.TODAY")

        interp = Interpreter()
        interp.register_module(ModuleA)

        interp.run("['module-a'] USE-MODULES")
        interp.run("module-a.MY-TODAY")
        today = datetime.date.today()

        self.assertEqual(today, interp.stack[0])
        self.assertEqual(1, len(interp.app_module.words))
        self.assertEqual("module-a.MY-TODAY", interp.app_module.words[0].name)
        self.assertEqual("date1.TODAY", interp.app_module.modules['module-a'].words[-1].name)


if __name__ == '__main__':
    unittest.main()
