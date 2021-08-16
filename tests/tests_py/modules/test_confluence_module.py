import unittest
from forthic.interpreter import Interpreter
from forthic.modules.confluence_module import ConfluenceModule
from tests.tests_py.modules.confluence_context import ConfluenceTestContext


def get_interp():
    result = Interpreter()
    result.register_module(ConfluenceModule)

    # Set up Confluence test context
    result.run('[["confluence" ""]] USE-MODULES')
    result.stack_push(ConfluenceTestContext())
    result.run("PUSH-CONTEXT!")
    return result


class TestConfluenceModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_HOST(self):
        self.interp.run("HOST")
        self.assertEqual(self.interp.stack[0], "http://testcontext")

    def test_PAGE_INFO(self):
        self.interp.run("'SPACE' 'A page title' PAGE-INFO")
        page_info = self.interp.stack[0]
        self.assertEqual("1234", page_info["id"])

    def test_NBSP(self):
        self.interp.run("NBSP")
        self.assertEqual("&nbsp;", self.interp.stack[0])

    def test_SPACES_WIDE(self):
        self.interp.run("""
        'Howdy' 10 SPACES-WIDE
        'Howdy' 3 SPACES-WIDE
        """)
        nbsp = "&nbsp;"
        self.assertEqual(f"Howdy{nbsp*5}", self.interp.stack[0])
        self.assertEqual(f"Howdy", self.interp.stack[1])

    def test_UPSERT_PAGE(self):
        self.interp.run("""
        'SPACE' 'A parent title' 'A new page title' 'h2. This is a test' UPSERT-PAGE
        'SPACE' 'A parent title' 'A new page title' 'h2. This is an update' UPSERT-PAGE
        """)

    def test_pipe_ESCAPE_TABLE_CONTENT2(self):
        self.interp.run("""   
        '+*Q2 Objectives*+\r\n\r\nReduce tech debts, operational burdens and cost-to-serve' |ESCAPE-TABLE-CONTENT
        """)
        assert(self.interp.stack[0] == '+*Q2 Objectives*+\nReduce tech debts, operational burdens and cost-to-serve')

if __name__ == '__main__':
    unittest.main()
