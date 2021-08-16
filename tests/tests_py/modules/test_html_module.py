import unittest
from forthic.interpreter import Interpreter
from forthic.modules.html_module import HtmlModule


def get_interp():
    result = Interpreter()
    result.register_module(HtmlModule)
    result.run('[["html" ""]] USE-MODULES')
    return result

class TestHtmlModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_element(self):
        self.interp.run("""
        [ "html" ] USE-MODULES
        "h1" html.ELEMENT
        """)
        stack = self.interp.stack
        self.assertEqual("H1", stack[0].tagName)

    def test_common_elements(self):
        self.interp.run("""
        H1 H2 H3 H4 H5 H6
        P UL OL LI A
        TABLE TR TH TD
        SPAN DIV SECTION
        IMG
        STYLE
        CANVAS
        """)
        return True

    def test_child_nodes(self):
        self.interp.run("""
        ["table" "row"] VARIABLES
        TABLE table !
        TR row !
        table @ row @ [TH TH TH] <APPEND <APPEND POP

        table @ CHILD-NODES LENGTH
        table @ CHILD-NODES 0 NTH  row @ ==
        row @ CHILD-NODES LENGTH
        """)
        stack = self.interp.stack
        self.assertEqual([1, True, 3], self.interp.stack)

    def test_node_content(self):
        self.interp.run("""
        P "Now is the <b>time</b>" <INNER-HTML! INNER-HTML
        P "Now is the <b>time</b>" <INNER-TEXT! INNER-HTML

        TABLE TR <APPEND INNER-HTML
        """)
        stack = self.interp.stack

        self.assertEqual(stack[0], "Now is the <b>time</b>")
        self.assertEqual(stack[1], "Now is the &lt;b&gt;time&lt;/b&gt;")
        self.assertEqual(stack[2], "<tr></tr>")

    def test_insert(self):
        self.interp.run("""
        DIV TABLE <APPEND "<span></span>" "afterbegin" <INSERT-ADJ-HTML RENDER
        """)
        stack = self.interp.stack
        self.assertEqual(stack[0], "<div><span></span><table></table></div>")

    def test_svg(self):
        self.interp.run("""
        SVG
        """)
        stack = self.interp.stack
        self.assertEqual(stack[0].getAttribute("xmlns"), "http://www.w3.org/2000/svg")
        self.assertEqual(stack[0].getAttribute("version"), "1.1")

    def test_attributes(self):
        self.interp.run("""
        A "src" "https://www.linkedin.com" <ATTR!  "src" ATTR
        P [["class" "error"] ["display" "block"]] <ATTR! DUP "class" ATTR SWAP "display" ATTR
        """)
        stack = self.interp.stack
        self.assertEqual(["https://www.linkedin.com", "error", "block"], stack)

    def test_classes(self):
        interp = get_interp()
        interp.run("""
        H1 "important" <ADD-CLASS [ "note" "highlight" ] <ADD-CLASS CLASSES
        H1 "important" <ADD-CLASS [ "note" "highlight" ] <ADD-CLASS "note" <REMOVE-CLASS CLASSES
        """)
        stack = interp.stack
        assert(stack[0] == ["important", "note", "highlight"])
        assert(stack[1] == ["important", "highlight"])

    def test_render(self):
        self.interp.run("""
        TABLE RENDER
        TR "class" "total" <ATTR! RENDER
        IMG "src" "https://my_pic.com/1234" <ATTR! RENDER
        P "Howdy, Everyone!" <INNER-HTML! RENDER
        TABLE TR "id" "main-row" <ATTR! <APPEND RENDER
        """)
        stack = self.interp.stack
        self.assertEqual(stack[0], '<table></table>')
        self.assertEqual(stack[1], '<tr class="total"></tr>')
        self.assertEqual(stack[2], '<img src="https://my_pic.com/1234">')
        self.assertEqual(stack[3], '<p>Howdy, Everyone!</p>')
        self.assertEqual(stack[4], '<table><tr id="main-row"></tr></table>')

    def test_raw_html(self):
        self.interp.run("""
        DIV "This is text that's in the div. <H1>My Title!</H1>" RAW-HTML <APPEND RENDER
        """)
        stack = self.interp.stack
        self.assertEqual(stack[0], "<div>This is text that's in the div. <H1>My Title!</H1></div>")


if __name__ == '__main__':
    unittest.main()
