import unittest
from forthic.v2.interpreter import Interpreter
from forthic.v2.modules.gdoc_module import ConcatText, GdocModule, Text, Table
from forthic.v2.modules.gsheet_module import CredsContext


def get_interp():
    result = Interpreter()
    result.register_module(GdocModule)
    result.run('["gdoc"] USE-MODULES')
    result.stack_push(get_context())
    result.run("gdoc.PUSH-CONTEXT!")
    return result


class TestGdocModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_Text(self):
        text = Text("Howdy")

        self.assertIsInstance(text, Text, "We have a Text object")
        self.assertEqual(text.get_text(), "Howdy", "Text has the correct content")
        self.assertEqual([text.get_start_index(), text.get_end_index()], [0, 6], "Start/end index are correct")

        # Update the start index and check
        text.update_start_index(22)
        self.assertEqual([text.get_start_index(), text.get_end_index()], [22, 28], "Start/end index are correct after update_start_index")

        # Check get insert and style requests
        insert_request = text.get_insert_request()
        style_requests = text.get_style_requests()
        expected_insert_request = {
            "insertText": {
                "text": "Howdy",
                "location": {"segmentId": "", "index": 22}
            }
        }
        self.assertEqual(insert_request, expected_insert_request, "Insert request is correct")
        self.assertEqual(style_requests, [], "There aren't any style requests yet")

        # Add a text style
        text_style = {
            "bold": True,
            "underline": False
        }
        text.add_text_style(text_style)
        style_requests = text.get_style_requests()
        self.assertEqual(len(style_requests), 1, "Only one style")
        text_style_request = {
            "updateTextStyle": {
                "textStyle": text_style,
                "fields": "bold,underline",
                "range": {
                    "segmentId": "",
                    "startIndex": 22,
                    "endIndex": 28
                }
            }
        }
        self.assertEqual(style_requests[0], text_style_request, "Text style request should be correct")

        # Add a paragraph style
        paragraph_style = {
            "namedStyleType": "HEADING_1"
        }
        text.add_paragraph_style(paragraph_style)
        style_requests = text.get_style_requests()
        self.assertEqual(len(style_requests), 2, "Have a text style and a paragraph style")
        paragraph_style_request = {
            "updateParagraphStyle": {
                "paragraphStyle": paragraph_style,
                "fields": "namedStyleType",
                "range": {
                    "segmentId": "",
                    "startIndex": 22,
                    "endIndex": 28
                }
            }
        }
        self.assertEqual(style_requests[1], paragraph_style_request, "Paragraph style is correct")
        return

    def test_ConcatText(self):
        bold_style = {
            "bold": True,
            "underline": False
        }

        bold_underline_style = {
            "bold": True,
            "underline": True
        }

        text1 = Text("Howdy, ")
        text2 = Text("Everyone!")
        text1.add_text_style(bold_style)
        text2.add_text_style(bold_underline_style)

        # Look at concatenating two Text objects
        concat1 = ConcatText([text1, text2])
        self.assertEqual(concat1.get_text(), "Howdy, Everyone!", "ConcatText should have correct content")
        expected_insert_request = {
            "insertText": {
                "text": "Howdy, Everyone!",
                "location": {"segmentId": "", "index": 0}
            }
        }
        self.assertEqual(concat1.get_insert_request(), expected_insert_request, "Insert request should be correct")
        expected_style_requests = [
            {
                "updateTextStyle": {
                    "textStyle": bold_style,
                    "fields": "bold,underline",
                    "range": {
                        "segmentId": "",
                        "startIndex": 0,
                        "endIndex": 8
                    }
                }
            },
            {
                "updateTextStyle": {
                    "textStyle": bold_underline_style,
                    "fields": "bold,underline",
                    "range": {
                        "segmentId": "",
                        "startIndex": 7,
                        "endIndex": 17
                    }
                }
            }
        ]
        self.assertEqual(concat1.get_style_requests(), expected_style_requests, "Style requests should be correct")

        # Try concatenating ConcatTexts
        concat2 = ConcatText([ConcatText([text1]), ConcatText([text2])])
        self.assertEqual(concat2.get_text(), "Howdy, Everyone!", "Concatenating ConcatText should have correct content")
        self.assertEqual(concat2.get_style_requests(), expected_style_requests, "Style requests should be correct when concatenating ConcatText objects")
        return

    def test_Table_row_normalization(self):
        rows = [
            [Text("A1")],
            [None, Text("B2")]
        ]
        table = Table(rows)
        for row in table.table_rows:
            self.assertEqual(len(row), 2, "All rows should be extended to 2 elements")

        self.assertEqual(table.table_rows[0][1], None, "Table rows are padded with None")
        return

    def test_basic_Table_content(self):
        rows = [
            [Text("A1")],
            [Text("A2")]
        ]
        table = Table(rows)

        # Check table insert request
        insert_request = table.get_insert_request()
        expected_insert_request = {
            "insertTable": {
                "rows": 2,
                "columns": 1,
                "location": {"segmentId": "", "index": -1}
            }
        }
        self.assertEqual(insert_request, expected_insert_request)

        # Check insert content requests
        insert_content_requests = table.get_insert_content_requests()
        self.assertEqual(len(insert_content_requests), 2, "Should have 2 insert content requests")

        # NOTE: `Table` content is inserted in reverse order (to preserve the computed start indexes of each
        #        cell) so the first insert is the last cell (lower right) in the Table
        expected_first_insert_request = {
            "insertText": {
                "text": "A2",
                "location": {"segmentId": "", "index": 6}
            }
        }
        self.assertEqual(insert_content_requests[0], expected_first_insert_request)
        return

    def test_Table_styles(self):
        rows = [
            [Text("A1")],
        ]
        table = Table(rows)

        self.interp.run("""
        : BLUE      0.235 0.522 0.776 gdoc.COLOR;
        : BLUE-ROW-STYLE   [
            ["backgroundColor"   BLUE]
        ] REC;
        BLUE-ROW-STYLE
        """)

        style = self.interp.stack_pop()
        table.add_table_style(style, 0, 0, 1, 1)

        expected_style = {
            "updateTableCellStyle": {
                "tableCellStyle": style,
                "fields": "backgroundColor",
                "tableRange": {
                    "tableCellLocation": {
                        "tableStartLocation": {"segmentId": "", "index": 0},
                        "rowIndex": 0,
                        "columnIndex": 0
                    },
                    "rowSpan": 1,
                    "columnSpan": 1
                }
            }
        }
        self.assertEqual(table.get_table_styles(), [expected_style])
        return

    def test_Table_cell_merge(self):
        rows = [
            [Text("A1"), Text("B1")],
            [Text("A2"), Text("B2")],
        ]
        table = Table(rows)

        # Merge cells in rightmost column
        table.add_merge_cells(0, 1, 2, 1)
        expected_merge_request = {
            "mergeTableCells": {
                "tableRange": {
                    "tableCellLocation": {
                        "tableStartLocation": {"segmentId": "", "index": 0},
                        "rowIndex": 0,
                        "columnIndex": 1
                    },
                    "rowSpan": 2,
                    "columnSpan": 1
                }
            }
        }
        self.assertEqual(table.get_merges(), [expected_merge_request])
        return


class TestGoogleCredsContext(CredsContext):
    def get_app_creds(self):
        return None

    def get_auth_token(self):
        return None


def get_context():
    result = TestGoogleCredsContext()
    return result


if __name__ == '__main__':
    unittest.main()