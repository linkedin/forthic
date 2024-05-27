import json
from requests_oauthlib import OAuth2Session   # type: ignore
import oauthlib.oauth2.rfc6749.errors
from ..module import Module
from ..interfaces import IInterpreter
from ...utils.errors import (
    GdocError,
    ExpiredGdocOAuthToken
)
from typing import List, Any, Dict


def raises_ExpiredGdocOAuthToken(fn):
    """Decorator that catches expiration errors and raises ExpiredGdocOAuthToken instead"""
    def wrapper(*args, **kwargs):
        res = None
        try:
            res = fn(*args, **kwargs)
        except (oauthlib.oauth2.rfc6749.errors.TokenExpiredError, oauthlib.oauth2.rfc6749.errors.InvalidGrantError):
            raise ExpiredGdocOAuthToken()
        return res
    return wrapper


FORTHIC = '''
'''


# TODO: Need to rework this so it matches the gsheet module
class GdocModule(Module):
    """This implements basic access to Gdocs via Google's [gdoc API](https://developers.google.com/docs/api)

    See `docs/modules/gdoc_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('gdoc', interp, FORTHIC)
        self.context_stack: List['CredsContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)

        self.add_module_word('DOC', self.word_DOC)
        self.add_module_word('NEW-DOC', self.word_NEW_DOC)
        self.add_module_word('BATCH-UPDATE', self.word_BATCH_UPDATE)
        self.add_module_word('INSERT', self.word_INSERT)

        self.add_module_word('PT', self.word_PT)
        self.add_module_word('COLOR', self.word_COLOR)

        # ----- Content
        self.add_module_word('TABLE', self.word_TABLE)
        self.add_module_word('TEXT', self.word_TEXT)
        self.add_module_word('PAGE-BREAK', self.word_PAGE_BREAK)

        # ----- Content manipulation
        self.add_module_word('TEXT-CONCAT', self.word_TEXT_CONCAT)
        self.add_module_word('<PARAGRAPH-STYLE', self.word_l_PARAGRAPH_STYLE)
        self.add_module_word('<TABLE-STYLE', self.word_l_TABLE_STYLE)
        self.add_module_word('<TABLE-COLUMN-PROPERTIES', self.word_l_TABLE_COLUMN_PROPERTIES)
        self.add_module_word('<FULL-TABLE-STYLE', self.word_l_FULL_TABLE_STYLE)
        self.add_module_word('<MERGE-TABLE-CELLS', self.word_l_MERGE_TABLE_CELLS)
        self.add_module_word('<TEXT-STYLE', self.word_l_TEXT_STYLE)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( doc_id -- doc )
    @raises_ExpiredGdocOAuthToken
    def word_DOC(self, interp: IInterpreter):
        doc_id = interp.stack_pop()

        gdoc_session = self.get_gdoc_session()
        response = self.gdoc_get(gdoc_session, f'https://docs.googleapis.com/v1/documents/{doc_id}')
        result = response.json()
        interp.stack_push(result)

    # ( title -- doc )
    @raises_ExpiredGdocOAuthToken
    def word_NEW_DOC(self, interp: IInterpreter):
        title = interp.stack_pop()

        gdoc_session = self.get_gdoc_session()
        body_data = {'title': title}
        response = self.gdoc_post(gdoc_session, 'https://docs.googleapis.com/v1/documents', body_data)
        result = response.json()
        interp.stack_push(result)

    # ( doc_id updates -- response )
    @raises_ExpiredGdocOAuthToken
    def word_BATCH_UPDATE(self, interp: IInterpreter):
        updates = interp.stack_pop()
        doc_id = interp.stack_pop()

        def is_empty_insert(update):
            insertText = update.get('insertText')
            if insertText and insertText['text'] == '':
                return True
            return False

        non_empty_updates = [u for u in updates if not is_empty_insert(u)]
        gdoc_session = self.get_gdoc_session()
        body_data = {'requests': non_empty_updates}
        response = self.gdoc_post(gdoc_session, f'https://docs.googleapis.com/v1/documents/{doc_id}:batchUpdate', body_data)
        result = response.json()
        if "error" in result:
            raise RuntimeError(result)
        interp.stack_push(result)

    # ( number -- dimension )
    def word_PT(self, interp: IInterpreter):
        number = interp.stack_pop()
        result = {
            "magnitude": number,
            "unit": "PT"
        }
        interp.stack_push(result)

    # ( red green blue -- Color )
    # NOTE: colors are from 0.0 to 1.0
    def word_COLOR(self, interp: IInterpreter):
        blue = interp.stack_pop()
        green = interp.stack_pop()
        red = interp.stack_pop()
        result = {
            "color": {
                "rgbColor": {
                    "red": red,
                    "green": green,
                    "blue": blue,
                }
            }
        }
        interp.stack_push(result)

    # ( text -- Text )
    def word_TEXT(self, interp: IInterpreter):
        text = interp.stack_pop()
        result = Text(text)
        interp.stack_push(result)

    # ( [Text] -- ConcatText )
    def word_TEXT_CONCAT(self, interp: IInterpreter):
        text_items = interp.stack_pop()
        result = ConcatText(text_items)
        interp.stack_push(result)

    # ( Text style -- Text )
    def word_l_TEXT_STYLE(self, interp: IInterpreter):
        style = interp.stack_pop()
        text = interp.stack_pop()
        text.add_text_style(style)
        interp.stack_push(text)

    # ( table_rows -- Table )
    def word_TABLE(self, interp: IInterpreter):
        table_rows = interp.stack_pop()
        result = Table(table_rows)
        interp.stack_push(result)

    # ( Text style -- Text )
    def word_l_PARAGRAPH_STYLE(self, interp: IInterpreter):
        style = interp.stack_pop()
        text = interp.stack_pop()
        text.add_paragraph_style(style)
        interp.stack_push(text)

    # ( Table style row col row_span col_span -- Table )
    def word_l_TABLE_STYLE(self, interp: IInterpreter):
        col_span = interp.stack_pop()
        row_span = interp.stack_pop()
        col = interp.stack_pop()
        row = interp.stack_pop()
        style = interp.stack_pop()
        table = interp.stack_pop()
        table.add_table_style(style, row, col, row_span, col_span)
        interp.stack_push(table)

    # ( Table column_properties column_indices -- Table )
    def word_l_TABLE_COLUMN_PROPERTIES(self, interp: IInterpreter):
        column_indices = interp.stack_pop()
        column_properties = interp.stack_pop()
        table = interp.stack_pop()
        table.add_column_properties(column_properties, column_indices)
        interp.stack_push(table)

    # ( Table style -- Table )
    def word_l_FULL_TABLE_STYLE(self, interp: IInterpreter):
        style = interp.stack_pop()
        table = interp.stack_pop()
        table.add_full_table_style(style)
        interp.stack_push(table)

    # ( Table row col row_span col_span -- Table )
    def word_l_MERGE_TABLE_CELLS(self, interp: IInterpreter):
        col_span = interp.stack_pop()
        row_span = interp.stack_pop()
        col = interp.stack_pop()
        row = interp.stack_pop()
        table = interp.stack_pop()
        table.add_merge_cells(row, col, row_span, col_span)
        interp.stack_push(table)

    # ( -- PageBreak )
    def word_PAGE_BREAK(self, interp: IInterpreter):
        result = PageBreak()
        interp.stack_push(result)

    # ( doc_id char_index content_array -- )
    def word_INSERT(self, interp: IInterpreter):
        content_array = interp.stack_pop()
        char_index = interp.stack_pop()
        doc_id = interp.stack_pop()

        def run_batch_update(batch_updates: List[Dict[str, Any]]):
            updates = [u for u in batch_updates if u]
            if not updates:
                return
            interp.stack_push(doc_id)
            interp.stack_push(updates)
            interp.run("BATCH-UPDATE")
            interp.stack_pop()  # Pop result of BATCH-UPDATE

        normalized_content_array = normalize_content_array(char_index, content_array)

        # NOTE: We don't support tables that contain tables

        # Insert first pass content
        batch_inserts = [c.get_insert_request() for c in normalized_content_array]
        run_batch_update(batch_inserts)

        # Style first pass content
        batch_styles = []
        for c in normalized_content_array:
            batch_styles += c.get_style_requests()

        run_batch_update(batch_styles)

        # Style tables
        batch_styles = []
        tables = filter(lambda c: isinstance(c, Table), normalized_content_array)
        for t in tables:
            batch_styles += t.get_table_styles()

        run_batch_update(batch_styles)

        # Add the table content
        batch_updates = []
        for c in reversed(normalized_content_array):
            batch_updates += c.get_insert_content_requests()
            batch_updates += c.get_merges()

        run_batch_update(batch_updates)

    # ----- Helpers ------------------------------------------------------------------------------------------
    def gdoc_get(self, gdoc_session: OAuth2Session, api_url: str):
        context = self.get_context()
        response = gdoc_session.get(api_url, proxies=context.get_proxies())
        if response.status_code == 403:
            raise ExpiredGdocOAuthToken()
        return response

    def gdoc_post(self, gdoc_session: OAuth2Session, api_url: str, body_data: Dict[str, Any]):
        context = self.get_context()
        response = gdoc_session.post(
            api_url,
            data=json.dumps(body_data),
            proxies=context.get_proxies())

        if response.status_code == 403:
            raise ExpiredGdocOAuthToken()
        return response

    def get_context(self) -> 'CredsContext':
        if not self.context_stack:
            raise GdocError(
                'Use gdoc.PUSH-CONTEXT! to provide a Google context'
            )
        result = self.context_stack[-1]
        return result

    def get_gdoc_session(self) -> OAuth2Session:
        context = self.get_context()
        app_creds = context.get_app_creds()
        token = context.get_auth_token()

        def token_updater(token):
            pass

        refresh_url = 'https://oauth2.googleapis.com/token'
        result = OAuth2Session(
            app_creds['client_id'],
            token=token,
            auto_refresh_kwargs=app_creds,
            auto_refresh_url=refresh_url,
            token_updater=token_updater,
        )
        return result


class Content:
    """This is the base class for all gdoc content objects

    Some functions like `get_start_index` and `get_end_index` are used by all `Content` subclasses,
    but some (like `get_insert_content_requests`) are only relevant to specific classes. The `Content`
    interface is a union of all possible gdoc content methods and provides sensible defaults so all content
    objects can be used in all rendering situations.
    """
    def __init__(self):
        self.start_index = 0
        self.end_index = 0

    def get_text(self) -> str:
        """Returns the raw text content"""
        return ""

    def update_start_index(self, index: int):
        """Updates the start index of content when it needs to move within a document"""
        return

    def get_start_index(self) -> int:
        return self.start_index

    def get_end_index(self) -> int:
        return self.end_index

    def get_insert_request(self) -> Dict[str, Any]:
        """Returns gdoc API batch request to insert the content into a document

        Some content (like `Table`) must be inserted first before their subcontent is added.
        """
        raise RuntimeError("Content is meant to be subclassed")

    def get_style_requests(self) -> List[Dict[str, Any]]:
        """Returns an array of style requests for the `Content` object"""
        return []

    def get_insert_content_requests(self) -> List[Dict[str, Any]]:
        """For container objects like `Table`, this returns an array of insertion requests for their subcontent"""
        return []

    def get_merges(self) -> List[Dict[str, Any]]:
        """For `Table`, this returns an array of cell merge requests"""
        return []

    def get_table_styles(self) -> List[Dict[str, Any]]:
        """For `Table`, returns an array of gdoc Table styles"""
        return []


class PageBreak(Content):
    def __init__(self):
        super().__init__()

    def update_start_index(self, index: int):
        self.start_index = index
        self.end_index = index + 2

    def get_insert_request(self) -> Dict[str, Any]:
        result = {
            "insertPageBreak": {
                "location": {"segmentId": "", "index": self.start_index}
            }
        }
        return result


class Text(Content):
    """This represents text that's being accumulated in a content array for a batch render
    """
    def __init__(self, text):
        super().__init__()
        self.text = text
        self.style_requests = []
        self.update_start_index(0)

    def get_text(self) -> str:
        return self.text

    def update_start_index(self, index: int):
        """Updates the start/end indexes of the content and style
        """
        self.start_index = index
        self.end_index = index + len(self.text) + 1  # Add implicit newline
        cur_index = index

        # Update style requests
        def update_style(update_type: str, style: Dict[str, Any]):
            num_chars = style[update_type]["range"]["endIndex"] - style[update_type]["range"]["startIndex"]
            style[update_type]["range"]["startIndex"] = cur_index
            style[update_type]["range"]["endIndex"] = cur_index + num_chars
            return

        for r in self.style_requests:
            if "updateTextStyle" in r:
                update_style("updateTextStyle", r)
            elif "updateParagraphStyle" in r:
                update_style("updateParagraphStyle", r)
            else:
                raise RuntimeError(f"Unknown style request: {r}")

    def get_insert_request(self) -> Dict[str, Any]:
        result = {
            "insertText": {
                "text": self.text,
                "location": {"segmentId": "", "index": self.start_index}
            }
        }
        return result

    def add_text_style(self, style: Dict[str, Any]):
        style_request = {
            "updateTextStyle": {
                "textStyle": style,
                "fields": ",".join(style.keys()),
                "range": {
                    "segmentId": "",
                    "startIndex": self.start_index,
                    "endIndex": self.end_index
                }
            }
        }
        self.style_requests.append(style_request)

    def add_paragraph_style(self, style: Dict[str, Any]):
        style_request = {
            "updateParagraphStyle": {
                "paragraphStyle": style,
                "fields": ",".join(style.keys()),
                "range": {
                    "segmentId": "",
                    "startIndex": self.start_index,
                    "endIndex": self.end_index
                }
            }
        }
        self.style_requests.append(style_request)

    def get_style_requests(self) -> List[Dict[str, Any]]:
        return self.style_requests


class ConcatText(Content):
    """This represents an array of Text that's being concatenated
    """
    def __init__(self, text_items: List[Text]):
        super().__init__()
        self.text_items = text_items
        self.update_start_index(0)

    def get_text(self) -> str:
        result = ""
        for t in self.text_items:
            result += t.get_text()
        return result

    def update_start_index(self, index: int):
        """Updates the start/end indexes of the content and style
        """
        text = self.get_text()
        self.start_index = index
        self.end_index = index + len(text) + 1  # Add implicit newline
        cur_index = index

        for t in self.text_items:
            t.update_start_index(cur_index)
            cur_index += len(t.get_text())

    def get_insert_request(self) -> Dict[str, Any]:
        result = {
            "insertText": {
                "text": self.get_text(),
                "location": {"segmentId": "", "index": self.start_index}
            }
        }
        return result

    def get_style_requests(self) -> List[Dict[str, Any]]:
        result = []
        for t in self.text_items:
            result += t.get_style_requests()
        return result


class Table(Content):
    """This represents a table to render
    """
    def __init__(self, table_rows: List[List[Content]]):
        super().__init__()
        self.table_rows = self.normalize_rows(table_rows)
        self.table_rows_w_indexes: List[List[Any]] = []
        self.table_styles: List[Dict[str, Any]] = []
        self.merges: List[Dict[str, Any]] = []
        self.update_start_index(0)

    def normalize_rows(self, rows: List[List[Any]]) -> List[List[Any]]:
        blank = None
        if not rows:
            return []

        def max_row_length() -> int:
            res = 0
            for r in rows:
                if len(r) > res:
                    res = len(r)
            return res

        row_length = max_row_length()
        for r in rows:
            if len(r) < row_length:
                r += (row_length - len(r)) * [blank]
        return rows

    def update_start_index(self, index: int):
        self.start_index = index

        # Tables advance the index by 1 at the start and 1 at the end
        # Every row advances the index by 1
        # Every cell advances the index by 2
        num_rows = len(self.table_rows)
        num_cols = len(self.table_rows[0])
        self.end_index = index + 2 + num_rows + 2 * num_rows * num_cols

        # Update merge cells requests
        for m in self.merges:
            m["mergeTableCells"]["tableRange"]["tableCellLocation"]["tableStartLocation"]["index"] = self.start_index

        # Add indexes to table content
        self.table_rows_w_indexes = []
        cur_index = index + 1   # Advance index for rows container
        for r in self.table_rows:
            cur_index += 1      # Advance index for row
            row_w_index = []
            for c in r:
                cur_index += 1  # Advance index for start cell
                cell_w_index = [c, cur_index]
                row_w_index.append(cell_w_index)
                cur_index += 1  # Advance index for start paragraph
            self.table_rows_w_indexes.append(row_w_index)
        return

    def add_table_style(self, style: Dict[str, Any], row: int, col: int, row_span: int, col_span: int):
        request = {
            "updateTableCellStyle": {
                "tableCellStyle": style,
                "fields": ",".join(style.keys()),
                "tableRange": {
                    "tableCellLocation": {
                        "tableStartLocation": {"segmentId": "", "index": self.start_index},
                        "rowIndex": row,
                        "columnIndex": col
                    },
                    "rowSpan": row_span,
                    "columnSpan": col_span
                }
            }
        }
        self.table_styles.append(request)
        return

    def add_full_table_style(self, style: Dict[str, Any]):
        request = {
            "updateTableCellStyle": {
                "tableCellStyle": style,
                "fields": ",".join(style.keys()),
                "tableStartLocation": {
                    "segmentId": "",
                    "index": self.start_index
                }
            }
        }
        self.table_styles.append(request)
        return

    def add_column_properties(self, column_properties: Dict[str, Any], column_indices: List[int]):
        request = {
            "updateTableColumnProperties": {
                "tableStartLocation": {
                    "segmentId": "",
                    "index": self.start_index
                },
                "columnIndices": column_indices,
                "tableColumnProperties": column_properties,
                "fields": ",".join(column_properties.keys())
            }
        }
        self.table_styles.append(request)
        return

    def add_merge_cells(self, row: int, col: int, row_span: int, col_span: int):
        request = {
            "mergeTableCells": {
                "tableRange": {
                    "tableCellLocation": {
                        "tableStartLocation": {"segmentId": "", "index": self.start_index},
                        "rowIndex": row,
                        "columnIndex": col
                    },
                    "rowSpan": row_span,
                    "columnSpan": col_span
                }
            }
        }
        self.merges.append(request)

    def get_merges(self) -> List[Dict[str, Any]]:
        return self.merges

    def get_table_styles(self) -> List[Dict[str, Any]]:
        def get_style_update(style) -> Dict[str, Any]:
            types = ["updateTableCellStyle", "updateTableColumnProperties"]
            for t in types:
                if t in style:
                    return style[t]
            raise RuntimeError(f"Couldn't find style update in {style}")

        # Update the start_index of each table style
        for style in self.table_styles:
            style_update = get_style_update(style)
            if "tableRange" in style_update:
                style_update["tableRange"]["tableCellLocation"]["tableStartLocation"]["index"] = self.start_index
            else:
                style_update["tableStartLocation"]["index"] = self.start_index
        return self.table_styles

    def get_insert_request(self) -> Dict[str, Any]:
        result = {
            "insertTable": {
                "rows": len(self.table_rows),
                "columns": len(self.table_rows[0]),  # We've normalized table rows, so there will be a valid row
                "location": {"segmentId": "", "index": self.start_index - 1}   # Bring within paragraph
            }
        }

        return result

    def get_insert_content_requests(self) -> List[Dict[str, Any]]:
        result = []
        for r in reversed(self.table_rows_w_indexes):
            for cell in reversed(r):
                index = cell[1]
                cell_content = cell[0]
                if cell_content:
                    cell_content.update_start_index(index)
                    result.append(cell_content.get_insert_request())
                    result += cell_content.get_style_requests()
        return result


def normalize_content_array(char_index: int, content_array: List[Content]) -> List[Content]:
    cur_index = char_index
    result: List[Content] = []
    last_content = None
    for content in content_array:
        # Add implied paragraph if necessary
        if isinstance(content, Table) and not isinstance(last_content, Text):
            implied_paragraph = Text(" ")
            implied_paragraph.update_start_index(cur_index)
            result.append(implied_paragraph)
            cur_index = implied_paragraph.get_end_index()

        # Remove implicit newline between two TextContents in a row
        if isinstance(content, Text) and isinstance(last_content, Text):
            cur_index -= 1

        content.update_start_index(cur_index)
        cur_index = content.get_end_index()
        result.append(content)
        last_content = content
    return result


class CredsContext:
    """Clients of the gsheet module must provide extend CredsContext and use PUSH-CONTEXT!
    in order to set the current creds context"""

    def get_app_creds(self):
        """Returns an object with the following fields: client_id, client_secret"""
        return None

    def get_proxies(self):
        """Returns a dict object containing proxies for fields 'http' and 'https'"""
        return None

    def get_auth_token(self):
        """Returns an object with token information returned from google.

        This will have fields like access_token, refresh_token, scope, etc.
        """
        return None
