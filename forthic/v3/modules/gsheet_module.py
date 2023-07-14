import re
import json
import urllib.parse
from requests_oauthlib import OAuth2Session   # type: ignore
import oauthlib.oauth2.rfc6749.errors
from ..module import Module
from ..interfaces import IInterpreter
from ...utils.errors import (
    GsheetError,
    ExpiredGsheetOAuthToken
)
from typing import List, Any, Dict, Tuple


def raises_ExpiredGsheetOAuthToken(fn):
    """Decorator that catches expiration errors and raises ExpiredGsheetOAuthToken instead"""
    def wrapper(*args, **kwargs):
        res = None
        try:
            res = fn(*args, **kwargs)
        except (oauthlib.oauth2.rfc6749.errors.TokenExpiredError, oauthlib.oauth2.rfc6749.errors.InvalidGrantError):
            raise ExpiredGsheetOAuthToken()
        return res
    return wrapper


FORTHIC = ""


class GsheetModule(Module):
    """This implements access to gsheets via Google's [Sheets API](https://developers.google.com/sheets/api)
    """

    def __init__(self, interp: IInterpreter):
        super().__init__('gsheet', interp, FORTHIC)
        self.context_stack: List['CredsContext'] = []

        # These are set by "flag words" to change the behavior of the words in this module
        self.flags = {
            "range": None,
            "transpose": False,
            "cell_format": False,
        }

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)

        self.add_module_word('SPREADSHEET', self.word_SPREADSHEET)
        self.add_module_word('TAB', self.word_TAB)
        self.add_module_word('TAB@', self.word_TAB_at)
        self.add_module_word('ENSURE-TAB!', self.word_ENSURE_TAB_bang)

        self.add_module_word('ROWS', self.word_ROWS)
        self.add_module_word('ROWS!', self.word_ROWS_bang)

        self.add_module_word('CLEAR!', self.word_CLEAR_bang)

        self.add_module_word('RECORDS', self.word_RECORDS)
        self.add_module_word('RECORDS!', self.word_RECORDS_bang)
        self.add_module_word('BATCH-UPDATE-TAB!', self.word_BATCH_UPDATE_TAB_bang)

        # Flag words
        self.add_module_word('!RANGE', self.word_bang_RANGE)
        self.add_module_word('!TRANSPOSE', self.word_bang_TRANSPOSE)
        self.add_module_word('!CELL-FORMAT', self.word_bang_CELL_FORMAT)

        # Utils
        self.add_module_word('INDEX>COL-NAME', self.word_INDEX_to_COL_NAME)
        self.add_module_word('COL-NAME>INDEX', self.word_COL_NAME_to_INDEX)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        """Sets the credentials context used to make calls against the API
        """
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( url -- Spreadsheet )
    # ( Tab -- Spreadsheet )
    @raises_ExpiredGsheetOAuthToken
    def word_SPREADSHEET(self, interp: IInterpreter):
        """Creates a `Spreadsheet` object from a url or extracts the parent spreadsheet from a `Tab` object
        """
        arg = interp.stack_pop()

        context = self.get_context()
        if isinstance(arg, str):
            url = arg
            result = Spreadsheet(context, url)
        elif isinstance(arg, Tab):
            tab = arg
            result = tab.get_spreadsheet()
        else:
            result = None
        interp.stack_push(result)

    # ( url -- Tab )
    @raises_ExpiredGsheetOAuthToken
    def word_TAB(self, interp: IInterpreter):
        """Creates a `Tab` object from a url
        """
        url = interp.stack_pop()

        context = self.get_context()
        _, tab_id = get_gsheet_id_and_tab_id(url)
        spreadsheet = Spreadsheet(context, url)
        result = spreadsheet.get_tab(tab_id)
        interp.stack_push(result)

    # ( Spreadsheet id -- Tab )
    # ( Spreadsheet name -- Tab )
    @raises_ExpiredGsheetOAuthToken
    def word_TAB_at(self, interp: IInterpreter):
        """Retrieves a `Tab` from a `Spreadsheet` using its id or name
        """
        id_or_name = interp.stack_pop()
        spreadsheet = interp.stack_pop()

        result = spreadsheet.get_tab(id_or_name)
        interp.stack_push(result)

    # ( Tab -- rows )
    @raises_ExpiredGsheetOAuthToken
    def word_ROWS(self, interp: IInterpreter):
        """Retrieves all the rows from a `Tab`

        Flag words:
            * !RANGE: This specifies the range to read (See https://developers.google.com/sheets/api/guides/concepts#cell)
            * !TRANSPOSE: If set, data is returned by column rather than by row
        """
        tab = interp.stack_pop()

        flags = self.get_flags()

        if flags.get('range'):
            tab_range = f"{tab.get_name()}!{flags.get('range')}"
        else:
            tab_range = tab.get_name()

        result = get_rows(tab.get_context(), tab.get_spreadsheet_id(), tab_range, flags.get('transpose'))
        interp.stack_push(result)

    # ( Tab rows -- )
    @raises_ExpiredGsheetOAuthToken
    def word_ROWS_bang(self, interp: IInterpreter):
        """Writes an array of rows to a `Tab`

        Flag words:
            * !RANGE: This specifies the start range to write to (See https://developers.google.com/sheets/api/guides/concepts#cell)
            * !TRANSPOSE: By default, data will be written as rows. If this flag word is set, data will be written as columns
            * !CELL-FORMAT: By default, data is assumed to be strings. If `!CELL-FORMAT` is set, the data will be treated
                            as being in a "cell" format. This is a record with a `content` string field and an `updateRequest`
                            field that contains a record with the structure of a gsheet API update request.
                            See https://developers.google.com/sheets/api/samples/formatting
        """
        rows = interp.stack_pop()
        tab = interp.stack_pop()

        flags = self.get_flags()

        if flags.get('range'):
            tab_range = f"{tab.get_name()}!{flags.get('range')}"
        else:
            tab_range = tab.get_name()

        if flags.get('cell_format'):
            write_cells(tab, tab_range, rows, flags.get('transpose'))
        else:
            write_rows(tab, tab_range, rows, flags.get('transpose'))

    # ( Tab header -- Records )
    @raises_ExpiredGsheetOAuthToken
    def word_RECORDS(self, interp: IInterpreter):
        """Reads data from a `Tab` as an array of records

        The specified `header` is an array of column names that will be searched for in the rows of the gsheet.
        If a header is found, the rows below it will be used to create an array of records where the header
        columns are used as record fields.
        """
        header = interp.stack_pop()
        tab = interp.stack_pop()

        # Check flags
        flags = self.get_flags()
        if flags.get('range'):
            tab_range = f"{tab.get_name()}!{flags.get('range')}"
        else:
            tab_range = tab.get_name()

        rows = get_rows(tab.get_context(), tab.get_spreadsheet_id(), tab_range)

        def to_ascii(value: str) -> str:
            res = ''.join([c for c in value if ord(c) < 128]).strip()
            return res

        def get_header_to_column(values: List[str]) -> Dict[str, int]:
            res = {}
            ascii_values = [to_ascii(v) for v in values]
            for h in header:
                for i in range(len(ascii_values)):
                    if ascii_values[i] == h:
                        res[h] = i
            return res

        def find_header() -> Any:
            res = None
            for i in range(len(rows)):
                header_to_column = get_header_to_column(rows[i])
                found_all = True
                for h in header:
                    if h not in header_to_column:
                        found_all = False
                        break
                if found_all:
                    res = {
                        'header_row': i,
                        'header_to_column': header_to_column,
                    }
                    break
            return res

        header_info = find_header()
        if not header_info:
            raise GsheetError(
                f"Can't find header ({header}) in gsheet {tab.get_spreadsheet_id()} {tab.get_name()}"
            )

        def row_to_rec(row: List[str]) -> Dict[str, Any]:
            res = {}
            for h in header:
                col = header_info['header_to_column'][h]
                res[h] = row[col]
            return res

        result = []
        for r in rows[header_info['header_row'] + 1:]:
            result.append(row_to_rec(r))

        interp.stack_push(result)

    # ( Tab header records -- )
    @raises_ExpiredGsheetOAuthToken
    def word_RECORDS_bang(self, interp: IInterpreter):
        """Writes an array of records to a `Tab`

        The specified header determines the order of the columns.
        NOTE: This uses the same flag words as `ROWS!`
        """
        records = interp.stack_pop()
        header = interp.stack_pop()
        tab = interp.stack_pop()

        # Peek at cell_format flag, but don't clear them since ROWS! will use them
        use_cell_format = self.flags.get('cell_format')

        header_values = header
        default_value = ""

        # The cell format requires values to be dicts with a "content" field
        if use_cell_format:
            header_values = [{"content": h} for h in header]
            default_value = {"content": ""}

        rows = [header_values]
        for rec in records:
            row = []
            for h in header:
                row.append(rec.get(h) or default_value)
            rows.append(row)

        interp.stack_push(tab)
        interp.stack_push(rows)
        interp.run("ROWS!")

    # ( Tab update_requests -- )
    @raises_ExpiredGsheetOAuthToken
    def word_BATCH_UPDATE_TAB_bang(self, interp: IInterpreter):
        """Makes a batch update against a tab

        This is essentially a low-level way to access the gsheets API directly.
        See https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/batchUpdate
        """
        update_requests = interp.stack_pop()
        tab = interp.stack_pop()
        batch_update_tab(tab, update_requests)

    # ( Tab -- )
    @raises_ExpiredGsheetOAuthToken
    def word_CLEAR_bang(self, interp: IInterpreter):
        """Clears the contents of a `Tab`
        """
        tab = interp.stack_pop()
        clear_tab(tab)

    # ( Spreadsheet tab_name -- Tab)
    @raises_ExpiredGsheetOAuthToken
    def word_ENSURE_TAB_bang(self, interp: IInterpreter):
        """Ensures that the specified `Tab` exists in the gsheet and then returns it
        """
        tab_name = interp.stack_pop()
        spreadsheet = interp.stack_pop()
        result = ensure_tab(spreadsheet, tab_name)
        interp.stack_push(result)

    # ( index -- col_name )
    def word_INDEX_to_COL_NAME(self, interp: IInterpreter):
        """Converts an integer index to a character column name
        """
        index = interp.stack_pop()
        result = index_to_col_name(index)
        interp.stack_push(result)

    # ( col_name -- index )
    def word_COL_NAME_to_INDEX(self, interp: IInterpreter):
        """Converts a character column name to an index
        """
        col_name = interp.stack_pop()
        result = col_name_to_index(col_name)
        interp.stack_push(result)

    # ( range -- )
    def word_bang_RANGE(self, interp: IInterpreter):
        """Sets a spreadsheet `range` flag
        """
        tab_range = interp.stack_pop()
        self.flags["range"] = tab_range

    # ( -- )
    def word_bang_TRANSPOSE(self, interp: IInterpreter):
        """Sets a `transpose` flag to treat data as columns instead of rows
        """
        self.flags["transpose"] = True

    # ( -- )
    def word_bang_CELL_FORMAT(self, interp: IInterpreter):
        """Sets a `cell_format` flag to indicate that data is provided in "cell" format rather than as strings
        """
        self.flags["cell_format"] = True

    # =================================
    # Helpers
    def get_flags(self):
        flags = self.flags.copy()
        self.flags = {}
        return flags

    def get_context(self) -> 'CredsContext':
        if not self.context_stack:
            raise GsheetError(
                'Use gsheet.PUSH-CONTEXT! to provide a Google context'
            )
        result = self.context_stack[-1]
        return result


# ------------------------------------------------
# Helper functions and classes
def get_gsheets_session(context) -> OAuth2Session:
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


def get_gsheet_id_and_tab_id(url: str) -> Tuple[str, str]:
    """Parses a spreadsheet ID and tab ID from a gsheet URL
    """
    match = re.match(r'.*docs\.google\.com.*\/d\/([^\/]+).*gid=(\d+)', url)
    if not match:
        raise GsheetError(
            f'Unable to find gsheet_id and tab key from: {url}'
        )
    gsheet_id = match.group(1)
    tab_id = int(match.group(2))
    return gsheet_id, tab_id


def get_sheet_info(context, gsheet_id: str) -> Any:
    gsheets_session = get_gsheets_session(context)
    response = gsheets_session.get(
        f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}',
        proxies=context.get_proxies(),
    )
    if not response.ok:
        raise GsheetError(response.text)
    result = response.json()
    return result


def get_rows(context, spreadsheet_id: str, spreadsheet_range: str, transpose: bool = False) -> List[List[str]]:
    spreadsheet_range_url_encoded = urllib.parse.quote_plus(spreadsheet_range)
    gsheets_session = get_gsheets_session(context)

    if transpose:
        majorDimension = 'COLUMNS'
    else:
        majorDimension = 'ROWS'

    base = 'https://sheets.googleapis.com/v4/spreadsheets'
    api_url = f"{base}/{spreadsheet_id}/values/{spreadsheet_range_url_encoded}?majorDimension={majorDimension}"
    response = gsheets_session.get(api_url, proxies=context.get_proxies())
    if not response.ok:
        raise GsheetError(response.text)

    data = response.json()
    if "values" not in data:
        rows = []
    else:
        rows = data['values']

    # We add empty cells where needed to make all rows the same length
    def pad_rows(rows: List[List[str]]) -> List[List[str]]:
        if not rows:
            return rows

        row_lengths = [len(r) for r in rows]
        max_length = max(row_lengths)
        res = []
        for r in rows:
            padded_row = r
            if len(r) < max_length:
                for _ in range(max_length - len(r)):
                    padded_row.append('')
            res.append(padded_row)
        return res

    result = pad_rows(rows)
    return result


def write_rows(tab: "Tab", spreadsheet_range: str, rows: List[List[str]], transpose: bool = False):
    context = tab.get_context()
    spreadsheet_id = tab.get_spreadsheet_id()

    spreadsheet_range_url_encoded = urllib.parse.quote_plus(spreadsheet_range)

    if not rows:
        return

    if transpose:
        majorDimension = 'COLUMNS'
    else:
        majorDimension = 'ROWS'

    gsheets_session = get_gsheets_session(context)
    update_data = {
        'range': spreadsheet_range,
        'majorDimension': majorDimension,
        'values': rows,
    }
    input_option = 'USER_ENTERED'
    api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheet_id}/values/{spreadsheet_range_url_encoded}?valueInputOption={input_option}'
    status = gsheets_session.put(
        api_url,
        data=json.dumps(update_data),
        proxies=context.get_proxies(),
    )
    if not status.ok:
        raise GsheetError(f'Problem writing to gsheet {spreadsheet_id} {spreadsheet_range}: {status.text}')


def write_cells(tab: "Tab", spreadsheet_range: str, rows: List[List[Any]], transpose: bool = False):
    spreadsheet_id = tab.get_spreadsheet_id()

    content_rows = []
    for r in rows:
        content_row = []
        for cell in r:
            content_row.append(cell.get('content'))
        content_rows.append(content_row)

    # Write content
    write_rows(tab, spreadsheet_range, content_rows, transpose)

    # Gather formatting
    # See: https://developers.google.com/sheets/api/samples/formatting

    def get_start_row_col():
        pieces = spreadsheet_range.split("!")

        if len(pieces) < 2:
            startRowIndex = 0
            startColumnIndex = 0
        else:
            range_pieces = pieces[1].split(":")
            range_start = range_pieces[0]
            match = re.match(r'([A-Z]+)(\d+)', range_start)

            column_name = match.group(1)
            row = int(match.group(2))

            startColumnIndex = col_name_to_index(column_name)
            startRowIndex = row - 1

        return startRowIndex, startColumnIndex

    startRowIndex, startColumnIndex = get_start_row_col()

    # Figure out update requests
    def get_update_request_row(row):
        values = []
        for cell in row:
            values.append(cell.get("updateRequest") or {})
        result = {
            "values": values
        }
        return result

    def transpose_rows(rows):
        num_rows = len(rows)
        if num_rows == 0:
            return []

        result = []
        num_cols = len(rows[0])
        for i in range(num_cols):
            col = []
            for j in range(num_rows):
                col.append(rows[j][i])
            result.append(col)
        return result

    if transpose:
        rows = transpose_rows(rows)

    update_request_rows = []
    for r in rows:
        update_request_rows.append(get_update_request_row(r))

    def get_fields():
        result = set()
        for row in rows:
            for cell in row:
                update_request = cell.get("updateRequest") or {}
                for k in update_request.keys():
                    if k == "userEnteredFormat":
                        for sub_k in update_request[k].keys():
                            result.add(f"{k}.{sub_k}")
                    else:
                        result.add(k)
        return list(result)

    fields = get_fields()

    # If there are no fields to update, we're done
    if not fields:
        return

    update_requests = [{
        "updateCells": {
            "range": {
                "sheetId": spreadsheet_id,
                "startRowIndex": startRowIndex,
                "startColumnIndex": startColumnIndex,
            },
            "rows": update_request_rows,
            "fields": ",".join(fields)
        }
    }]

    batch_update_tab(tab, update_requests)


def clear_tab(tab: "Tab"):
    context = tab.get_context()
    spreadsheet_id = tab.get_spreadsheet_id()
    tab_id = tab.get_id()

    gsheets_session = get_gsheets_session(context)
    update_data = {
        'requests': [
            {
                'updateCells': {
                    'range': {'sheetId': tab_id},
                    'fields': 'userEnteredValue',
                }
            },
        ]
    }
    api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheet_id}:batchUpdate'
    status = gsheets_session.post(
        api_url,
        data=json.dumps(update_data),
        proxies=context.get_proxies(),
    )
    if not status.ok:
        raise GsheetError(f'Problem clearing gsheet {spreadsheet_id} {tab.get_name()}: {status.text}')


def ensure_tab(spreadsheet: "Spreadsheet", tab_name: str) -> "Tab":
    if spreadsheet.has_tab(tab_name):
        return spreadsheet.get_tab(tab_name)

    # Otherwise, add tab, update spreadsheet state, and return tab
    context = spreadsheet.get_context()
    gsheets_session = get_gsheets_session(context)
    update_data = {
        'requests': [
            {
                'addSheet': {
                    'properties': {
                        'title': tab_name
                    }
                }
            },
        ]
    }
    spreadsheet_id = spreadsheet.get_spreadsheet_id()
    api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheet_id}:batchUpdate'
    status = gsheets_session.post(
        api_url,
        data=json.dumps(update_data),
        proxies=context.get_proxies(),
    )
    if not status.ok:
        raise GsheetError(f'Problem adding sheet to gsheet {spreadsheet_id}: {status.text}')

    # Update spreadsheet
    updated_spreadsheet = Spreadsheet(context, spreadsheet.get_url())
    spreadsheet.update(updated_spreadsheet)
    return spreadsheet.get_tab(tab_name)


def batch_update_tab(tab: "Tab", update_requests):
    context = tab.get_context()
    spreadsheet_id = tab.get_spreadsheet_id()
    tab_id = tab.get_id()

    gsheets_session = get_gsheets_session(context)

    def add_sheet_id(update_requests):
        for r in update_requests:
            for v in r.values():
                if 'range' in v:
                    v['range']['sheetId'] = tab_id
        return

    add_sheet_id(update_requests)
    data = {
        'requests': update_requests
    }

    api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheet_id}:batchUpdate'
    status = gsheets_session.post(
        api_url,
        data=json.dumps(data),
        proxies=context.get_proxies(),
    )
    if not status.ok:
        raise GsheetError(f'Problem running batch_update_tab {spreadsheet_id} {tab.get_name()}: {status.text}')


def index_to_col_name(zero_based_index: int) -> str:
    if zero_based_index < 0:
        raise GsheetError(f'Index ({zero_based_index}) must be >= 0')

    one_based_index = zero_based_index + 1

    def rightmost_digit(num):
        modulo = num % 26
        if modulo == 0:
            res = 'Z'
        else:
            offset = modulo - 1
            res = chr(ord('A') + offset)
        return res

    def downshift(num):
        res = int((num - 1) / 26)
        return res

    digits = []
    while one_based_index:
        digits.append(rightmost_digit(one_based_index))
        one_based_index = downshift(one_based_index)
    digits.reverse()
    result = ''.join(digits)
    return result


def col_name_to_index(col_name: str) -> int:
    col_name = col_name.upper().strip()
    if not re.match('^[A-Z]+$', col_name):
        raise GsheetError(f'Column name ({col_name}) must be all letters')

    def char_to_val(c):
        res = ord(c) - ord('A') + 1
        return res

    reversed_col_name = col_name[::-1]
    result = 0
    for i in range(len(reversed_col_name)):
        char = reversed_col_name[i]
        result += char_to_val(char) * (26 ** i)

    result = result - 1   # Convert to 0-based index
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


class Spreadsheet:
    def __init__(self, context, url):
        self.context = context
        self.url = url

        self.spreadsheet_id, _ = get_gsheet_id_and_tab_id(url)
        self.sheet_info = get_sheet_info(context, self.spreadsheet_id)

    def update(self, spreadsheet):
        self.context = spreadsheet.context
        self.url = spreadsheet.url
        self.spreadsheet_id = spreadsheet.spreadsheet_id
        self.sheet_info = spreadsheet.sheet_info

    def get_context(self):
        return self.context

    def get_url(self):
        return self.url

    def get_spreadsheet_id(self):
        return self.spreadsheet_id

    def has_tab(self, id_or_name):
        sheets = self.sheet_info['sheets']
        for s in sheets:
            properties = s['properties']
            if properties['sheetId'] == id_or_name or properties['title'] == id_or_name:
                return True
        return False

    def get_tab(self, id_or_name):
        sheets = self.sheet_info['sheets']

        tab_properties = None
        for s in sheets:
            properties = s['properties']
            if properties['sheetId'] == id_or_name or properties['title'] == id_or_name:
                tab_properties = properties
                break

        if tab_properties is None:
            return None

        result = Tab(self.context, self, tab_properties)
        return result


class Tab:
    def __init__(self, context, spreadsheet, tab_properties):
        self.context = context
        self.spreadsheet = spreadsheet
        self.tab_properties = tab_properties

    def get_context(self):
        return self.context

    def get_spreadsheet(self):
        return self.spreadsheet

    def get_spreadsheet_id(self):
        return self.spreadsheet.spreadsheet_id

    def get_id(self):
        return self.tab_properties['sheetId']

    def get_name(self):
        return self.tab_properties['title']
