import re
import json
import urllib.parse
from requests_oauthlib import OAuth2Session   # type: ignore
import oauthlib.oauth2.rfc6749.errors
from ..module import Module
from ..interfaces import IInterpreter
from typing import List, Any, Dict, Optional, Tuple


class GsheetError(RuntimeError):
    pass


class ExpiredGsheetOAuthToken(GsheetError):
    pass


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


FORTHIC = '''
["headers"] VARIABLES

: SHEET-ROWS   URL>SHEET-ID/RANGE ROWS;   # ( url -- rows )
: SHEET-RECORDS   ( headers ! ) URL>SHEET-ID/RANGE headers @ RECORDS;   # (url headers  -- records )

: NUM-ROWS   URL>TAB-INFO ["properties" "gridProperties" "rowCount"] REC@; # ( url -- num)

["SHEET-ROWS"
"SHEET-RECORDS"
"NUM-ROWS"] EXPORT
'''


class GsheetModule(Module):
    """This implements basic access to gsheets via Google's [Sheets API](https://developers.google.com/sheets/api)

    See `docs/modules/gsheets_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('gsheet', interp, FORTHIC)
        self.context_stack: List['CredsContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)
        self.add_module_word('SHEET-INFO', self.word_SHEET_INFO)
        self.add_module_word('URL>SHEET-INFO', self.word_URL_to_SHEET_INFO)
        self.add_module_word('URL>TAB-INFO', self.word_URL_to_TAB_INFO)
        self.add_module_word('URL>SHEET-ID/RANGE', self.word_URL_to_SHEET_ID_slash_RANGE)
        self.add_module_word('URL>SHEET-ID/TAB-ID', self.word_URL_to_SHEET_ID_slash_TAB_ID)
        self.add_module_word('ROWS', self.word_ROWS)
        self.add_module_word('ROWS!', self.word_ROWS_bang)
        self.add_module_word('COLUMNS!', self.word_COLUMNS_bang)
        self.add_module_word('RECORDS', self.word_RECORDS)
        self.add_module_word('CLEAR-SHEET!', self.word_CLEAR_SHEET_bang)

        self.add_module_word('CLEAR-TAB!', self.word_CLEAR_TAB_bang)
        self.add_module_word('ENSURE-TAB!', self.word_ENSURE_TAB_bang)

        self.add_module_word('BATCH-UPDATE', self.word_BATCH_UPDATE)
        self.add_module_word('BATCH-UPDATE-TAB', self.word_BATCH_UPDATE_TAB)

        self.add_module_word(
            'CONDITIONAL-FORMATS', self.word_CONDITIONAL_FORMATS
        )
        self.add_module_word(
            'DELETE-CONDITIONAL-FORMATS', self.word_DELETE_CONDITIONAL_FORMATS
        )

        self.add_module_word(
            'REPEAT-CELL-FORMATS', self.word_REPEAT_CELL_FORMATS
        )

        self.add_module_word('FILTERS', self.word_FILTERS)
        self.add_module_word(
            'UPDATE-ALL-FILTER-END-ROWS', self.word_UPDATE_ALL_FILTER_END_ROWS
        )

        # Support for batch updates
        self.add_module_word('RANGE', self.word_RANGE)
        self.add_module_word('COLOR', self.word_COLOR)
        self.add_module_word('CONDITION', self.word_CONDITION)
        self.add_module_word('FORMAT', self.word_FORMAT)
        self.add_module_word('BORDER', self.word_BORDER)
        self.add_module_word('BOOLEAN-RULE', self.word_BOOLEAN_RULE)

        self.add_module_word('<BACKGROUND-COLOR', self.word_l_BACKGROUND_COLOR)
        self.add_module_word('<FOREGROUND-COLOR', self.word_l_FOREGROUND_COLOR)
        self.add_module_word('<BOLD', self.word_l_BOLD)
        self.add_module_word('<TEXT-FORMAT', self.word_l_TEXT_FORMAT)

        self.add_module_word('ADD-CONDITIONAL-FORMAT-RULES', self.word_ADD_CONDITIONAL_FORMAT_RULES)
        self.add_module_word('UPDATE-BORDERS', self.word_UPDATE_BORDERS)

        self.add_module_word('INDEX>COL-NAME', self.word_INDEX_to_COL_NAME)
        self.add_module_word('COL-NAME>INDEX', self.word_COL_NAME_to_INDEX)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( gsheet_id -- info )
    @raises_ExpiredGsheetOAuthToken
    def word_SHEET_INFO(self, interp: IInterpreter):
        gsheet_id = interp.stack_pop()

        result = self.get_sheet_info(gsheet_id)
        interp.stack_push(result)

    # ( url -- info )
    @raises_ExpiredGsheetOAuthToken
    def word_URL_to_SHEET_INFO(self, interp: IInterpreter):
        url = interp.stack_pop()
        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        result = self.get_sheet_info(gsheet_id)
        interp.stack_push(result)

    # ( url -- info )
    @raises_ExpiredGsheetOAuthToken
    def word_URL_to_TAB_INFO(self, interp: IInterpreter):
        url = interp.stack_pop()
        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        gsheet_info = self.get_sheet_info(gsheet_id)

        result = None
        for s in gsheet_info['sheets']:
            if str(s['properties']['sheetId']) == str(tab_id):
                result = s
                break
        interp.stack_push(result)

    # (url -- sheet_id range)
    @raises_ExpiredGsheetOAuthToken
    def word_URL_to_SHEET_ID_slash_RANGE(self, interp: IInterpreter):
        url = interp.stack_pop()

        def get_tab_title(tab_id: str, sheet_info: Any) -> Optional[str]:
            sheets = sheet_info['sheets']
            for s in sheets:
                properties = s['properties']
                if str(properties['sheetId']) == tab_id:
                    return properties['title']
            return None

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        sheet_info = self.get_sheet_info(gsheet_id)
        tab_title = get_tab_title(tab_id, sheet_info)
        if tab_title is None:
            raise GsheetError(
                f'Unable to find tab with id {tab_id} in gsheet {url}'
            )

        interp.stack_push(gsheet_id)
        interp.stack_push(tab_title)

    # (url -- sheet_id tab_id)
    @raises_ExpiredGsheetOAuthToken
    def word_URL_to_SHEET_ID_slash_TAB_ID(self, interp: IInterpreter):
        url = interp.stack_pop()
        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        interp.stack_push(gsheet_id)
        interp.stack_push(tab_id)

    # ( gsheet_id range -- rows )
    @raises_ExpiredGsheetOAuthToken
    def word_ROWS(self, interp: IInterpreter):
        spreadsheet_range = interp.stack_pop()
        spreadsheet_id = interp.stack_pop()
        result = self.get_rows(spreadsheet_id, spreadsheet_range)
        interp.stack_push(result)

    # ( gsheet_id range rows -- )
    @raises_ExpiredGsheetOAuthToken
    def word_ROWS_bang(self, interp: IInterpreter):
        rows = interp.stack_pop()
        spreadsheet_range = interp.stack_pop()
        spreadsheet_id = interp.stack_pop()

        context = self.get_context()
        spreadsheet_range_url_encoded = urllib.parse.quote_plus(
            spreadsheet_range
        )

        gsheets_session = self.get_gsheets_session()
        update_data = {
            'range': spreadsheet_range,
            'majorDimension': 'ROWS',
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
            raise GsheetError(
                f'Problem writing to gsheet {spreadsheet_id} {spreadsheet_range}: {status.text}'
            )

    # ( gsheet_id range columns -- )
    @raises_ExpiredGsheetOAuthToken
    def word_COLUMNS_bang(self, interp: IInterpreter):
        columns = interp.stack_pop()
        spreadsheet_range = interp.stack_pop()
        spreadsheet_id = interp.stack_pop()

        context = self.get_context()
        spreadsheet_range_url_encoded = urllib.parse.quote_plus(
            spreadsheet_range
        )

        gsheets_session = self.get_gsheets_session()
        update_data = {
            'range': spreadsheet_range,
            'majorDimension': 'COLUMNS',
            'values': columns,
        }
        input_option = 'USER_ENTERED'
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{spreadsheet_id}/values/{spreadsheet_range_url_encoded}?valueInputOption={input_option}'
        status = gsheets_session.put(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem writing to gsheet {spreadsheet_id} {spreadsheet_range}: {status.text}'
            )

    # ( gsheet_id range header -- Records )
    @raises_ExpiredGsheetOAuthToken
    def word_RECORDS(self, interp: IInterpreter):
        header = interp.stack_pop()
        spreadsheet_range = interp.stack_pop()
        spreadsheet_id = interp.stack_pop()
        rows = self.get_rows(spreadsheet_id, spreadsheet_range)

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
                f"Can't find header ({header}) in gsheet {spreadsheet_id} {spreadsheet_range}"
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

    # ( url -- conditional_formats )
    @raises_ExpiredGsheetOAuthToken
    def word_CONDITIONAL_FORMATS(self, interp: IInterpreter):
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()
        gsheets_session = self.get_gsheets_session()

        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}?fields=sheets(properties(title,sheetId),conditionalFormats)'
        response = gsheets_session.get(api_url, proxies=context.get_proxies())
        if not response.ok:
            raise GsheetError(
                f'Problem getting conditional formats for gsheet {gsheet_id}: {response.text}'
            )

        formats_by_sheet_id = {}
        data = response.json()
        for sheet in data['sheets']:
            sheet_id = str(sheet['properties']['sheetId'])
            conditional_formats = sheet.get('conditionalFormats') or []
            formats_by_sheet_id[sheet_id] = conditional_formats

        result = formats_by_sheet_id[str(tab_id)]
        interp.stack_push(result)

    # ( url -- filters )
    @raises_ExpiredGsheetOAuthToken
    def word_FILTERS(self, interp: IInterpreter):
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()
        gsheets_session = self.get_gsheets_session()

        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}?fields=sheets(properties(title,sheetId),filterViews)'
        response = gsheets_session.get(api_url, proxies=context.get_proxies())
        if not response.ok:
            raise GsheetError(
                f'Problem getting conditional formats for gsheet {gsheet_id}: {response.text}'
            )

        filters_by_sheet_id = {}
        data = response.json()
        for sheet in data['sheets']:
            sheet_id = str(sheet['properties']['sheetId'])
            filters = sheet.get('filterViews') or []
            filters_by_sheet_id[sheet_id] = filters

        result = filters_by_sheet_id[str(tab_id)]
        interp.stack_push(result)

    # ( url end_row -- )
    @raises_ExpiredGsheetOAuthToken
    def word_UPDATE_ALL_FILTER_END_ROWS(self, interp: IInterpreter):
        """Updates the end row of all filters in the sheet to the specified 1-based end_row"""
        end_row = interp.stack_pop()
        url = interp.stack_pop()

        # Get filters on sheet
        interp.stack_push(url)
        interp.run('FILTERS')
        filters = interp.stack_pop()

        if not filters:
            return

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()
        gsheets_session = self.get_gsheets_session()

        # This constructs a record used to update a filter
        def make_update_record(g_filter: Dict[str, Any]) -> Dict[str, Any]:
            g_range = g_filter['range']
            g_range['sheetId'] = tab_id
            g_range[
                'endRowIndex'
            ] = end_row   # gsheet API end row is not inclusive, so go 1 past the desired 0-based index
            res = {
                'updateFilterView': {
                    'filter': {
                        'filterViewId': g_filter['filterViewId'],
                        'range': g_range,
                    },
                    'fields': {'paths': ['*']},
                }
            }
            return res

        requests = [make_update_record(f) for f in filters]
        update_data = {'requests': requests}
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )

        if not status.ok:
            raise GsheetError(
                f'Problem updating all filter views {gsheet_id}: {status.text}'
            )

    # ( url -- )
    @raises_ExpiredGsheetOAuthToken
    def word_DELETE_CONDITIONAL_FORMATS(self, interp: IInterpreter):
        url = interp.stack_pop()

        interp.stack_push(url)
        interp.run('CONDITIONAL-FORMATS')
        conditional_formats = interp.stack_pop()

        if not conditional_formats:
            return

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()
        gsheets_session = self.get_gsheets_session()

        def make_delete_request() -> Dict[str, Any]:
            res = {
                'deleteConditionalFormatRule': {'sheetId': tab_id, 'index': 0}
            }
            return res

        requests = []
        for _ in conditional_formats:
            requests.append(make_delete_request())

        update_data = {'requests': requests}
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem deleting conditional formats {gsheet_id}: {status.text}'
            )

    # ( url Range Format -- )
    @raises_ExpiredGsheetOAuthToken
    def word_REPEAT_CELL_FORMATS(self, interp: IInterpreter):
        g_format = interp.stack_pop()
        g_range = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()
        gsheets_session = self.get_gsheets_session()

        # Matching this in the API -- "fields": "userEnteredFormat(backgroundColor,textFormat,horizontalAlignment)"
        fields = f"userEnteredFormat({','.join(g_format.get_data().keys())})"
        update_data = {
            'requests': [
                {
                    'repeatCell': {
                        'range': g_range.get_data(),
                        'cell': {'userEnteredFormat': g_format.get_data()},
                        'fields': f'{fields}',
                    }
                },
            ]
        }
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem repeating cell formats {gsheet_id}: {status.text}'
            )

    # ( url update_requests -- )
    @raises_ExpiredGsheetOAuthToken
    def word_BATCH_UPDATE(self, interp: IInterpreter):
        """Performs a batch update to a gsheet
        Params:
        * `url`: gsheet URL
        * `update_requests`: An array of objects with a single key being the gsheet operation to perform
                             (e.g., updateCells, deleteDimension, etc.) and with a value corresponding to
                             the operation. If the value requires a range, the `sheetId` will be filled out
                             by this method
        """
        update_requests = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        gsheets_session = self.get_gsheets_session()

        def add_sheet_id(update_requests):
            for r in update_requests:
                for v in r.values():
                    if 'range' in v:
                        v['range']['sheetId'] = tab_id
            return

        add_sheet_id(update_requests)
        update_data = {
            'requests': update_requests
        }

        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem running BATCH-UPDATE {gsheet_id}: {status.text}'
            )

    # ( url tab_title update_requests -- )
    @raises_ExpiredGsheetOAuthToken
    def word_BATCH_UPDATE_TAB(self, interp: IInterpreter):
        """Performs a batch update to a gsheet
        Params:
        * `url`: gsheet URL
        * `tab_title`: Title of tab to update
        * `update_requests`: An array of objects with a single key being the gsheet operation to perform
                             (e.g., updateCells, deleteDimension, etc.) and with a value corresponding to
                             the operation. If the value requires a range, the `sheetId` will be filled out
                             by this method
        """
        update_requests = interp.stack_pop()
        tab_title = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, _ = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        def get_sheet_id(gsheet_id, tab_title):
            properties = self.get_spreadsheet_properties(gsheet_id)
            sheets = properties.get('sheets')
            if not sheets:
                return None
            for s in sheets:
                if s['properties']['title'] == tab_title:
                    return s["properties"]["sheetId"]
            return None

        gsheet_id, _ = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        sheet_id = get_sheet_id(gsheet_id, tab_title)
        if (sheet_id is None):
            raise GsheetError(f"Can't find tab '{tab_title}' for gsheet {gsheet_id}")

        gsheets_session = self.get_gsheets_session()

        def add_sheet_id(update_requests):
            for r in update_requests:
                for v in r.values():
                    if 'range' in v:
                        v['range']['sheetId'] = sheet_id
            return

        add_sheet_id(update_requests)
        update_data = {
            'requests': update_requests
        }

        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem running BATCH-UPDATE-TAB {gsheet_id}: {status.text}'
            )

    # ( url -- )
    @raises_ExpiredGsheetOAuthToken
    def word_CLEAR_SHEET_bang(self, interp: IInterpreter):
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        gsheets_session = self.get_gsheets_session()
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
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem clearing gsheet {gsheet_id}: {status.text}'
            )

    # ( url tabname -- )
    @raises_ExpiredGsheetOAuthToken
    def word_CLEAR_TAB_bang(self, interp: IInterpreter):
        tabname = interp.stack_pop()
        url = interp.stack_pop()

        context = self.get_context()
        gsheet_id, _ = self.get_gsheet_id_and_tab_id(url)

        sheet_info = self.get_sheet_info(gsheet_id)

        def get_tab_id(sheet_info, tabname):
            res = None
            for s in sheet_info['sheets']:
                if s['properties']['title'] == tabname:
                    return s['properties']['sheetId']
            return res

        tab_id = get_tab_id(sheet_info, tabname)

        gsheets_session = self.get_gsheets_session()
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
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem clearing tab {tabname} from gsheet {gsheet_id}: {status.text}'
            )

    # ( url title -- )
    @raises_ExpiredGsheetOAuthToken
    def word_ENSURE_TAB_bang(self, interp: IInterpreter):
        title = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, _ = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        def does_tab_exist(gsheet_id, tab_title):
            properties = self.get_spreadsheet_properties(gsheet_id)
            sheets = properties.get('sheets')
            if not sheets:
                return False
            for s in sheets:
                if s['properties']['title'] == tab_title:
                    return True
            return False

        # If tab exists, return its title
        if does_tab_exist(gsheet_id, title):
            return

        # Otherwise, add tab and return the new tab ID
        gsheets_session = self.get_gsheets_session()
        update_data = {
            'requests': [
                {
                    'addSheet': {
                        'properties': {
                            'title': title
                        }
                    }
                },
            ]
        }
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem adding sheet to gsheet {gsheet_id}: {status.text}'
            )

    # ( url col_start row_start col_end row_end -- GsheetRange )
    #  col_start/col_end are letters
    #  row_start/row_end are 1-based numbers
    @raises_ExpiredGsheetOAuthToken
    def word_RANGE(self, interp: IInterpreter):
        row_end = interp.stack_pop()
        col_end = interp.stack_pop()
        row_start = interp.stack_pop()
        col_start = interp.stack_pop()
        url = interp.stack_pop()

        _gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        result = GsheetRange(tab_id, col_start, col_end, row_start, row_end)
        interp.stack_push(result)

    # ( color_rec -- GsheetColor )
    # color_rec has optional fields: red, green, blue, alpha
    # Values are 0-255
    def word_COLOR(self, interp: IInterpreter):
        color_rec = interp.stack_pop()
        result = GsheetColor(color_rec)
        interp.stack_push(result)

    # ( type value -- GsheetCondition )
    def word_CONDITION(self, interp: IInterpreter):
        value = interp.stack_pop()
        condition_type = interp.stack_pop()
        result = GsheetCondition(condition_type, value)
        interp.stack_push(result)

    # ( -- GsheetFormat )
    def word_FORMAT(self, interp: IInterpreter):
        result = GsheetFormat()
        interp.stack_push(result)

    # ( -- GsheetBorder )
    def word_BORDER(self, interp: IInterpreter):
        result = GsheetBorder()
        interp.stack_push(result)

    # ( condition format -- GsheetBooleanRule )
    def word_BOOLEAN_RULE(self, interp: IInterpreter):
        g_format = interp.stack_pop()
        g_condition = interp.stack_pop()
        result = GsheetBooleanRule(g_condition, g_format)
        interp.stack_push(result)

    # ( GsheetFormat GsheetColor -- GsheetFormat )
    def word_l_BACKGROUND_COLOR(self, interp: IInterpreter):
        color = interp.stack_pop()
        result = interp.stack_pop()
        result['backgroundColor'] = color.get_data()
        interp.stack_push(result)

    # ( GsheetFormat GsheetColor -- GsheetFormat )
    def word_l_FOREGROUND_COLOR(self, interp: IInterpreter):
        color = interp.stack_pop()
        result = interp.stack_pop()
        result['foregroundColor'] = color.get_data()
        interp.stack_push(result)

    # ( GsheetFormat bool -- GsheetFormat )
    def word_l_BOLD(self, interp: IInterpreter):
        is_bold = interp.stack_pop()
        result = interp.stack_pop()
        result['bold'] = is_bold
        interp.stack_push(result)

    # ( GsheetFormat text_format -- GsheetFormat )
    def word_l_TEXT_FORMAT(self, interp: IInterpreter):
        text_format = interp.stack_pop()
        result = interp.stack_pop()
        result['textFormat'] = text_format.get_data()
        interp.stack_push(result)

    # ( url Range Rules -- )
    @raises_ExpiredGsheetOAuthToken
    def word_ADD_CONDITIONAL_FORMAT_RULES(self, interp: IInterpreter):
        g_rules = interp.stack_pop()
        g_range = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        gsheets_session = self.get_gsheets_session()

        def make_conditional_format_rule(rule: 'GsheetBooleanRule'):
            res = {
                'addConditionalFormatRule': {
                    'rule': {
                        'ranges': [g_range.get_data()],
                        'booleanRule': rule.get_data(),
                    },
                    'index': 0,
                }
            }
            return res

        conditional_format_rules = [
            make_conditional_format_rule(r) for r in g_rules
        ]
        update_data = {'requests': conditional_format_rules}
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem adding conditional formatting to gsheet {url}: {status.text}'
            )

    # ( url range border_rec -- )
    # border_rec has fields (all optional): top, bottom, left, right
    # The values of this record are GsheetBorder objects
    @raises_ExpiredGsheetOAuthToken
    def word_UPDATE_BORDERS(self, interp: IInterpreter):
        border_rec = interp.stack_pop()
        g_range = interp.stack_pop()
        url = interp.stack_pop()

        gsheet_id, tab_id = self.get_gsheet_id_and_tab_id(url)
        context = self.get_context()

        gsheets_session = self.get_gsheets_session()

        update_borders_rec = {'range': g_range.get_data()}
        for side, border in border_rec.items():
            update_borders_rec[side] = border.get_data()

        update_data = {'requests': [{'updateBorders': update_borders_rec}]}
        api_url = f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}:batchUpdate'
        status = gsheets_session.post(
            api_url,
            data=json.dumps(update_data),
            proxies=context.get_proxies(),
        )
        if not status.ok:
            raise GsheetError(
                f'Problem adding conditional formatting to gsheet {url}: {status.text}'
            )

    # ( index -- col_name )
    def word_INDEX_to_COL_NAME(self, interp: IInterpreter):
        # We're converting an integer into "base 26" using letters only
        index = interp.stack_pop()
        result = index_to_col_name(index)
        interp.stack_push(result)

    # ( col_name -- index )
    def word_COL_NAME_to_INDEX(self, interp: IInterpreter):
        col_name = interp.stack_pop()
        result = col_name_to_index(col_name)
        interp.stack_push(result)

    # =================================
    # Helpers

    def get_gsheet_id_and_tab_id(self, url: str) -> Tuple[str, str]:
        match = re.match(r'.*docs\.google\.com.*\/d\/([^\/]+).*gid=(\d+)', url)
        if not match:
            raise GsheetError(
                f'Unable to find gsheet_id and tab key from: {url}'
            )
        gsheet_id = match.group(1)
        tab_id = match.group(2)
        return gsheet_id, tab_id

    def get_sheet_info(self, gsheet_id: str) -> Any:
        gsheets_session = self.get_gsheets_session()
        context = self.get_context()
        result = gsheets_session.get(
            f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}',
            proxies=context.get_proxies(),
        ).json()
        return result

    def get_spreadsheet_properties(self, gsheet_id: str) -> Any:
        gsheets_session = self.get_gsheets_session()
        context = self.get_context()
        result = gsheets_session.get(
            f'https://sheets.googleapis.com/v4/spreadsheets/{gsheet_id}?fields=sheets.properties',
            proxies=context.get_proxies(),
        ).json()
        return result

    def get_rows(self, spreadsheet_id: str, spreadsheet_range: str) -> List[List[str]]:
        spreadsheet_range_url_encoded = urllib.parse.quote_plus(
            spreadsheet_range
        )
        gsheets_session = self.get_gsheets_session()
        majorDimension = 'ROWS'
        base = 'https://sheets.googleapis.com/v4/spreadsheets'
        context = self.get_context()
        api_url = f"{base}/{spreadsheet_id}/values/'{spreadsheet_range_url_encoded}'?majorDimension={majorDimension}"
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

    def get_context(self) -> 'CredsContext':
        if not self.context_stack:
            raise GsheetError(
                'Use gsheet.PUSH-CONTEXT! to provide a Google context'
            )
        result = self.context_stack[-1]
        return result

    def get_gsheets_session(self) -> OAuth2Session:
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


# ------------------------------------------------
# Helper functions and classes


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


# ------------------------------------------------
# Classes to interact with batch update


class GsheetElement:
    def __init__(self):
        self.data: Dict[str, Any] = {}

    def get_data(self):
        return self.data

    def __getitem__(self, key: str) -> Any:
        return self.data.get(key)

    def __setitem__(self, key: str, value: Any):
        self.data[key] = value


class GsheetRange(GsheetElement):
    def __init__(self, tab_id: str, col_start: str, col_end: str, row_start: int, row_end: int):
        """col_start/col_end: Column letters (inclusive)
        row_start/row_end: 1-based row numbers (inclusive)
        """
        super().__init__()
        self.data['sheetId'] = tab_id
        self.data['startRowIndex'] = row_start - 1
        self.data[
            'endRowIndex'
        ] = row_end                            # API range does not include end, so specifying next row
        self.data['startColumnIndex'] = col_name_to_index(col_start)
        self.data['endColumnIndex'] = (
            col_name_to_index(col_end) + 1
        )  # API range does not include end, so specifying next col


class GsheetColor(GsheetElement):
    def __init__(self, color_rec: Dict[str, int]):
        """color_rec has fields, all optional: red, green, blue, alpha
        The range of the fields is 0-255
        """
        super().__init__()
        for color in ['red', 'green', 'blue', 'alpha']:
            value = color_rec.get(color)
            if value:
                self.data[color] = self.normalize_color(value)
            elif color == 'alpha' and not value:
                self.data['alpha'] = 1.0

    def normalize_color(self, color: int) -> float:
        result = color / 255.0
        return result


class GsheetCondition(GsheetElement):
    def __init__(self, type, value):
        """
        `type` is something like TEXT_CONTAINS or DATE_BEFORE
        `value` is a structure like
        ```
            {
                "userEnteredValue": "Red"
            }
        ```
        See https://developers.google.com/sheets/api/samples/conditional-formatting for more info
        """
        super().__init__()
        self.data['type'] = type
        self.data['values'] = [value]


class GsheetBooleanRule(GsheetElement):
    def __init__(self, gsheet_condition, gsheet_format):
        super().__init__()
        self.data['condition'] = gsheet_condition.get_data()
        self.data['format'] = gsheet_format.get_data()


class GsheetFormat(GsheetElement):
    pass


class GsheetBorder(GsheetElement):
    def __init__(self):
        super().__init__()
        self.data = {
            'style': 'SOLID',
            'width': 1,
        }
