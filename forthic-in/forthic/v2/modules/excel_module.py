import base64
import json
import oauthlib.oauth2.rfc6749.errors
from requests_oauthlib import OAuth2Session   # type: ignore
from ..module import Module
from ..interfaces import IInterpreter
from ...utils.errors import (
    ExpiredMSGraphOAuthToken,
    ExcelError
)
from typing import List


def raises_ExpiredMSGraphOAuthToken(fn):
    """Decorator that catches expiration errors and raises ExpiredMSGraphOAuthToken instead"""
    def wrapper(*args, **kwargs):
        res = None
        try:
            res = fn(*args, **kwargs)
        except (oauthlib.oauth2.rfc6749.errors.TokenExpiredError, oauthlib.oauth2.rfc6749.errors.InvalidGrantError):
            raise ExpiredMSGraphOAuthToken()
        return res
    return wrapper


class ExcelModule(Module):
    """This implements basic access to Excel via MS Graph

    See `docs/modules/excel_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('excel', interp, EXCEL_FORTHIC)
        self.context_stack: List['CredsContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)

        self.add_module_word('WORKBOOK-INFO', self.word_WORKBOOK_INFO)
        self.add_module_word('SHEET-NAMES', self.word_SHEET_NAMES)
        self.add_module_word('TABLE-NAMES', self.word_TABLE_NAMES)

        self.add_module_word('TABLE-RECORDS', self.word_TABLE_RECORDS)
        self.add_module_word('ADD-TABLE-ROWS', self.word_ADD_TABLE_ROWS)
        self.add_module_word('UPDATE-RANGE', self.word_UPDATE_RANGE)
        self.add_module_word("USED-RANGE", self.word_USED_RANGE)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # (shared_url -- doc_info)
    @raises_ExpiredMSGraphOAuthToken
    def word_WORKBOOK_INFO(self, interp: IInterpreter):
        shared_url = interp.stack_pop()
        msgraph_session = self.get_msgraph_session()

        # See https://docs.microsoft.com/en-us/graph/api/shares-get?view=graph-rest-1.0&tabs=http
        def get_encoded_url() -> str:
            encoded_url = base64.b64encode(shared_url.encode()).decode('utf-8')
            res = 'u!' + encoded_url.strip('=').replace('/', '_').replace(
                '+', '-'
            )
            return res

        context = self.get_context()
        api_url = (
            f'https://graph.microsoft.com/v1.0/shares/{get_encoded_url()}/root'
        )
        response = msgraph_session.get(api_url, proxies=context.get_proxies())
        data = response.json()
        result = {
            'drive_id': data['parentReference']['driveId'],
            'item_id': data['id'],
        }
        interp.stack_push(result)

    # (workbook_info -- names)
    @raises_ExpiredMSGraphOAuthToken
    def word_SHEET_NAMES(self, interp: IInterpreter):
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )

        api_url = f'https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets'
        headers = {'workbook-session-id': workbook_session_id}
        context = self.get_context()
        response = msgraph_session.get(
            api_url, headers=headers, proxies=context.get_proxies()
        )
        if response.status_code != 200:
            raise ExcelError(
                f'Unable to get sheet names for {item_id}: {response.text}'
            )

        data = response.json()
        result = [item['name'] for item in data['value']]
        interp.stack_push(result)

    # (workbook_info sheet_name -- names)
    @raises_ExpiredMSGraphOAuthToken
    def word_TABLE_NAMES(self, interp: IInterpreter):
        sheet_name = interp.stack_pop()
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )

        api_url = f'https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets/{sheet_name}/tables'
        headers = {'workbook-session-id': workbook_session_id}
        context = self.get_context()
        response = msgraph_session.get(
            api_url, headers=headers, proxies=context.get_proxies()
        )
        if response.status_code != 200:
            raise ExcelError(
                f'Unable to get table names for {item_id}/{sheet_name}: {response.text}'
            )

        data = response.json()
        result = [item['name'] for item in data['value']]
        interp.stack_push(result)

    # (workbook_info sheet_name table_name -- records)
    @raises_ExpiredMSGraphOAuthToken
    def word_TABLE_RECORDS(self, interp: IInterpreter):
        table_name = interp.stack_pop()
        sheet_name = interp.stack_pop()
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )

        def get_table_columns():
            api_url = f'https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets/{sheet_name}/tables/{table_name}/columns'
            headers = {'workbook-session-id': workbook_session_id}
            context = self.get_context()
            response = msgraph_session.get(
                api_url, headers=headers, proxies=context.get_proxies()
            )
            data = response.json()
            res = []
            for item in data['value']:
                col_vals = []
                for v in item['values']:
                    col_vals.append(v[0])
                res.append(col_vals)
            return res

        def columns_to_records(columns):
            if len(columns) == 0:
                return []

            # Set up result
            res = []
            num_records = (
                len(columns[0]) - 1
            )  # Don't count heading as a record
            for _ in range(num_records):
                res.append({})

            # Store values
            for col in columns:
                field = col[0]
                values = col[1:]
                for i in range(len(values)):
                    res[i][field] = values[i]
            return res

        # Pull the data and convert it into records
        table_columns = get_table_columns()
        result = columns_to_records(table_columns)
        interp.stack_push(result)

    # (workbook_info sheet_name table_name rows -- )
    @raises_ExpiredMSGraphOAuthToken
    def word_ADD_TABLE_ROWS(self, interp: IInterpreter):
        rows = interp.stack_pop()
        table_name = interp.stack_pop()
        sheet_name = interp.stack_pop()
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )

        api_url = f'https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets/{sheet_name}/tables/{table_name}/rows'
        headers = {'workbook-session-id': workbook_session_id}
        data = {'values': rows}
        context = self.get_context()
        response = msgraph_session.post(
            api_url, json=data, headers=headers, proxies=context.get_proxies()
        )
        if response.status_code != 201:
            raise RuntimeError(
                f'Unable to add table rows to {item_id}/{sheet_name}/{table_name}: {response.text}'
            )

    # (workbook_info sheet_name range rows -- )
    @raises_ExpiredMSGraphOAuthToken
    def word_UPDATE_RANGE(self, interp: IInterpreter):
        rows = interp.stack_pop()
        a1_range = interp.stack_pop()
        sheet_name = interp.stack_pop()
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )

        api_url = f"https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets/{sheet_name}/range(address='{a1_range}')"
        headers = {'workbook-session-id': workbook_session_id}
        data = {'values': rows}
        context = self.get_context()
        response = msgraph_session.patch(
            api_url, json=data, headers=headers, proxies=context.get_proxies()
        )
        if response.status_code != 200:
            raise ExcelError(
                f'Unable to update range {item_id}/{sheet_name}/{a1_range}: {response.text}'
            )

    # (workbook_info sheet_name -- rows)
    @raises_ExpiredMSGraphOAuthToken
    def word_USED_RANGE(self, interp: IInterpreter):
        sheet_name = interp.stack_pop()
        workbook_info = interp.stack_pop()
        drive_id = workbook_info['drive_id']
        item_id = workbook_info['item_id']

        msgraph_session = self.get_msgraph_session()
        workbook_session_id = self.get_workbook_session_id(
            drive_id, item_id, msgraph_session
        )
        api_url = f"https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/worksheets/{sheet_name}/usedRange"
        headers = {
            "workbook-session-id": workbook_session_id
        }
        response = msgraph_session.get(api_url, headers=headers)
        if response.status_code != 200:
            raise RuntimeError(f"Unable to get used range {item_id}/{sheet_name}: {response}")
        data = response.json()
        result = data.get('values')
        interp.stack_push(result)

    # =================================
    # Helpers

    def get_msgraph_session(self) -> OAuth2Session:
        context = self.get_context()
        app_creds = context.get_app_creds()
        token = context.get_auth_token()

        def token_updater(token):
            pass

        refresh_url = (
            'https://login.microsoftonline.com/common/oauth2/v2.0/token'
        )
        result = OAuth2Session(
            app_creds['client_id'],
            token=token,
            auto_refresh_kwargs=app_creds,
            auto_refresh_url=refresh_url,
            token_updater=token_updater,
        )
        return result

    def get_context(self) -> 'CredsContext':
        if not self.context_stack:
            raise ExcelError(
                'Need to push an MS Graph context with PUSH-CONTEXT!'
            )
        result = self.context_stack[-1]
        return result

    def get_workbook_session_id(self, drive_id: str, item_id: str, msgraph_session: OAuth2Session) -> str:
        api_url = f'https://graph.microsoft.com/v1.0/drives/{drive_id}/items/{item_id}/workbook/createSession'
        request_body = {'persistChanges': True}
        context = self.get_context()
        response = msgraph_session.post(
            api_url,
            data=json.dumps(request_body),
            proxies=context.get_proxies(),
        )
        if response.status_code != 201:
            raise ExcelError(
                f'Unable to get workbook session id for {item_id}: {response.text}'
            )
        result = response.json()['id']
        return result


class CredsContext:
    """Clients of the excel module must provide extend CredsContext and use PUSH-CONTEXT!
    in order to set the current creds context"""

    def get_app_creds(self):
        """Returns an object with the following fields: client_id, client_secret"""
        return None

    def get_proxies(self):
        """Returns a dict object containing proxies for fields 'http' and 'https'"""
        return None

    def get_auth_token(self):
        return None


EXCEL_FORTHIC = '''
: WORKBOOK-ID   WORKBOOK-INFO 'item_id' REC@;   # (shared_url -- workbook_id)

["WORKBOOK-ID"] EXPORT
'''
