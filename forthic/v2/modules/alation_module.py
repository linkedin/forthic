import requests
import csv

from ..module import Module
from ..interfaces import IInterpreter
from typing import List


class InvalidAlationCreds(RuntimeError):
    def __init__(self, field, host):
        super().__init__(f'Invalid field: {field}')
        self.field = field
        self.host = host


class AlationError(RuntimeError):
    pass


class AlationModule(Module):
    """Adds support for working with Alation

    This adds basic support for working with Alation:

    * Updating/clearing refresh tokens
    * Accessing SQL queries
    * Accessing query results
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('alation', interp, ALATION_FORTHIC)
        self.context_stack: List['AlationCredsContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)

        self.add_module_word('QUERY-SQL', self.word_QUERY_SQL)
        self.add_module_word('QUERY-RESULT-INFO', self.word_QUERY_RESULT_INFO)
        self.add_module_word('QUERY-RESULT', self.word_QUERY_RESULT)
        self.add_module_word('UPDATE-REFRESH-TOKEN', self.word_UPDATE_REFRESH_TOKEN)
        self.add_module_word('DELETE-CREDS', self.word_DELETE_CREDS)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( query_id -- sql )
    def word_QUERY_SQL(self, interp: IInterpreter):
        query_id = interp.stack_pop()
        context = self.get_context()
        access_token = self.get_access_token()
        headers = {'Token': access_token}
        url = f'https://{context.get_host()}/integration/v1/query/{query_id}/sql/'
        response = requests.get(
            url, headers=headers, verify=context.get_cert_verify()
        )

        if not response.ok:
            raise AlationError(f'QUERY-SQL failed: {response.text}')

        interp.stack_push(response.text)

    # ( query_id -- result_info )
    def word_QUERY_RESULT_INFO(self, interp: IInterpreter):
        """Returns the last query result ID"""
        query_id = interp.stack_pop()
        context = self.get_context()
        access_token = self.get_access_token()
        headers = {'Token': access_token}
        url = f'https://{context.get_host()}/integration/v1/query/{query_id}/result/latest'
        response = requests.get(
            url, headers=headers, verify=context.get_cert_verify()
        )

        if not response.ok:
            raise AlationError(f'QUERY-RESULT-ID failed: {response.text}')

        result = response.json()
        interp.stack_push(result)

    # ( result_id -- records )
    def word_QUERY_RESULT(self, interp: IInterpreter):
        """Returns result for the given result_id"""
        result_id = interp.stack_pop()
        context = self.get_context()
        access_token = self.get_access_token()
        headers = {'Token': access_token}
        url = f'https://{context.get_host()}/integration/v1/result/{result_id}/csv'
        response = requests.get(
            url, headers=headers, verify=context.get_cert_verify()
        )

        if not response.ok:
            raise AlationError(f'QUERY-RESULT-ID failed: {response.text}')

        decoded_content = response.content.decode('utf-8')
        csv_reader = csv.DictReader(
            decoded_content.splitlines(), delimiter=','
        )
        result = list(csv_reader)
        interp.stack_push(result)

    # ( -- )
    def word_UPDATE_REFRESH_TOKEN(self, interp: IInterpreter):
        """Regenerates Alation refresh token for current user, updating current Alation context and database

        NOTE: After calling this, the previous token will become invalid!
        """
        context = self.get_context()

        data = {
            'refresh_token': context.get_refresh_token(),
            'user_id': context.get_user_id(),
        }

        response = requests.post(
            f'https://{context.get_host()}/integration/v1/regenRefreshToken/',
            data=data,
            verify=context.get_cert_verify(),
        )

        if not response.ok:
            raise AlationError(f'REGEN-REFRESH-TOKEN failed: {response.text}')

        # Update current context
        context.update_token_info(response.json())

    # ( -- )
    def word_DELETE_CREDS(self, interp: IInterpreter):
        """Deletes Alation creds
        """
        context = self.get_context()
        context.delete_creds()

    # =================================
    # Helpers

    def get_context(self):
        if not self.context_stack:
            raise AlationError(
                'Need to push an AlationCredsContext with PUSH-CONTEXT!'
            )
        result = self.context_stack[-1]
        return result

    def get_access_token(self):
        context = self.get_context()
        data = {
            'refresh_token': context.get_refresh_token(),
            'user_id': context.get_user_id(),
        }

        url = f'https://{context.get_host()}/integration/v1/createAPIAccessToken/'
        response = requests.post(
            url, data=data, verify=context.get_cert_verify()
        )

        if not response.ok:
            raise InvalidAlationCreds(context.get_field(), context.get_host())

        result = response.json()['api_access_token']
        return result


class AlationCredsContext:
    """Clients of the alation module must extend CredsContext and use PUSH-CONTEXT!
    in order to set the current creds context"""

    def update_token_info(self, token_info):
        self.token_info = token_info

    def delete_creds(self):
        """Use this to clear out Alation creds"""
        pass

    def get_host(self):
        return None

    def get_field(self):
        return None

    def get_proxies(self):
        """Returns a dict object containing proxies for fields 'http' and 'https'"""
        return None

    def get_user_id(self):
        return None

    def get_refresh_token(self):
        return None

    def get_cert_verify(self):
        return False


ALATION_FORTHIC = '''
'''
