import requests
import urllib
from ..module import Module
from ..interfaces import IInterpreter
from typing import List


class InvalidAirtableCreds(RuntimeError):
    def __init__(self, field, host):
        super().__init__(f'Invalid field: {field}')
        self.field = field
        self.host = host


class AirtableError(RuntimeError):
    pass


MAX_ITERATIONS = 100


class AirtableModule(Module):
    """Adds support for working with Airtable

    This adds basic support for working with Airtable:
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('airtable', interp, FORTHIC)
        self.context_stack: List['AirtableCredsContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)
        self.add_module_word('RECORDS', self.word_RECORDS)

    # ( creds_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        creds_context = interp.stack_pop()
        self.context_stack.append(creds_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( base_id table config -- )
    def word_RECORDS(self, interp: IInterpreter):
        config = interp.stack_pop()
        table = interp.stack_pop()
        base_id = interp.stack_pop()

        def urlencode(string):
            return urllib.parse.quote_plus(string)

        def make_fields_param(value):
            pieces = []
            for v in value:
                pieces.append(f"fields%5B%5D={urlencode(v)}")
            return "&".join(pieces)

        def make_sort_param(records):
            # We're converting records to things like
            # sort[0][field]=TaskID&sort[0][direction]=asc
            pieces = []
            for index, r in enumerate(records):
                pieces.append(f"sort%5B{index}%5D%5Bfield%5D={r['field']}")
                if r.get('direction'):
                    pieces.append(f"sort%5B{index}%5D%5Bdirection%5D={r['direction']}")
            return "&".join(pieces)

        def make_query_param(field, value):
            res = ""
            if field == "fields":
                res = make_fields_param(value)
            elif field == "sort":
                res = make_sort_param(value)
            else:
                res = f"{field}={urlencode(value)}"
            return res

        def construct_query_param_string(config, offset):
            if not config:
                config = {}

            if offset:
                config["offset"] = offset

            if len(config) == 0:
                return ""

            pieces = []
            for field, value in config.items():
                pieces.append(make_query_param(field, value))

            res = f"?{'&'.join(pieces)}"
            return res

        context = self.get_context()

        # We may need to iterate to get all of the records
        def get_records(records=[], offset=None, iterations=1):
            qstring = construct_query_param_string(config, offset)
            api_url = f'/v0/{base_id}/{table}{qstring}'
            response = context.requests_get(api_url)
            if not response.ok:
                raise RuntimeError(f"airtable.RECORDS: Error getting records: {response.reason}")
            data = response.json()

            records.extend(data["records"])
            if iterations > MAX_ITERATIONS:
                raise RuntimeError(f"airtable.RECORDS exceeded {MAX_ITERATIONS} iterations")

            if data.get("offset"):
                get_records(records, data["offset"], iterations + 1)
            return records

        result = get_records()
        interp.stack_push(result)

    # =================================
    # Helpers

    def get_context(self):
        if not self.context_stack:
            raise AirtableError(
                'Need to push an AirtableCredsContext with PUSH-CONTEXT!'
            )
        result = self.context_stack[-1]
        return result


class AirtableCredsContext:
    """Clients of the alation module must extend CredsContext and use PUSH-CONTEXT!
    in order to set the current creds context"""
    def __init__(self, field):
        self.field = field

    def get_host(self):
        return None

    def get_api_token(self):
        return None

    def get_cert_verify(self):
        return False

    def requests_get(self, api_url):
        """Makes HTTP GET call to pull data"""
        api_url_w_host = self.get_host() + api_url
        headers = {
            "Authorization": f"Bearer {self.get_api_token()}"
        }
        result = requests.get(
            api_url_w_host,
            headers=headers,
            verify=self.get_cert_verify(),
        )
        return result


FORTHIC = '''
'''
