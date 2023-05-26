from ..module import Module
from ..interfaces import IInterpreter
from typing import List
import trino
import pandas as pd
import json


class TrinoError(RuntimeError):
    pass


class TrinoModule(Module):
    """This implements a trino client
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('trino', interp, "")
        self.context_stack: List['TrinoContext'] = []

        self.flags = {}
        self.get_flags()

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)
        self.add_module_word('QUERY', self.word_QUERY)

    # ( context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        context = interp.stack_pop()
        self.context_stack.append(context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( query -- result )
    def word_QUERY(self, interp: IInterpreter):
        query = interp.stack_pop()
        context = self.current_context()

        context.connect()
        df = context.query(query)
        result = json.loads(df.to_json())
        context.close()
        interp.stack_push(result)

    # =================================
    # Helpers
    def get_flags(self):
        flags = self.flags.copy()
        self.flags = {}
        return flags

    def current_context(self):
        if not self.context_stack:
            raise RuntimeError('Use trino.PUSH-CONTEXT! to provide a TrinoContext')

        result = self.context_stack[-1]
        return result


class TrinoContext:
    """NOTE: Override this to use"""
    def __init__(self):
        self.conn = None
        self.cursor = None

    def connect(self):
        self.conn = trino.dbapi.connect(
            host=self.get_host(),
            port=self.get_port(),
            user=self.get_username(),
            verify=False,
            catalog=self.get_catalog(),
            http_scheme='https',
            auth=trino.auth.BasicAuthentication(self.get_username(), self.get_password()),)
        self.cursor = self.conn.cursor()

    def close(self):
        self.cursor.close()

    def query(self, q):
        self.cursor.execute(q)
        rows = self.cursor.fetchall()
        result = pd.DataFrame(list(rows))
        result.columns = [desc[0] for desc in self.cursor.description]
        return result

    # =============================
    # Connection settings to override
    def get_field(self):
        return None

    def get_host(self):
        return None

    def get_port(self):
        return None

    def get_catalog(self):
        return None

    def get_username(self):
        return None

    def get_password(self):
        return None

    # Supply the path to the cert file to use. Use False to skip verification
    def get_cert_verify(self):
        return False
