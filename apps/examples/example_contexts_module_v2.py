"""Provides contexts for modules that interact with external services/data
"""
from forthic.v2.module import Module
from forthic.v2.modules import (
    jira_module,
    gsheet_module,
    excel_module,
    confluence_module,
)
from forthic.utils.creds import Creds

CONTEXTS = {}

SECRETS_DIR = '..'


class ExampleContextsModuleV2(Module):
    def __init__(self, interp):
        super().__init__('example_contexts', interp, FORTHIC)
        self.add_module_word('JIRA-PROD', self.word_JIRA_PROD)
        self.add_module_word('JIRA-STG', self.word_JIRA_STG)
        self.add_module_word('CONFLUENCE', self.word_CONFLUENCE)
        self.add_module_word('GOOGLE', self.word_GOOGLE)
        self.add_module_word('MSGRAPH', self.word_MSGRAPH)

    # ( -- JiraContext )
    def word_JIRA_PROD(self, interp):
        """Returns JiraContext based on JIRA creds in the .secrets file"""
        result = self.get_jira_context('JIRA')
        interp.stack_push(result)

    # ( -- JiraContext )
    def word_JIRA_STG(self, interp):
        """Returns JiraContext based on JIRA_STG creds in the .secrets file"""
        result = self.get_jira_context('JIRA_STG')
        interp.stack_push(result)

    # ( -- ConfluenceContext )
    def word_CONFLUENCE(self, interp):
        """Returns ConfluenceContext based on CONFLUENCE creds in the .secrets file"""
        result = ConfluenceContext('CONFLUENCE')
        interp.stack_push(result)

    def word_GOOGLE(self, interp):
        creds = Creds(SECRETS_DIR)
        app_creds = creds.get_app_creds('GOOGLE_APP')
        auth_token = creds.get_oauth_token('GOOGLE_TOKEN')

        class GoogleCredsContext(gsheet_module.CredsContext):
            def get_app_creds(self):
                return app_creds

            def get_auth_token(self):
                return auth_token

        interp.stack_push(GoogleCredsContext())

    def word_MSGRAPH(self, interp):
        creds = Creds(SECRETS_DIR)
        app_creds = creds.get_app_creds('MSGRAPH_APP')
        auth_token = creds.get_oauth_token('MSGRAPH_TOKEN')

        class MSGraphCredsContext(excel_module.CredsContext):
            def get_app_creds(self):
                return app_creds

            def get_auth_token(self):
                return auth_token

        interp.stack_push(MSGraphCredsContext())

    # ----------------------------------
    # Helpers
    def get_jira_context(self, key):
        if key in CONTEXTS:
            result = CONTEXTS[key]
        else:
            result = JiraContext(key)
            CONTEXTS[key] = result
        return result


class JiraContext(jira_module.JiraContext):
    def __init__(self, field):
        self.field = field
        creds = Creds(SECRETS_DIR)
        self.app_creds = creds.get_password_creds(field)
        super().__init__()

    def get_field(self):
        return self.field

    def get_host(self):
        return self.app_creds['host']

    def get_username(self):
        return self.app_creds['username']

    def get_password(self):
        return self.app_creds['password']

    # Uncomment to verify ssl certs in REST calls
    # def get_cert_verify(self):
    #     return "/export/apps/openssl/ssl/cert.pem"


class ConfluenceContext(confluence_module.ConfluenceContext):
    def __init__(self, field):
        self.field = field
        creds = Creds(SECRETS_DIR)
        self.app_creds = creds.get_password_creds(field)
        super().__init__()

    def get_field(self):
        return self.field

    def get_host(self):
        return self.app_creds['host']

    def get_username(self):
        return self.app_creds['username']

    def get_password(self):
        return self.app_creds['password']

    # Uncomment to verify ssl certs in REST calls
    # def get_cert_verify(self):
    #     return "/export/apps/openssl/ssl/cert.pem"


FORTHIC = ''
