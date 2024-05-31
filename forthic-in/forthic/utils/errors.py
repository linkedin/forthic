# This gathers errors shared between modules of different Forthic versions to simplify
# exception handling.

class UnauthorizedError(RuntimeError):
    def __init__(self, field):
        super().__init__(f'Unauthorized creds for: {field}')
        self.field = field


# ---------------
# Airtable Errors
class AirtableError(RuntimeError):
    pass


class AirtableUnauthorized(RuntimeError):
    pass


# ---------------
# gdoc errors
class GdocError(RuntimeError):
    pass


class ExpiredGdocOAuthToken(GdocError):
    pass


# ---------------
# gsheet errors
class GsheetError(RuntimeError):
    pass


class ExpiredGsheetOAuthToken(GsheetError):
    pass


# ---------------
# confluence errors
class ConfluenceError(RuntimeError):
    pass


# ---------------
# jira errors
class JiraError(RuntimeError):
    pass


# ---------------
# MS Graph errors
class ExcelError(RuntimeError):
    pass


class ExpiredMSGraphOAuthToken(ExcelError):
    pass


# ---------------
# html module errors
class HtmlModuleError(RuntimeError):
    pass


class InvalidForthicWordError(HtmlModuleError):
    def __init__(self, name):
        super().__init__(f"Expecting a single Forthic word. Not '{name}'")
