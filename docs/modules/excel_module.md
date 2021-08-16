# excel_module

The excel module allows you to interact with Excel spreadsheets hosted in Office
365 via the [MS Graph API](https://docs.microsoft.com/en-us/graph/overview).

In order to use this module, you will need to do register a client application
on Microsoft Azure and go through an OAuth flow to receive an OAuth token. An
example of doing this is in the [example setup](../EXAMPLES.md)

## Example
```
["excel"] USE-MODULES

# NOTE: You must create an Excel `CredsContext` and apply it using PUSH-CONTEXT!

# This stores the workbook info from a specified Excel URL
["workbook_info"] VARIABLES
"https://link-to-excel-workbook" excel.WORKBOOK-INFO workbook_info !

# This returns an array of sheet names in the workbook
workbook_info @ excel.SHEET-NAMES

# This returns an array of table names from the `mytab` tab
workbook_info @ "mytab" excel.TABLE-NAMES

# This returns an array of records from `Table 1` of the `mytab` tab
workbook_info @ "mytab" "Table 1" excel.TABLE-RECORDS
```

## Reference

### PUSH-CONTEXT!
`(context --)`

This pushes a CredsContext (See `excel_module.py`) onto the excel module's context stack.
The most recent context is used to provide credentials to access the MS Graph API.
The CredsContext must be configured in Python.

Here's a simple example:
```
from forthic.interpreter import Interpreter
import forthic.modules.excel_module as excel_module

def get_interp():
    interp = Interpreter()
    interp.set_dev_mode(True)

    def configure_excel_module(interp):
        interp.register_module(excel_module.ExcelModule)

        class ExcelCredsContext(excel_module.CredsContext):
            def get_app_creds(self):
                res = {
                    "client_id": <read this from file>,
                    "client_secret": <read this from file>,
                }
                return res

            def get_auth_token(self):
                res = <read this from file>
                return res

        interp.run("['excel'] USE-MODULES")
        interp.stack_push(ExcelCredsContext())
        interp.run("excel.PUSH-CONTEXT!")
        return

    configure_excel_module(interp)
    return interp
```

### POP-CONTEXT!
`( -- )`

This pops a context from the context stack and throws it away.


### WORKBOOK-INFO
`( shared_url -- doc_info )`

Returns metadata about a workbook from an Excel document url.


### SHEET-NAMES
`( workbook_info -- names )`

Given the workbook_info for an Excel document (from `WORKBOOK-INFO`), returns a
list of its sheet names.


### TABLE-NAMES
`( workbook_info sheet_name -- names )`

Given the workbook_info for an Excel document (from `WORKBOOK-INFO`) and a sheet
name, returns a list of table names.


### TABLE-RECORDS
`( workbook_info sheet_name table_name -- records )`

Given the workbook_info for an Excel document (from `WORKBOOK-INFO`), a sheet
name, and a table name returns an array of records where the fields of each
record correspond to the column headers of the table.


### ADD-TABLE-ROWS
`(workbook_info sheet_name table_name rows -- )`

Adds the specified `rows` to the end of the specified table.


### UPDATE-RANGE
`(workbook_info sheet_name a1_range rows -- )`

Given the workbook_info for an Excel document (from `WORKBOOK-INFO`), a sheet
name, and an `a1_range` (like "A1:E12"), overwrites that range with the
specified `rows`.

### WORKBOOK-ID
`(shared_url -- workbook_id)`

Givena a URL to a workbook, returns its workbook id.
