# gsheet_module

The gsheet module allows you to interact with Google spreadsheets via the [gsheets API](https://developers.google.com/sheets/api).

In order to use this module, you'll need to register a client application with
Google and go through an OAuth flow to receive an OAuth token. An example of
doing this is in the [example setup](../EXAMPLES.md).

## Example
```
["gsheet"] USE-MODULES
# NOTE: You must create a Google `CredsContext` and apply it using PUSH-CONTEXT!

: GSHEET-URL     "https://docs.google.com/spreadsheets/d/my-spreadsheet-id/edit#gid=0";
@: SPREADSHEET   GSHEET-URL gsheet.SPREADSHEET;
@: TAB           GSHEET-URL gsheet.TAB;

# Read all of the rows from a tab
TAB gsheet.ROWS

# Read a range of rows using the !RANGE flag word
TAB "O1:AC5" gsheet.!RANGE gsheet.ROWS

# Read data as records from a Tab
TAB ["Initiative" "Jira" "Milestone"] gsheet.RECORDS

# Clear a Tab
TAB gsheet.CLEAR!

# Write records to a Tab
: RECORDS [
    [["Latin"  "A"] ["Greek"  "Alpha"]] REC
    [["Latin"  "B"] ["Greek"  "Beta"]] REC
    [["Latin"  "C"] ["Greek"  "Gamma"]] REC
];

TAB ["Greek" "Martian" "Latin"] RECORDS gsheet.RECORDS!

# Write records as columns to a Tab
TAB ["Greek" "Martian" "Latin"] RECORDS gsheet.!TRANSPOSE gsheet.RECORDS!

# Ensure a tab exists
SPREADSHEET "FY23Q4" gsheet.ENSURE-TAB!
```


## Reference

### PUSH-CONTEXT!
`(context --)`

This pushes a CredsContext (See `gsheet_module.py`) onto the gsheet module's
context stack. The most recent context is used to provide credentials to access
the gsheets API. The CredsContext must be configured in Python.

Here's a simple example:
```
def get_interp():
    interp = Interpreter()
    interp.set_dev_mode(True)

    def configure_gsheet_module(interp):
        interp.register_module(gsheet_module.GsheetModule)

        class GSheetCredsContext(gsheet_module.CredsContext):
            def get_app_creds(self):
                res = {
                    "client_id": <read this from file>,
                    "client_secret": <read this from file>,
                }
                return res

            def get_auth_token(self):
                res = <read this from file>
                return res

        interp.run("['gsheet'] USE-MODULES")
        interp.stack_push(GSheetCredsContext())
        interp.run("gsheet.PUSH-CONTEXT!")
        return

    configure_gsheet_module(interp)
    return interp
```


### POP-CONTEXT!
`( -- )`

This pops a context from the context stack and throws it away.


### SPREADSHEET
`( url --  Spreadsheet )`
`( tab --  Spreadsheet )`

Returns a `Spreadsheet` object for the specified url. Also, can return the parent spreadsheet for a given `Tab`.


### TAB
`( url --  Tab )`

Returns the `Tab` for a specified gsheet url.

Flag words:
* `!NULL-ON-ERROR`: On error, suppresses error returning `NULL` instead


### TAB@
`( Spreadsheet tab_id --  Tab )`
`( Spreadsheet tab_name --  Tab )`

Given a spreadsheet, returns a tab in it for the specified `tab_id` or `tab_name`.

Flag words:
* `!NULL-ON-ERROR`: On error, suppresses error returning `NULL` instead


### ENSURE-TAB!
`( Spreadsheet tab_name -- Tab )`

Given a spreadsheet, ensures that a tab named `tab_named` exists and then returns it

### ROWS
`( Tab -- rows )`

Returns the rows of a Tab.

Flag words:
* `!RANGE`: This specifies the range to read (See https://developers.google.com/sheets/api/guides/concepts#cell)
* `!TRANSPOSE`: If set, data is returned by column rather than by row
* `!NULL-ON-ERROR`: On error, suppresses error returning `NULL` instead


### ROWS!
`( Tab rows -- )`

Writes the specified rows to a `Tab`

Flag words:
* `!RANGE`: This specifies the start range to write to. See [gsheets API guide](https://developers.google.com/sheets/api/guides/concepts#cell)
* `!TRANSPOSE`: By default, data will be written as rows. If this flag word is set, data will be written as columns
* `!CELL-FORMAT`: By default, data is assumed to be strings. If `!CELL-FORMAT` is set, the data will be treated as being in a "cell" format. This is a record with a `content` string field and an `updateRequest` field that contains a record with the structure of a gsheet API update request. See [gsheets API guide](https://developers.google.com/sheets/api/samples/formatting)


### CLEAR!
`( Tab -- )`

Clears all content and formatting from the specified tab in a gsheet.


### RECORDS
`( Tab header -- records )`

Given `header`, an array of column headings, searches the `Tab` for row that matches all of them and then returns the rows following it as an array of records where each column corresponds to a record field.

Flag words:
* `!RANGE`: This specifies the range to read. See [gsheets API guide](https://developers.google.com/sheets/api/guides/concepts#cell)
* `!NULL-ON-ERROR`: On error, suppresses error returning `NULL` instead


### RECORDS!
`( Tab header records -- )`

Writes an array of records to a `Tab` using the specified header to determine the column order.

Flag words:
* `!RANGE`: This specifies the start range to write to. See [gsheets API guide](https://developers.google.com/sheets/api/guides/concepts#cell)
* `!TRANSPOSE`: By default, data will be written as rows. If this flag word is set, data will be written as columns
* `!CELL-FORMAT`: By default, data is assumed to be strings. If `!CELL-FORMAT` is set, the data will be treated as being in a "cell" format. This is a record with a `content` string field and an `updateRequest` field that contains a record with the structure of a gsheet API update request. See [gsheets API guide](https://developers.google.com/sheets/api/samples/formatting)


### BATCH-UPDATE-TAB!
`( Tab update_requests -- )`

This provides low-level access to the [batchUpdate API](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/batchUpdate) for updating a specific tab.

Example of JSON data for updateRequests:
```
update_requests = [{
    "updateCells": {
        "range": {
            "sheetId": spreadsheet_id,
            "startRowIndex": startRowIndex,
            "startColumnIndex": startColumnIndex,
        },
        "rows": [
            {
                "values": [
                    {
                        "userEnteredFormat": {
                            "backgroundColor": {
                                "blue": 1
                            },
                            "textFormat": {
                                "foregroundColor" : {
                                    "green": 1
                                },
                                "italic": True
                            }
                        }
                    },
                    {
                    },
                    {
                        "userEnteredFormat": {
                            "backgroundColor": {
                                "green": 1
                            }
                        },
                        "note": "My note for the green cell"
                    }
                ]
            },
            {
                "values": [
                    {
                            "userEnteredFormat": {
                            "backgroundColor": {
                                "red": 1
                            }
                        }
                    },
                    {
                        "userEnteredFormat": {
                            "backgroundColor": {
                                "blue": 1
                            }
                        }
                    }
                ]
            }
        ],
        "fields": "userEnteredFormat.backgroundColor,userEnteredFormat.textFormat,note"
    }
}]
```

NOTE: changed fields must be specified in `fields` in order to have a valid update request.


### !RANGE
`( range -- )`

Flag word to set the `range` flag to something like "A1" or "A1:E7"


### !TRANSPOSE
`( -- )`

Flag word to set the `transpose` flag so data is read/written as columns rather than rows.


### !NULL-ON-ERROR
`( -- )`

Flag word to cause next word that returns a result to return `NULL` and suppress an error if one occurs.


### !CELL-FORMAT
`( -- )`

Flag word to set the `cell_format` flag to indicate that data is presented as "cells" rather than strings.

Example cell format:
```
: CELL1 [
    # `content` is the string content of the cell
    ["content"   "This is my new content"]

    # `updateRequest` allows the cell to be formatted, notes to be added, etc.
    ["updateRequest"   [
        ["userEnteredFormat"  [
            ["backgroundColor"  [["red" 0.796] ["green" 0.004] ["blue" 0]] REC]
            ["textFormat"  [
                ["foregroundColor"   [["red" 0.42]  ["green" 0.66]  ["blue" 0.31]] REC]
                ["italic"  TRUE]
                ["bold"  TRUE]
            ] REC]
        ] REC]
        ["note"   "This is a note I just added!"]
    ] REC]
] REC;
```

### INDEX>COL-NAME
`( 0-based_index -- col_name )`

Given a 0-based index, returns the corresponding column name. E.g., given 0, return 'A'.

### COL-NAME>INDEX
` ( col_name -- 0-based_index )`

Given a column name, returns the corresponding 0-based index. E.g., given 'Z', returns 25.