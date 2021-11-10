# gsheet_module

The gsheet module allows you to interact with Google spreadsheets via the [gsheets API](https://developers.google.com/sheets/api).

In order to use this module, you'll need to register a client application with
Google and go through an OAuth flow to receive an OAuth token. An example of
doing this is in the [example setup](../EXAMPLES.md).

## Example
```
["gsheet"] USE-MODULES
# NOTE: You must create a Google `CredsContext` and apply it using PUSH-CONTEXT!

["sheet_id"] VARIABLES

# Returns all rows in a gsheet tab
'https://gsheet-tab-url' gsheet.URL>SHEET-ID/RANGE gsheet.ROWS

# Store a gsheet ID for rest of examples
'https://gsheet-tab-url' gsheet.URL>SHEET-ID/RANGE POP sheet_id !

# Get all rows in "Sheet1"
sheet_id @ 'Sheet1' gsheet.ROWS

# Look for a header row and return rows beneath it as an array of records
sheet-id @ 'Sheet1' ['Heading1' 'Heading2'] gsheet.RECORDS

# Write data to a range of cells
sheet-id @ 'Sheet1!D1:F2' [["Col1" "Col2" "Col3"] ["1" "2" 30]] gsheet.ROWS!
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


### URL>SHEET-ID/RANGE
`( gsheet_url -- sheet_id range )`

Given a URL to a gsheet tab, returns the sheet ID and its range. The range is typically the
tab name. The sheet ID is used with other words in this module.


### URL>SHEET-ID/TAB-ID
`( gsheet_url -- sheet_id tab_id )`

Given a gsheet URL, returns its sheet id followed by its tab ID.


### SHEET-INFO
`( gsheet_id -- info )`

Returns metainfo for a gsheet ID.

### URL>SHEET-INFO
`( url -- info )`

Givne a URL to a gsheet, returns the metainfo for the gsheet (and all tabs)


### URL>TAB-INFO
`( url -- tab_info )`

Givne a URL to a gsheet tab, returns the metainfo for that tab


### NUM-ROWS
`( url -- num )`

Given a URL to a gsheet tab, returns the number of rows in that tab


### ROWS
`( gsheet_id range -- rows )`

Returns cell data for a specified sheet and range as an array of rows.


### ROWS!
`( gsheet_id range rows -- )`

Updates a gsheet range with an array of row data.

### RECORDS
`( gsheet_id range header -- records )`

Returns a range of gsheet data as an array of records. The fields in each record
correspond to the specified `header`.

If the `header` cannot be found in the range, an error is raised.

### BATCH-UPDATE
`( url update_requests -- )`

This provides direct access to the [batchUpdate API](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/batchUpdate).

This takes a `url` and an array of `update_requests` where each request is a [Request object](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/request#Request) like:
```
    : DELETE-ROWS-RANGE   [
        ["dimension"   "ROWS"]
        ["startIndex"   5]
    ] REC;

    ["deleteDimension"  [
        ["range"   DELETE-ROWS-RANGE]
    ] REC]
```

This word adds the tab ID associated with the `url` to each of the request objects and executes the batch update call.

### CLEAR-SHEET!
`( url -- )`

Clears all content and formatting from a gsheet

### CLEAR-TAB!
`( url tabname -- )`

Clears all content and formatting from the specified tab in a gsheet.

### ENSURE-TAB!
`( url tabname -- )`

Ensures that a tab named `tabname` is in the gsheet.

### SHEET-ROWS
`( url -- rows )`

Given a URL to a gsheet tab, returns its rows.

### SHEET-RECORDS
`( url headers -- records )`

Given a URL and a list of headers, returns an array of records. The fields in
each record correspond to the specified `header`.

If the `header` cannot be found in the range, an error is raised.

### CONDITIONAL-FORMATS
`( url -- conditional_formats )`

Given a URL to a gsheet, return all of its conditional formats.


### DELETE-CONDITIONAL-FORMATS
`( url -- )`

Deletes all conditional formats from a gsheet tab.

### REPEAT-CELL-FORMATS
`( url Range Format -- )`

Given a gsheet url a `Range` object (see `RANGE`) and a `Format` object (see `FORMAT`), applies this format to all cells in the range.

### FILTERS
`( url -- filters )`

Given a gsheet url, returns all filters defined in the tab.

### UPDATE-ALL-FILTER-END-ROWS
`( url end_row -- )`

Given a 1-based end row, updates all filters in the specified gsheet by extending their rows to that specified end row.


### RANGE
`( url col_start row_start col_end row_end -- Range )`
Given a gsheet url and a starting row and column (e.g., 'A' 1) and an ending row and column (e.g., 'H' 32), returns a Range object that represents this range. Row numbers are 1-based.

### COLOR
`( color_rec -- Color )`

Given a color record with fields "red", "green", "blue", "alpha" whose values are from 0 to 255, returns a `Color` object.

### CONDITION
`( type value -- Condition)`

Given a condition type and value, returns a `Condition` object. See https://developers.google.com/sheets/api/samples/conditional-formatting for more on how to choose `type` and `value`.

Example:
```
"TEXT_CONTAINS" "Red" gsheet.CONDITION
```

### FORMAT
`( -- Format)`

Constructs a default `Format` object that can be modified using words like `<BACKGROUND-COLOR`, `<FOREGROUND-COLOR`, `<TEXT-FORMAT`.

### BORDER
`( -- Border)`

Constructs a default `Border` object with 1px width and solid lines.

### BOOLEAN-RULE
`( condition format -- BooleanRule )`
Given a condition and a format, creates a `BooleanRule` that can be used to conditionally format cells.

Example:
```
: RED-CONDITION   "TEXT_CONTAINS" "Red" gsheet.CONDITION;
: RED-BACKGROUND  gsheet.FORMAT  RED-COLOR gsheet.<BACKGROUND-COLOR;

RED-CONDITION RED-BACKGROUND gsheet.BOOLEAN-RULE
```

### <BACKGROUND-COLOR
`( Format Color -- Format )`

Sets the background color on a `Format` object

### <FOREGROUND-COLOR
`( Format Color -- Format )`

Sets the foreground color on a `Format` object

### <BOLD
`( Format bool -- Format )`

Sets bold flag on `Format` object.

### <TEXT-FORMAT
`( Format text_format -- Format )`

Sets the text format (also a `Format`) of a `Format` object.

Example:
```
: <GRAY-BACKGROUND   GRAY-COLOR <BACKGROUND-COLOR;    # ( Format -- Format )
: <BOLD-TEXT         TRUE <BOLD <TEXT-FORMAT;         # ( Format -- Format )

: HEADER-FORMAT      FORMAT <GRAY-BACKGROUND <BOLD-TEXT;
```

### ADD-CONDITIONAL-FORMAT-RULES
`( url Range Rules -- )`

Applies conditional formatting rules to the specified range in the specified gsheet tab.

### UPDATE-BORDERS
`( url Range border_record -- )`

Applies borders to the specified range in the specified gsheet tab. The border_record has fields like "top", "bottom", "left", "right", "innerHorizontal", "innerVertical".

Example:
```
: BORDERS   [
    ["top"              BORDER]
    ["bottom"           BORDER]
    ["left"             BORDER]
    ["right"            BORDER]
    ["innerHorizontal"  BORDER]
    ["innerVertical"    BORDER]
] REC;

MY-SHEET MY-RANGE BORDERS UPDATE-BORDERS
```

### INDEX>COL-NAME
`( 0based_index -- col_name )`

Given a 0-based index, returns the corresponding column name. E.g., given 0, return 'A'.

### COL-NAME>INDEX
` ( col_name -- 0based_index )`

Given a column name, returns the corresponding 0-based index. E.g., given 'Z', returns 25.