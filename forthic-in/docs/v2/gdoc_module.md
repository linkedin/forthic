# gdoc_module

The gdoc module allows you to interact with Google documents via the [gdoc API](https://developers.google.com/docs/api).

Because the gdoc API is fairly low level (in terms of document operations), the gdoc module defines higher-level
components and words that keep track of the character indexes so that text can be styled (more) reliably and
styled tables can be defined more easily.

In order to use this module, you'll need to register a client application with
Google and go through an OAuth flow to receive an OAuth token. An example of
doing this is in the [example setup](../EXAMPLES.md).

## Example
```
["gdoc"] USE-MODULES
# NOTE: You must create a Google `CredsContext` and apply it using gdoc.PUSH-CONTEXT!

# TODO: Add annotated example
```


## Reference

### PUSH-CONTEXT!
`(context --)`

This pushes a CredsContext (See `gsheet_module.py`) onto the gdoc module's
context stack. The most recent context is used to provide credentials to access
the gdocs API. The CredsContext must be configured in Python.

NOTE: We're reusing a `gsheet_module.CredsContext` since that module was written first

Here's a simple example:
```
def get_interp():
    interp = Interpreter()
    interp.set_dev_mode(True)

    def configure_gdoc_module(interp):
        interp.register_module(gdoc_module.GdocModule)

        class GoogleCredsContext(gsheet_module.CredsContext):
            def get_app_creds(self):
                res = {
                    "client_id": <read this from file>,
                    "client_secret": <read this from file>,
                }
                return res

            def get_auth_token(self):
                res = <read this from file>
                return res

        interp.run("['gdoc'] USE-MODULES")
        interp.stack_push(GoogleCredsContext())
        interp.run("gdoc.PUSH-CONTEXT!")
        return

    configure_gdoc_module(interp)
    return interp
```


### POP-CONTEXT!
`( -- )`

This pops a context from the context stack and throws it away.


### DOC
`( doc_id -- doc )`

Given a Google document ID (`https://docs.google.com/document/d/DOCUMENT_ID/edit#`), this returns the
JSON representation of the document as described in the [gdoc get API](https://developers.google.com/docs/api/reference/rest/v1/documents/get)


### NEW-DOC
`( title -- doc )`

Given a document `title`, this creates a new document and returns a JSON representation of it as described in the [gdoc create API](https://developers.google.com/docs/api/reference/rest/v1/documents/create). The document ID is in the `documentId` field of the response.


### BATCH-UPDATE
`( doc_id updates -- response )`

This is a lower-level word for explicitly applying batch updates to a document as described in the [gdoc batchUpdate API](https://developers.google.com/docs/api/reference/rest/v1/documents/batchUpdate). This can be used to construct any series of batch updates to a document, but it requires *all* of the required data structures (nested ones, too) to be explicitly constructed.

**NOTE**: Using this can be tricky because the gdoc API relies on character indexes to target styling and insertions, but those indexes change whenever new content is added/removed. The higher level words like `TABLE` and `TEXT` keep track of those changes and are easier to use.


### INSERT
`( doc_id char_index content_array -- )`

This is a higher level word that can insert an array of `Content` objects like `Text` and `Table` into an
existing gdoc at the specified character index.

This word adds content in multiple passes to preserve character indexes at points where text, table, and paragraph styles can be accurately applied. We've tested many cases, but it's impossible to test all of
them because the behavior of the gdoc data model is opaque.

**NOTE**: Nested tables are currently not supported

### PT
`( number -- dimension )`

This is a convenience word to convert a number to a `Dimension` object in the [gdoc API](https://developers.google.com/docs/api/reference/rest/v1/documents#Dimension) that looks like this:

```
{
  "magnitude": number,
  "unit": PT
}
```

### COLOR
`( red green blue -- Color )`

This is a convenience word to convert red, green, blue components (from 0.0 to 1.0) into a `Color` object in the [gdoc API](https://developers.google.com/docs/api/reference/rest/v1/documents#color):

```
{
    "color": {
        "rgbColor": {
            "red": red,
            "green": green,
            "blue": blue,
        }
    }
}
```


### TABLE
`( table_rows -- Table )`

This is a higher level `Content` word that takes a `table_rows` array containing rows of `Content` and returns a `Table` object that can be added to a gdoc via `INSERT`.

To specify an empty cell, use `NULL`.

The `Table` will ensure that all rows are the same length by adding `NULL` elements to the end of
shorter rows as needed.

**NOTE**: Cell elements must be `Text` or `ConcatText`. Nested tables are not supported.


### TEXT
`( string -- Text )`

This takes a `string` and returns a `Text` object.


### PAGE-BREAK
`( -- PageBreak )`

Returns a `PageBreak` object that can be inserted into a document as `Content`.


### TEXT-CONCAT
`( [Text] -- ConcatText )`

Given an array of `Text` objects, returns a `ConcatText` object that represents the concatenation of them. This object is required in order to preserve the styling of the component text objects.


### <PARAGRAPH-STYLE
`( Text style -- Text )`
This takes a `Text` object and applies a paragraph style to it. The paragraph style objects are specified in the [gdoc API](https://developers.google.com/docs/api/reference/rest/v1/documents#ParagraphStyle)

Example:
```
# ----- Define the styles
: MED-BLUE      0.235 0.522 0.776 gdoc.COLOR;

: THIN-BLUE-TABLE-BORDER   [
    ["color"      MED-BLUE]
    ["width"      1.5 PT]
    ["dashStyle"  "SOLID"]
] REC;

: H1-PARAGRAPH-STYLE   [
    ["namedStyleType" "HEADING_1"]
    ["borderBottom"  THIN-BLUE-BORDER 2 PT "padding" <REC!]
] REC;


# ----- Apply the paragraph style
"My Header" gdoc.TEXT H1-PARAGRAPH-STYLE gdoc.<PARAGRAPH-STYLE;
```

### <TEXT-STYLE
`( Text style -- Text )`

Applies a [text style](https://developers.google.com/docs/api/reference/rest/v1/documents#TextStyle) to a `Text` object.

Example:
```

# ----- Define styles
: DARK-GRAY     0.2 0.2 0.2 gdoc.COLOR;

: BOLD-STYLE   [
    ["bold"  TRUE]
    ["underline" FALSE]
    ["fontSize"   11 PT]
    ["weightedFontFamily"   [["fontFamily"  "Helvetica Neue"]  ["weight"  700]] REC]
    ["foregroundColor" DARK-GRAY]
] REC;


# ----- Apply style
"My header" gdoc.TEXT BOLD-STYLE gdoc.<TEXT-STYLE
```


### <TABLE-STYLE
`( Table style row col row_span col_span -- Table )`

This applies a [table cell style](https://developers.google.com/docs/api/reference/rest/v1/documents#TableCellStyle) to a `Table` starting from (`row`, `col`) and extending for `row_span`
rows and `col_span` columns.

Example:
```
# ----- Define styles
: NO-BORDER   [
    ["color"      1 1 1 gdoc.COLOR]
    ["width"      0 PT]
    ["dashStyle"  "SOLID"]
] REC;

: THIN-BLUE-TABLE-BORDER   [
    ["color"      MED-BLUE]
    ["width"      1.5 PT]
    ["dashStyle"  "SOLID"]
] REC;

: TOP/BOTTOM-BLUE-BORDERS   [
    ["borderBottom"  THIN-BLUE-TABLE-BORDER]
    ["borderLeft"    NO-BORDER]
    ["borderRight"   NO-BORDER]
    ["borderTop"     THIN-BLUE-TABLE-BORDER]
] REC;


# ----- Apply table style to cells in first 3 rows and first 2 columns
TOP/BOTTOM-BLUE-BORDERS 0 0 3 2 gdoc.<TABLE-STYLE
```

### <TABLE-COLUMN-PROPERTIES
`( Table column_properties column_indices -- Table )`

This applies column properties to an array of columns within a `Table`. Column properties
are essentially column widths as described in the [gdoc API](https://developers.google.com/docs/api/reference/rest/v1/documents#TableColumnProperties).

Example:
```
# ----- Define styles
: INCH               72 * gdoc.PT;

: 1-INCH-COLUMN   [
    ["widthType"  "FIXED_WIDTH"]
    ["width"       1.1 INCH]
] REC;

# ----- Make 1st and 3rd columns 1 inch wide
1-INCH-COLUMN [0 2] gdoc.<TABLE-COLUMN-PROPERTIES

```

### <FULL-TABLE-STYLE
`( Table style -- Table )`

This applies a [table cell style](https://developers.google.com/docs/api/reference/rest/v1/documents#TableCellStyle) to all cells of a table.


### <MERGE-TABLE-CELLS
`( Table row col row_span col_span -- Table )`

This merges the rows of a table starting from (`row`, `col`) and extending `row_span` rows and `col_span` cols (inclusive).
