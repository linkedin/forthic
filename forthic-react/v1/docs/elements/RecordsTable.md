# RecordsTable

A `RecordsTable` can render an array of records in a table. This table can be configured to automatically show total columns and rows, handle click events, and paginate the data.

## Props
* `records`: A list of records to render
* `column_info`: A list of records describing each column. The order of the columns matches the order of the column_info elements. Each record has the following fields
    * `field`: What field in each record to render
    * `label` (optional): What label to use as the heading for the table
    * `is_header` (optional): Boolean to indicate if the column is a header column for each row
    * `fsort` (optional): Forthic used to return a value for sorting the column. If specifed, enables sort
    * `fclick` (optional): Forthic executed on click. The Forthic should expect the record value associated with the cell
    * `fformat` (optional): Forthic executed to format value
    * `className` (optional): Classnames for column
* `total_info` (optional): Record with the following fields
    * `total_row_label` (optional): Specifies label to use for total row. If set, computes total row. NOTE: The row totals are computed only for the visible records
    * `total_col_label` (optional): Specifies label to use for total col. If set, computes total col
    * `fclick` (optional): Forthic executed on click. The Forthic should expect an array record values associated with the row
    * `col_className` (optional): Classnames for total column
* `pagination_info` (optional): Record with the following fields
    * `page_size`: Number of records to show in each page. Setting this enables pagination
* `wrapper_className` (optional): Adds a className to the wrapper div
* `header_className` (optional): Adds a className to the th elements in the table
* `interp`: Forthic interpreter (this is automatically set by the global module)

## Examples

### Simple Table
```
: RECORDS   [
    [["key"  "a"] ["greek"  "alpha"]] REC
    [["key"  "b"] ["greek"  "beta"]] REC
    [["key"  "c"] ["greek"  "gamma"]] REC
];

: COL-INFO [
    [["field" "key"] ["label" "Letter"]] REC
    [["field" "greek"]] REC
];

: SIMPLE-TABLE  RecordsTable [
    ["records"      RECORDS]
    ["column_info"  COL-INFO]
] REC <PROPS;
```