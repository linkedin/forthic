# recharts_module

The global module defines words that are available in all Forthic programs.


## Reference: Base Words

### ELEMENT
`( tag -- Element)`

Given a Recharts tag name, returns the corresponding element. See [Recharts API](https://recharts.org/en-US/api) for the possible Recharts tags.


### LABEL-FUNC
`( field -- function )`

Returns a Javascript `function` that takes an entry and returns `entry[field]`.

This can be used to construct custom labels for charts.

### DATE-FORMATTER
`( format_strings -- function )`

Returns a Javascript `function` that takes a date and formats it. See [date-fns](https://date-fns.org/docs/Getting-Started) for more details on date formats.


### NUMBER-FORMATTER
`( num_digits suffix -- function )`

Returns a Javascript `function` that takes a number and formats it. The function will round the number to the specified `num_digits` and then append a `suffix` to the result.

### TRUNCATE-FORMATTER
`( max_length -- function )`

Returns a Javascript `function` that takes a string and truncates it to the specified
length, appending "..." if truncation was required.


### CUMULATIVE-DIST>CHART-DATA
`( cumulative_dist -- charts_data )`

This conditions the output of the Forthic Python `CUMULATIVE-DIST` word so it can be
rendered as a line chart.
