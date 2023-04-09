# global_module

The global module defines words that are available in all Forthic programs.


## Reference: Base Words

### VARIABLES
`( varnames -- )`

Adds the specified variables to the current module. Once a variable is created, its value can be set with `!` and retrieved with `@`.

Example:
```
["x" "y"] VARIABLES
20 x !
30 y !

[x @ y @]    # [20 30]
```

### !
`( value variable -- )`

Sets the value of a variable.


### @
`( variable -- value )`

Returns the value of a variable.

### !@
`( value variable -- value )`

Sets a value of a variable, leaving value on stack.

### <!
`( value variable -- variable )`

Sets a value of a variable, leaving the variable on stack.

### INTERPRET
`( string -- ? )`

Runs a forthic string. The string runs in the same context that INTERPRET is called (i.e., same current module and module stack).

### MEMO
`( name forthic -- )`

Creates three words in the current module that are constructed based on the specified `name`:

* name: The first time this is called, it runs the specified `forthic` code, stores the result, and then returns the result. On  subsequent calls, it simply returns the stored result.
* name!: Forces `forthic` to be executed, updating the stored value with the result
* name!@: Same as name!, but also returns the stored value

Example:
```
"RECORDS"    "EXPENSIVE-RECORDS-LOOKUP"   MEMO

RECORDS      # Runs EXPENSIVE-RECORDS-LOOKUP memoizes the result and returns it
RECORDS      # Returns memoized result without doing expensive lookup

RECORDS!     # Forces EXPENSIVE-RECORDS-LOOKUP call to update memoized value
```

### EXPORT
`( names -- )`

This marks words defined in a module as exportable when that module is used by another module (see `USE-MODULES`)


### USE-MODULES
`( names -- )`

Given a list of modules that have been registered with the Forthic interpreter,
imports all of their exported words. The name of each imported word is prefixed
with the name of the module and a ".". This prefix may be overridden by passing
an array like [module_name, prefix] instead of a module name. If the overridden
prefix is the empty string, then the words are imported without a prefix.

Example:
```
["jira"] USE-MODULES

"assignee = someuser"  ["Summary" "Due Date"] jira.SEARCH

[["jira" "j"]] USE-MODULES
"assignee = someuser"  ["Summary" "Due Date"] j.SEARCH

[["jira" ""]] USE-MODULES
"assignee = someuser"  ["Summary" "Due Date"] SEARCH
```


### REC
`( key_vals -- rec )`

Given an array of [key value] pairs, returns a record whose fields are the specified keys.

Example:
```
[
    ["id"      101]
    ["title"   "My Title"]
    ["cost"    9.99]
] REC

# {"id": 101, "title": "My Title", "cost": 9.99}
```

### REC@
`( rec field -- value )`
`( rec fields -- value )`

Given a record, returns the value for the specified `field`. If an array of fields is provided,
this will assume the record's value is another record, nested to at least the number of fields indicated and will drill down into the record to retrieve the value.

Example:
```
[["key" 101] ["name" "John Doe"]] REC
"key" REC@     # 101

[["key" 102] ["item" [["name" "cabbage"] ["cost" 2.00]] REC]] REC
["item" "name"] REC@   # "cabbage"
```

### <REC!
`( rec value field -- rec )`

Given a record, sets its `field` to `value` and leave the record on the stack.

Example:
```
[] REC  "cabbage" "name" <REC!   # {"name": "cabbage"}
```


### SCREEN!
`( content name -- )`

Stores `content` as a Forthic "screen" in the app module. A screen is a chunk of
code that may be loaded and run later. Screens are used to break up a large
Forthic program into smaller pieces.


### SCREEN
`( name -- content )`

Returns the screen with the specified name. If the screen does not exist, an
empty string is returned.


### LOAD-SCREEN
`(name -- ? )`

Interprets a screen with the specified name. Screens may load other screens, but if an infinite load is detected, an error will be raised.

## Reference: Lambda words

### LAMBDA
`( forthic_str -- Lambda )`

Converts a Forthic string into a Lambda object. Lambdas represent the execution of a Forthic string and can be
converted into other Lambdas with new behaviors (e.g., gathering execution errors rather than raising them)

### NULL-ON-ERROR-LAMBDA
`( Lambda -- Lambda )`

Converts a Lambda into one that pushes `NULL` onto the stack on exception, re-rasing that exception.

### ACCUM-ERROR-LAMBDA
`( Lambda list_variable -- Lambda )`

Converts a Lambda into one that catches exceptions and pushes them onto the specified `list_variable`. If there
was no exception, `NULL` is pushed onto `list_variable`.

## Reference: Array/Record words

These words apply to both arrays of items and records of items. These containers
are viewed as identical from the perspective of how elements are accessed:
indices for arrays and keys for records.

### APPEND
`( array item -- array )`

`( record key/val -- record )`

If have an array and an item, appends item to end of array. If have a record and a key/value pair, adds key/value pair to record overwriting an existing key.

### REVERSE
`( array -- array )`

Reverses the elements of an array. For a record, this has no effect.


### UNIQUE
`( array -- array )`

`(record -- record )`

For an array, returns an array where all elements are unique. For a record, returns a record where all values are unique. For records with duplicate values, only one of the associated keys will be retained.

### <DEL
`( array index -- array )`

`( record key -- record )`

For an array, deletes element at index. For record, deletes element at key.


### RELABEL
`( array old_indices new_indices -- array )`

`( record old_keys new_keys -- record )`

For an array, rearranges elements by moving the element at the old index to the
associated new index. Because he resulting list will be ordered according to the
order of the new indices, the new indices may be outside of the list range.

For a record, this relabels the specified keys to the new keys. Any specified keys that are missing from the record are dropped from the resulting record.

### BY-FIELD
`( array field -- record )`

`( record field -- record )`

Given an array of records, returns a record that maps each record's field to itself. If there are duplicate fields, only one item will be retained.

This takes the values of the record and organizes them by the specified field. If there are duplicate fields, only one record is retained.


### GROUP-BY-FIELD
`( array field -- record )`

`( record field -- record )`

This groups the values of an array/record into lists such that in each list, items all share the same value for the specified `field`.


### GROUP-BY
`( array forthic -- record )`

`( record forthic -- record )`

This groups the values of an array/record into lists such that in each list, share the same value when operated on by the specified `forthic` string.



### GROUP-BY-w/KEY
`( array forthic -- record )`

`( record forthic -- record )`

This groups the values of an array/record into lists such that in each list, share the same value when operated on by the specified `forthic` string.
The `forthic` string should expect an index/key and then the value.


### GROUPS-OF
`( array n -- arrays )`

`( record n -- records )`

Given an array, returns an array of arrays with at most `n` elements from the
original array.

Given a record, returns an array of records with at most `n` key/value pairs
from the original record.

### INDEX
`( array forthic -- array )`

Given an array and `forthic` which takes an object and returns a list of keys for it, this returns a record
from keys to list of objects with the corresponding keys. NOTE that all keys in the resulting record will be lower-cased.

Example:
```
TICKETS "'Labels' REC@" INDEX
```

### MAP
`( array forthic -- array )`

`( record forthic -- record )`

For an array, returns a new array whose values are the result of the `forthic` string be executed for the corresponding items. For a record, returns a new record whose values are the result of the `forthic` string being executed for corresponding values in the source record.

Example:
```
: DOUBLE   2 *;
[1 2 3 4 5] "DOUBLE" MAP    # [2 4 6 8 10]

[
    ["alpha" 2]
    ["beta" 18]
] REC "DOUBLE" MAP   # {"alpha": 4, "beta": 36}
```

### MAP-w/KEY
`( array forthic -- array )`

`( record forthic -- record )`

For an array, returns a new array whose values are the result of the `forthic` string be executed for the corresponding items. For a record, returns a new record whose values are the result of the `forthic` string being executed for corresponding values in the source record. The `forthic` string should expect an index/key and then the value.

Example:
```
["key" "value"] VARIABLES
: MAKE-ID   (value ! key !)  [key @ "-" value @ ] CONCAT;

[1 2 3] "MAKE-ID" MAP-w/KEY   # ["0-1" "1-2" "2-3"]
```

### FOREACH
`( array forthic -- ? )`

`( record forthic -- ? )`

For an array, executes a `forthic` string for each of the corresponding items. For a record, executes a `forthic` string for each of the values in a record.

NOTE: This does not return any values

Example:
```
: DOUBLE   2 *;
[1 2 3]
"DOUBLE" FOREACH    # ( [1 2 3] -- 2 4 6 )
```

### FOREACH-w/KEY
`( array forthic -- ? )`

`( record forthic -- ? )`

For an array, executes a `forthic` string for each of the corresponding items. For a record, executes a `forthic` string for each of the values in a record.
The `forthic` string should expect an index/key and then the value.

NOTE: This does not return any values

### INVERT-KEYS
`( record -- record )`

Given a record with nested keys (i.e., a record whose fields are records), returns a new record with the first level keys
and the second level keys reversed.

Example:
```
[
    ["alpha"  [["open"  1] ["closed" 2]] REC]
    ["beta"   [["closed" 4]] REC]
] REC

INVERT-KEYS  # {open: {alpha: 1}, closed: {alpha: 2, beta: 4}}
```

### ZIP
`( array1 array2 -- array )`

`( record1 record2 -- record )`

Given two arrays, returns a new array whose elements are pairs of corresponding elements where the first element from each pair is from `array1` and the second from `array2`. If `array1` is shorter than `array2`, then only elements through the end of `array1` will be zipped. If `array1` is longer than `array2`, then `None` elements will be used as values from `array2` as necessary.

Given two records, returns a new record analogously to the arrays case, but pairs will be created according to keys from `record1`.

### ZIP-WITH
`( array1 array2 forthic -- array )`

`( record1 record2 forthic -- record )`

Similar to `ZIP` but the value in the new array/record is determined by running the specified `forthic` string.
This string expects two values: the first value from array1/record1 and the second from array2/record2.


### KEYS
`( array -- indices )`

`( record -- keys )`

Given an array, returns an array of its indices starting from 0. Given a record, returns its keys.

### VALUES
`( array -- values )`

`( record -- values )`

For an array, this returns the same array. For a record, this returns its values.


### LENGTH
`( array -- length )`

`( record -- length )`

Returns the number of elements in an array/record.


### SLICE
`( array start end -- array )`

`( record start end -- record )`

For an array, returns an array whose elements start at `start` and end at `end`, inclusively. Nonnegative values of `start` and `end` behave as normal indexes. Negative values start at the end of the array and move towards the front (e.g., -1 is the last element, -2 is the next to last element, etc.). Values larger than the length of the list refer to the end of the list. Elements are returned in order starting from `start` and going to `end`.

For a record, the behavior is as follows:

* Sort the record keys
* Apply the same `start` and `end` logic to the sorted keys as with an array
* Return a record with the resulting key/value pairs

Example:
```
['x'] VARIABLES
['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !

x @ 0 2 SLICE      # ['a' 'b' 'c']
x @ 1 3 SLICE      # ['b' 'c' 'd']
x @ 5 3 SLICE      # ['f' 'e' 'd']
x @ -1 -2 SLICE    # ['g' 'f']
x @ 4 -2 SLICE     # ['e' 'f']
x @ 5 10 SLICE     # ['f' 'g' NULL NULL NULL NULL]
```


### DIFFERENCE
`( array1 array2 -- array )`

`( record1 record2 -- record )`

Given two arrays, returns all elements in `array1` but not in `array2`.

Given two records, returns a record of all key/vals in `record1` but not in `record2` (considering only the keys).


### INTERSECTION
`( array1 array2 -- array )`

`( record1 record2 -- record )`

Given two arrays, returns all elements in `array1` and `array2`.

Given two records, returns a record of all key/vals in `record1` such that the keys are also in `record2`.


### UNION
`( array1 array2 -- array )`

`( record1 record2 -- record )`

Given two arrays, returns a unique array of all elements in both.

Given two records, returns a record of all key/vals in `record1` and all key/vals in `record2` that were not in `record1`.


### SELECT
`( array forthic -- array )`

`( record forthic -- record )`

Given an `array` and a `forthic` predicate, returns all elements in the array such that the predicate returns `True`.

Given a `record` and a `forthic` predicate, returns a record with key/vals where the predicate returns `True` for each value.

Example:
```
: SHORT-WORD?   LENGTH 3 <=;

["a" "the" "elephant" "hamburger"] "SHORT-WORD?" SELECT   # ["a" "the"]
```

### SELECT-w/KEY
`( array forthic -- array )`

`( record forthic -- record )`

Given an `array` and a `forthic` predicate, returns all elements in the array such that the predicate returns `True`.

Given a `record` and a `forthic` predicate, returns a record with key/vals where the predicate returns `True` for each value.

The `forthic` predicate expects the key/index and the value.


### TAKE
`( array n -- rest result )`

`( record n -- rest result )`

Given an `array` and a number `n`, splits the array into the first `n` elements and the `rest` of the elements and returns `rest` followed by the `result`.

Given a `record` and a number `n`, splits the keys in the record into the first `n` keys (after sorting the keys) and the rest of the keys. The record is split into a `rest` record with the rest of the key/vals and a `result` record with the first `n` key/vals.


### DROP
`( array n -- rest )`

`( record n -- rest )`

Drops the first `n` elements of a container.


### ROTATE
`( array -- array )`

`( record -- record )`

Given an array, removes the last element and inserts it as the first element.

For a record, this is a no-op.


### ROTATE-ELEMENT
`( array element -- array )`

`( record element -- record )`

Given an `array` and an `element`, moves the first matching element in the array to the front of the array.

For a record, this is a no-op.


### SHUFFLE
`( array -- array )`

`( record -- record )`

Given an array, returns a new array containing the original elements but in a random order.

For a record, this is a no-op.


### SORT
`( array -- array )`

`( record -- record )`

Sorts elements of an array by a default comparison. For a record, this is a no-op.


### SORT-w/FORTHIC
`( array forthic -- array )`

`( record forthic -- record )`

Sorts elements of an array using a `forthic` string to return a value for comparison. For
a record, this is a no-op.

### SORT-w/KEY-FUNC
`( array key_func -- array )`

`( record key_func -- record )`

Sorts elements of an array using a function implemented in the host language
that takes an item and returns a value to compare. This is more performant than
using a Forthic string. See `FIELD-KEY-FUNC`

### FIELD-KEY-FUNC
`( field -- key_func )`

Given a field name, returns a function (implemented in the host language) that takes an items and
returns its `field` value.


### NTH
`( array n -- item )`

`( record n -- value )`

Given an `array` of items and a number `n`, returns the nth value in the array (0-based).

Given a `record` and a number `n`, sorts the record's keys, selects the nth key
and returns the associated value.


### LAST
`( array -- item )`

`( record -- item )`

Given an `array`, returns its last element. Given a `record`, sorts its keys and
returns the value associated with the last element.


### UNPACK
`( array -- a1 .. an )`

Given an array, returns its elements pushed individually onto the stack in order.

### FLATTEN
`( nested_arrays -- array )`

`( nested_records -- record )`

Given an array of arrays, nested to any level, returns a new array consisting of
all of the underlying elements.

Given a record with records as values, nested to any level, returns a new record
that maps keys to individual values. The keys in the resulting record are
constructed by joining the "key chain" with the "\t" character.

Example:
```
[
    ['a' 1]
    ['b' [['alpha'  4]] REC]
] REC
FLATTEN   #  {'a': 1, 'b': {'alpha': 4}}  --  {'a': 1, 'b\talpha': 4}
```


### KEY-OF
`( array item -- index )`

`( record item -- key )`

Given an `array` of items and an `item` returns the index of the first matching item.

Given a `record` and an item, returns a key where one of the values matches.

If an item can't be found, `NULL` is returned.


### REDUCE
`( array initial forthic -- value )`

`( record initial forthic -- value )`

Given an `array` of items, an `initial` value, and a `forthic` string that takes two values (a value and an item from the array), pushes the `initial` value onto the stack and then repeatedly pushes elements from the `array` running `forthic` for each, resulting in a final value.

Given a `record` of items, an `initial` value, and a `forthic` string that takes two values (a value and a value from the record), pushes the `initial` value onto the stack and then repeatedly pushes values from the `record` running `forthic` for each, resulting in a final value.

Example:
```
[1 2 3 4 5] 10 "+" REDUCE       # 25
```

### CUMULATIVE-DIST
`( records field breakpoints -- cumulative_distribution )`

Given an array of `records`, a `field` that's part of those records, and a set of
`breakpoints` (in ascending order), this computes the cumulative distribution
of those records relative to the breakpoints. The return value is a record with the
following fields:

* `records`: The input records
* `field`: The input field
* `breakpoints`: The input breakpoints
* `record_breakpoint_indexes`: This is an array that is 1-1 with the records array
and which contains the breakpoint index for each record.
This is the index of the breakpoint largest for which the record's field value
is less than or equal to that breakpoint value.
* `breakpoint_counts`: This is an array that is 1-1 with the `breakpoints` array
and contains the number of records whose field values are less than or equal to
the corresponding breakpoint.
* `breakpoint_pcts`: This is like `breakpoint_counts` but is in terms of percent of total records


## Reference: Stack words
These words directly affect the parameter stack.

### POP
`( item -- )`

Pops an item from the parameter stack and throws it away.

### DUP
`( a -- a a )`

Duplicates the top element of the stack.

For the Python and Javascript host languages, the duplicated element is a reference to the original element.

### N-DUP
`( a num -- a ... a )`

Pops the top element of the stack and pushes `num` duplicates of it onto the stack.

For the Python and Javascript host languages, the duplicated element is a reference to the original element.

### SWAP
`( a b -- b a )`

Swaps the order of the top two elements of the parameter stack.


## Reference: String words
These words manipulate generic strings.

### CONCAT
`( str1 str2 -- str)`

`( strings -- str )`

Given two strings, this concatenates them and returns the result.

Given an array of strings, this concatenates all of them and returns the result.


### SPLIT
`( string sep -- strings )`

Given a string and a separator, splits the string on the separator and returns
the resulting array of strings.


### JOIN
`( strings sep -- string )`

Given an array of strings and a separator, joins them into a single string using the
separator string.


### /N
`( -- \n )`

Returns a newline character (`\n`).

### /R
`( -- \r )`

Returns a carriage return character (`\r`).

### /T
`( -- \t )`

Returns a tab character (`\t`).


### |LOWER
`( string -- string )`

Given a `string`, returns a lower-cased version of it.


### |ASCII
`( string -- string )`

Given a string, returns a new string with all non-ASCII characters removed.

### STRIP
`( string -- string )`

Given a string, returns one with beginning and trailing whitespace removed.

### REPLACE
`( string s r -- string )`

Given a `string`, a substring `s`, and a replacement string `r`, returns a new string with all instances of `s` replaced with `r`.

### RE-MATCH
`( string regex -- match )`

Given a `string` and a regular expression `regex`, returns a `match` object from
the host language's regex module. If no match is found, returns `NULL`. Also see `RE-MATCH-GROUP`.


### RE-MATCH-GROUP
`( match num -- string )`

Given a `match` object from `RE-MATCH` and a number `num`, returns the corresponding matched group from the match object.

### >STR
`( object -- string )`

Given an object return a string representation of it determined by the host
language.


### URL-ENCODE
`( str -- url_encoded_str )`

Converts a string into a URL-encoded string

### URL-DECODE
`( url_encoded_str -- str )`

Converts a URL-encoded string back to its unencoded version.


## Reference: Misc words

### NULL
`( -- None )`

Pushes the host language's concept of `null` onto the stack.


### QUOTE-CHAR
`( -- DLE )`

Returns the ASCII character `DLE` (16). This is used as a non-typeable quote character for sending
data and Forthic strings across environments.


### DEFAULT
`( value default_value -- val )`

If a `value` is `NULL` or an empty string, return the `default_value`; otherwise, return `value`.

Example:
```
"Howdy"  "<no message>" DEFAULT    # "Howdy"
NULL     "<no message>" DEFAULT    # "<no message>"
```


### *DEFAULT
`( value default_forthic -- val )`

If a `value` is `NULL` or an empty string, run `default_forthic` to provide a value; otherwise, return `value` without running `default_forthic`.

Example:
```
NULL  "1 2 +"  *DEFAULT    # 3
```


### <REPEAT
`( item forthic n -- ? )`

Pops an item, runs `forthic` string `n` times. On completion, leave `item` in front of any result.

Example:
```
1 "2 *" 5 <REPEAT   # 1 2 4 8 16 32
```


### IDENTITY
`( a -- a )`

A no-op.


### >FIXED
`( number num_digits -- str )`

Given a `number` and a `num_digits` returns a string representation of the
number with the specified number of significant digits.


### >JSON
`( object -- json )`

Given an object in the host language, returns a JSON string representation of it. If the object cannot be rendered as a JSON string, an exception is raised.

### JSON>
`( json -- object )`

Given a JSON string, returns a host language object version of it.


### >TSV
`( rows -- tsv )`

Given an array of rows, returns a tab-separated value string.

Example:
```
[['alpha' 'beta' 'gamma'] [1 2 3]] >TSV  # "alpha\tbeta\gamma\n1\t2\t3\n"
```


### TSV>
`( tsv -- rows )`

Given a tab-separated values string, returns an array of rows corresponding to it.


### RECS>TSV
`( records header -- tsv )`

Given an array of `records` and a `header` with values corresponding to the fields of the records, returns a tab-separated values string  with a header row followed by rows for each record.

### TSV>RECS
`( tsv -- records )`

Given a tab-separated values string with a header row, returns an array of
records for each row after the header row with fields corresponding to the
header row.


### .s
`( -- )`

Prints the parameter stack. If the interpreter is in "dev mode", triggers a breakpoint; otherwise, raises an Exception to halt execution.



### AM
`( time -- time )`

Given a time object, forces its time value to be A.M.

Example:
```
20:30 AM    # 8:30 AM
```


### PM
`( time -- time )`

Given a time object, forces its time value to be P.M.

### NOW
`( -- time )`

Returns the current time in the timezone configured in the Interpreter.


### >TIME
`( str -- time )`

`( time -- time )`

Given a string, parses the time using the host language's time facilities.

Given a time, this is a no-op.


### <TZ!
`( time tzstr -- time )`

Given a `time` and a timezone string `tzstr`, parses the timezone string and sets the time's timezone to it. The timezone is parsed using the host languages timezone utilities.

For Python timezone strings see [Stack Overflow](https://stackoverflow.com/questions/13866926/is-there-a-list-of-pytz-timezones).


### TIME>STR
`( time -- string )`

Given a time object, renders it as a Forthic time literal.

Example:
```
NOW TIME>STR   # "13:39"
```

### >DATE
`( object -- date )`

Given an `object` tries to convert it into a date. This is typically used with
strings but is safe to use with dates.


### TODAY
`( -- date )`

Returns the current date.


### MONDAY
`( -- date )`

Returns the Monday of this week.

### TUESDAY
`( -- date )`

Returns the Tuesday of this week.

### WEDNESDAY
`( -- date )`

Returns the Wednesday of this week.

### THURSDAY
`( -- date )`

Returns the Thursday of this week.

### FRIDAY
`( -- date )`

Returns the Friday of this week.

### SATURDAY
`( -- date )`

Returns the Saturday of this week.

### SUNDAY
`( -- date )`

Returns the Sunday of this week.


### +DAYS
`( date num_days -- date )`

Given a `date` and an integer `num_days` (which may be negative) returns the
date that as `num_days` after `date`.


### SUBTRACT-DATES
`( ldate rdate -- num_days )`

Returns the number of days `rdate` is after `ldate`.


### SUBTRACT-TIMES
`( ltime rtime -- num_secs )`

Returns the number of seconds `rtime` is after `ltime`.


### DATE>STR
`( date -- string )`

Given a `date` returns a Forthic literal representation of it.

Example:
```
TODAY DATE>STR    # "2020-12-20"
```


### DATE-TIME>DATETIME
`( date time -- datetime )`

Given a date and a time, returns a datetime object. Datetime objects are needed
to construct timestamps (see `DATETIME>TIMESTAMP`)


### DATETIME>TIMESTAMP
`( datetime -- timestamp )`

Given a datetime object, returns associated unix timestamp.

### TIMESTAMP>DATETIME
`( timestamp -- datetime )`

Given a unix timestamp, returns associated datetime object.

### STR>DATETIME
`( str -- datetime )`

Attempts to parse a string as a date/time. Raises error if format is unrecognized.

## Reference: Math Words

### TRUE
`( -- TRUE )`

Returns the host language value for `TRUE`.


### FALSE
`( -- FALSE )`

Returns the host language value for `FALSE`.


### +
`( a b -- sum )`

`( items -- sum )`

Given two objects `a` and `b`, returns their `sum`.

Given an array of items, returns the summation over them.


### -
`( a b -- difference )`

Given `a` and `b`, returns the difference `a - b`.

### *
`( a b -- product )`

Given `a` and `b`, returns their product.

### /
`( a b -- result )`

Given `a` and `b`, returns `a` divided by `b`.


### MOD
`( m n -- m%n )`

Given integers `m` and `n`, returns `m modulo n`.

### ROUND
`( number -- integer )`

Given a number, rounds it to the nearest integer.

### ==
`( a b -- a==b )`

Given two objects, returns `TRUE` if they are equal; `FALSE` otherwise.


### !=
`( a b -- a!=b )`

Given two objects, returns `TRUE` if they are not equal; `FALSE` otherwise.


### >
`( a b -- a>b )`

Given two objects `a` and `b` returns `TRUE` if `a > b`, `FALSE` otherwise.


### >=
`( a b -- a>=b )`

Given two objects `a` and `b` returns `TRUE` if `a >= b`, `FALSE` otherwise.

### <
`( a b -- a<b )`

Given two objects `a` and `b` returns `TRUE` if `a < b`, `FALSE` otherwise.


### <=
`( a b -- a<=b )`

Given two objects `a` and `b` returns `TRUE` if `a <= b`, `FALSE` otherwise.

### OR
`( a b -- bool )`

`( bools -- bool )`

Given `a` and `b`, returns `TRUE` if either of them are `TRUE`; `FALSE` otherwise

Given an array of boolean values, returns `TRUE` if any of them are `TRUE`; `FALSE` otherwise.


### AND
`( a b -- bool )`

`( bools -- bool )`

Given `a` and `b`, returns `TRUE` if both are `TRUE`; `FALSE` otherwise.

Given an array of boolean values, returns `TRUE` if all of them are `TRUE`; `FALSE` otherwise.

### NOT
`( bool -- result )`

If bool is `TRUE`, returns `FALSE`; `TRUE` otherwise.


### IN
`( item items -- bool )`

Returns `TRUE` if `item` is in `items`; `FALSE` otherwise.

### ANY
`( vals required_vals -- bool )`

If `required_vals` is empty, then returns `TRUE`.

If `required_vals` is nonempty, returns `TRUE` if any value in `vals` is in
`required_vals`; `FALSE` otherwise.

Example:
```
["alpha" "beta"] ["beta" "gamma"] ANY   # TRUE
["delta" "beta"] ["gamma" "alpha"] ANY  # FALSE
```

### ALL
`( vals required_vals -- bool )`

Returns `TRUE` if `vals` contains all `required_vals`.

### >BOOL
`( object -- bool )`

If `object` is truthy in the host language, return `TRUE`; otherwise `FALSE`.

### >INT
`( object -- int )`

Given an `object` attempts to convert it into an `int` using the host language.
If this cannot be done, an exception is raised.


### >FLOAT
`( object -- float )`

Given an `object` attempts to convert it into a `float` using the host language.
If this cannot be done, an exception is raised.


### UNIFORM-RANDOM
`( low high -- int )`

Given an integer range `low` and `high`, returns an integer drawn using a
uniformly random distribution over that range.

### RANGE-INDEX
`( val start_ranges -- index )`

Given a value `val` and an array of `start_ranges` that give the starting values of an array of ranges,
this returns the index where `val` falls.

If `val` is less than the first start range, then `NULL` is returned.

NOTE: `start_ranges` must be in ascending order.


## Reference: Profiling Words
These words are used to profile the execution of a Forthic application to
identify bottlenecks and areas for optimization.

### PROFILE-START
`( -- )`

Enables a profiling run. This clears all timestamps and resets all counters.

### PROFILE-TIMESTAMP
`( label -- )`

Logs a `label` and the current time since the start of the profiling run.

### PROFILE-END
`( -- )`

Stops profiling and adds a final `END` timestamp. Returns a `ProfileAnalyzer` and drops the interpreter into debug mode.


### PROFILE-DATA
`( -- data )`

Returns the results of the last profiling run. This is a record with the following fields:

* `word_counts`: This is a map from each Forthic word executed to how many times it was called during the profiling run.
* `timestamps`: This is a list of timestamp labels and timestamps in the order in which they occurred during the profiling run.

### PROFILE-REPORT
`( -- profile_report )`

This returns a formatted string representation of `PROFILE-DATA`.

Sample Report:
```
Word counts:
                             @: 98
                   org.MANAGER: 46
                            !=: 42
                USERNAME>EMAIL: 38
                          REC@: 28
                          NULL: 28
                <ADD-JIRA-LINK: 14

Timestamps (sec):
                         START: 0.000 (0.000)
              recipients-start: 0.001 (0.001)
                recipients-end: 4.375 (4.374)
                           END: 4.378 (0.002)
```

### CURRENT-USER
`( -- username )`

Returns the username for the current user.
