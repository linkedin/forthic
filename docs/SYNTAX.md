> **⚠️ ARCHIVED - This documentation is from an archived repository.**
> **For current documentation, visit:** https://github.com/forthix/forthic

---

# Forthic Syntax

Forthic is intended to be the "top" of the application. It relies on a host language like Python and Javascript to provide lower level capability and constructs. Forthic is designed to coordinate chunks of code written in the host languages.

A Forthic program consists of whitespace-separated **words** that are executed sequentially. Words pass data to each other via the **parameter stack**. Some words push data onto the stack, while some words consume data from the stack, possibly pushing new data onto the stack.

## Tokens

### Literal words

Literal words push their own values onto the stack. For example:

```
1
2.4
2021-02-18
09:30
```

### Comments and strings

Comments and strings are in the style of Python:

```
# This is a comment
"Double quoted string"
'Single quoted string'
"""Triple double quoted string"""
'''Triple single quoted string'''
```

### Parentheses are whitespace

Parentheses are whitespace and have no other syntactic meaning.

### Start and end array

Arrays are constructed using a "start array token" '`[`' and an "end array token" '`]`':

```
[ 1 2 3 ]
```

Because arrays are constructed using words and not syntactical elements, things like this are possible:

```
1 [ SWAP 2 3 ]   # Has the same effect as [ 1 2 3 ]
```

The start/end array tokens are recognized directly by the Tokenizer, and so whitespace is not required when using them:

```
[1 2 3]    # Same as [ 1 2 3 ]
```

### Definitions

New words can be created out of existing words using "start definition" (`:`) and end definition (`;`) tokens:

```
: DOUBLE   2 * ;
```

Definitions can use previously defined words:

```
: DOUBLE      2 *;
: QUADRUPLE   DOUBLE DOUBLE;   # yum
```

In the interpreter, definitions are stored in an array rather than a dictionary. This allows them to be redefined without affecting existing definitions that already use them:

```
: DOUBLE      2 *;
: QUADRUPLE   DOUBLE DOUBLE;

3 DOUBLE      # 6

: DOUBLE      [ SWAP DUP ];

3 QUADRUPLE   # 12
3 DOUBLE      # [3 3]
```

### Memoized Definitions

A definition can be memo-ized by using `@:` instead of `:`. When a memoized definition is called for the first
time, it behaves like an ordinary definition except that it stores a copy of the top of the stack. On subsequent
calls, it simply returns that stored copy. The memoized value can be refreshed by calling the memoized definition
with a `!` appended. For example:

```
@: TICKETS   JQL FIELDS jira.SEARCH;

TICKETS   # Will do the Jira search and save a copy of the results
TICKETS   # Will return the results from the previous call

TICKETS!  # Will perform the Jira search again and save a new copy of the results
TICKETS   # Will return the latest copy of the results
```

## Modules

All definitions are stored within Forthic modules. A Forthic application can use any number of modules, but two of them are special: the app module and the global module. The app module is the default module for a Forthic program. For a plain Forthic app, new definitions are added here. The global module contains all of the "built-in" Forthic words (like `SWAP`, `DUP`, `MAP`, `*`, `+`, etc.)

Modules can be created within Forthic using the start/end module tokens. For example, this creates a `my-module` module with a definition of `DOUBLE` inside of it:

```
{my-module
    : DOUBLE   2 *;
}
```

If a module already exists, you can "open it" again using the same syntax:

```
{my-module
    : DOUBLE   2 *;
}

{my-module
    4 DOUBLE    # 8
}

```

When a module is created, it is created within the context of the current module. In the previous example, `my-module` is created within the app module. When modules are nested, a "module stack" is created:

```
{first
    {second
        {third
            # At this point, the current module is `third`
            # The next module in the module stack is `second`
            # The final module in the module stack is `first`
        }
    }
}
```

When a word is to be executed, the modules are searched in this order:

1. Current module and the active module stack
2. App module
3. Global module

New Forthic modules can be created in the host language by subclassing `Module`. Words defined and exported from a module become available to a Forthic program when imported via `USE-MODULES`.

## Variables

Like definitions, variables exist within modules. The words used to create, read, and set variables are defined in the `global` module:

```
["x" "y"] VARIABLES     # Defines variables `x` and `y` in the current module
20 x !                  # The `!` word sets the value of x to 20
5  y !                  # Set y to 5
x @                     # The `@` word pushes the current value of `x` onto the stack
y @                     # Stack is now (20 5)
*                       # 100
```

## Global Module

The `global` module defines all of the core words in Forthic. While similar in spirit to C's Standard Library, the `global` module also defines base words for doing things that could arguably be built into the language (e.g., defining and using variables). This keeps the Forthic interpreter as simple as possible.

The `global` module contains words that are expected of every Forthic interpreter regardless of host language, but it may also contain words that are specific to a host language. For instance, an interpreter running in a web browser has the global `HASH-PARAMS` word to get the hash parameters for the current page's URL.

The `global` module defines the following types of words: base words, array/record words, string words, date/time words, math words, profiling words, and misc words. For more details, see the `global module` documentation.

### Base Words

-   `VARIABLES` defines variables in a module
-   `!` sets value of a variable
-   `@` gets value of a variable
-   `INTERPRET` runs a Forthic string
-   `MEMO` memoizes the results of a Forthic string (deprecated, use `@:` instead)
-   `EXPORT` makes words from the current module importable
-   `USE-MODULES` imports words from the specified modules
-   `REC` creates a record object (like a Python dictionary)
-   `REC@` retrieves the value of a field from a record
-   `<REC!` sets the value of a record's field, leaving the record on the stack
-   `SCREEN!` stores a "screen" of Forthic code
-   `SCREEN` returns a "screen" of Forthic code as a string
-   `LOAD-SCREEN` loads and runs a "screen" of Forthic code
-   `POP` pops a value from the parameter stack and throws it away
-   `DUP` pushes a copy (reference) of the top of the parameter stack
-   `SWAP` reverses the order of the top two elements of the parameter stack

### Array/Record Words

-   `APPEND` adds an element to an array/record
-   `REVERSE` reverses the order of an array
-   `UNIQUE` ensures unique values
-   `<DEL` deletes an element
-   `RELABEL` changes indexes/keys in an array/record
-   `BY-FIELD` organizes elements by a field
-   `GROUP-BY-FIELD` groups elements by a field
-   `GROUP-BY` groups elements using a Forthic string
-   `GROUP-BY-w/KEY` is the same as `GROUP-BY`, but Forthic string expects a key and value
-   `GROUPS-OF` splits a collection of elements into groups of a specified length
-   `MAP` maps a Forthic string over a collection of elements
-   `MAP-w/KEY` is the same as `MAP` but the Forthic string expects a key and value
-   `FOREACH` runs a Forthic string over a collection of elements
-   `FOREACH-w/KEY` is the same as `FOREACH` but the Forthic string expects a key and value
-   `ZIP` takes two containers and returns a new container containing corresponding pairs of elements
-   `ZIP-WITH` is like `ZIP` but uses a Forthic string to return the element values
-   `KEYS` returns the indexes/keys of a container
-   `VALUES` returns the values of a container
-   `LENGTH` returns the number of items in a container
-   `SLICE` returns a slice of elements from a container
-   `DIFFERENCE` returns the items in the first container that are missing from the second
-   `INTERSECTION` returns the items that are in both containers
-   `UNION` returns items that are in either container
-   `SELECT` filters a container using a Forthic string predicate
-   `SELECT-w/KEY` is like `SELECT`, but the Forthic string expects a key and a value
-   `TAKE` returns the first `n` elements of a container
-   `DROP` returns a container with the first `n` elements dropped
-   `ROTATE` takes the last element of a container and puts it at the start
-   `ROTATE-ELEMENT` takes an element and puts it at the start
-   `SHUFFLE` randomizes the order of elements in a container
-   `SORT` sorts the container using the default comparison
-   `SORT-w/FORTHIC` sorts a container using a Forthic key function
-   `SORT-w/KEY-FUNC` sorts a container using a host language key function
-   `FIELD-KEY-FUNC` returns a host language key function that returns the value of an element's field
-   `NTH` returns the nth element of a container
-   `LAST` returns the last element of a container
-   `UNPACK` takes an array and pushes its elements on the stack
-   `FLATTEN` takes an array of nested arrays and removes all levels of nesting
-   `KEY-OF` returns the index/key of an element in a container
-   `REDUCE` reduces a container using an initial value and a Forthic string

### String words

-   `CONCAT` concatenates two strings or an array of strings
-   `SPLIT` takes a string and splits it by a separator into an array of strings
-   `JOIN` takes an array of strings and a separator and joins them by the separator
-   `/N` pushes a newline (\n) onto the stack
-   `/R` pushes a carriage return (\r) onto the stack
-   `/T` pushes a tab (\t) onto the stack
-   `|LOWER` converts a string to lowercase
-   `|UPPER` converts a string to uppercase
-   `|ASCII` strips out non-ASCII characters from a string
-   `STRIP` removes surrounding whitespace from a string
-   `REPLACE` replaces subtrings in a string with another substring
-   `RE-MATCH` performs a regular expression match on a string
-   `RE-MATCH-GROUP` returns the nth match from the result of `RE-MATCH`
-   `>STR` converts the top of the stack to a string
-   `URL-ENCODE` escapes a string so it can be added as a query param in a URL
-   `URL-DECODE` unescapes a url-encoded string

### Date/time words

-   `AM` forces a time literal to AM (e.g., 20:30 becomes 8:30 AM)
-   `PM` forces a time literal to PM (e.g., 8:30 becomes 20:30)
-   `NOW` returns the current time
-   `>TIME` converts an object to a time value if possible
-   `<TZ!` specifies the timezone of a time value
-   `TIME>STR` converts a time value to a string (military time)
-   `>DATE` converts an object to a date if possible
-   `TODAY` returns the current date
-   `MONDAY` returns the date of the Monday of this week
-   `TUESDAY` returns the date of the Tuesday of this week
-   `WEDNESDAY` returns the date of the Wednesday of this week
-   `THURSDAY` returns the date of the Thursday of this week
-   `FRIDAY` returns the date of the Friday of this week
-   `SATURDAY` returns the date of the Saturday of this week
-   `SUNDAY` returns the date of the Sunday of this week
-   `NEXT` is used with `MONDAY`, `TUESDAY`, etc. to return the next `MONDAY`, `TUESDAY`, etc.
-   `+DAYS` adds a number of days to a date
-   `SUBTRACT-DATES` returns the number of days difference between two dates
-   `SUBTRACT-TIMES` returns the difference between two times in seconds
-   `DATE>STR` converts a date to a string like "2021-02-18"
-   `DATE-TIME>DATETIME` combines a date and time into a datetime value
-   `DATETIME>TIMESTAMP` returns the unix timestamp for a datetime
-   `TIMESTAMP>DATETIME` returns the datetime for a unix timestamp

### Math words

-   `FALSE` pushes the host language's idea of false onto the stack
-   `TRUE` pushes the host language's idea of true onto the stack
-   `+` adds two numbers
-   `-` subtracts two numbers
-   `*` multiplies two numbers
-   `/` divides two numbers
-   `MOD` returns the modulo of two numbers
-   `ROUND` rounds a number to the nearest integer
-   `==` checks if two items are equal
-   `!=` checks if two items are not equal
-   `>` checks if the earlier stack item is greater than the top of the stack
-   `>=` checks if the earlier stack item is greater than or equal to the top of the stack
-   `<` checks if the earlier stack item is less than the top of the stack
-   `<=` checks if the earlier stack item is less than or equal to the top of the stack
-   `OR` returns `TRUE` either of the top two stack items is `TRUE`; if top is an array, then `TRUE` if any of the array elements is `TRUE`
-   `AND` returns `TRUE` if the top two stack items are `TRUE`; if top is an array, then `TRUE` if all array elements are `TRUE`
-   `NOT` inverts a Boolean value
-   `IN` returns `TRUE` if an item is in an array
-   `ANY` returns `TRUE` if any item in an array is in another array
-   `ALL` returns `TRUE` if all items in an array are in another array
-   `>BOOL` converts a value to a Boolean
-   `>INT` converts a value to an integer
-   `>FLOAT` converts a value to a floating point number
-   `UNIFORM-RANDOM` returns a random value between two numbers

### Profiling words

-   `PROFILE-START` begins profiling a Forthic program
-   `PROFILE-TIMESTAMP` adds a timestamped label to a profiling run
-   `PROFILE-END` stops the profiling of a Forthic program and returns an object for analyzing expensive calls
-   `PROFILE-DATA` returns stats for the most recent profiling run
-   `PROFILE-REPORT` returns a formatted string version of `PROFILE-DATA`

### Misc words

-   `NULL` returns the host language's `null` value (e.g., `None` in Python)
-   `QUOTE-CHAR` returns an untypeable ASCII character for quoting arbitrary strings sent between two different Forthic interpreters (e.g., browser and server)
-   `DEFAULT` if the top of stack is `NULL` then replaces it with a specified value
-   `*DEFAULT` if top of stack is `NULL` runs a Forthic string to replace its value
-   `<REPEAT` repeatedly executes a Forthic string a specified number of times against an item, leaving the original item on the stack
-   `IDENTITY` does nothing
-   `>FIXED` converts a number to a string with a specified number of significant digits
-   `>JSON` converts an object to a JSON string
-   `JSON>` converts a JSON string to an object
-   `>TSV` converts an array of items to a TSV string
-   `>TSV` converts a TSV string to an array of items
-   `RECS>TSV` converts an array of records to a TSV string with a header corresponding to the record fields
-   `TSV>RECS` converts a TSV string with a header row to an array of records with fields corresponding to the header row
-   `.s` prints the parameter stack to the console and drops the Forthic interpreter into the host language's debugger
