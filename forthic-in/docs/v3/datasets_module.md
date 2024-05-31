# datasets_module

The datasets module provides the ability to read/write/upsert arrays organized in fields
of a record.

## Example
```
["datasets"] USE-MODULES

: EVENS   [2 4 6 8 10];
: ODDS    [1 3 5 7 9];
: PRIMES  [3 5 7 11];

: NUMBERS [
    ["evens"   EVENS]
    ["odds"    ODDS]
    ["primes"  PRIMES]
] REC;

# Store a dataset
"numbers" datasets.DATASET!

# Retrieve a dataset
"numbers" datasets.DATASET

# Update a dataset
[["evens"  [2 4 6 8 10 12]]] REC dataset.DATASET!

# Pull specific fields from a dataset
["odds" "primes"] dataset.RECORDS

# Missing fields: Default is to return NULL
["odds" "garbage" "primes"] dataset.RECORDS   # => [[1 3 5 7 9] NULL [3 5 7 11]]

# Missing fields: Drop NULL values
["odds" "garbage" "primes"] dataset !DROP-NULLS dataset.RECORDS   # => [[1 3 5 7 9] [3 5 7 11]]

# Overwrite a dataset
[["evens"  [2 4 6 8 10 12]]] REC dataset.!OVERWRITE dataset.DATASET!
```

## Reference


### DATASET!
`( record dataset_label -- )`

Updates a dataset with the new record information. By default, the specified `record` is merged into the dataset (if it exists)

If `!OVERWRITE` is called just before this, then the specified `record` will overwrite the dataset


### DATASET
`( dataset_label -- record )`

Returns the dataset record for the corresponding `dataset_label`. If there is no dataset, an empty record is returned.


### RECORDS
`( dataset_label keys -- values )`

For the dataset specified by `dataset_label`, returns the values associated with each of the `keys`. The order of the values matches the order of the keys. If a key is missing, then the corresponding array value is `NULL`.

If `!DROP-NULLS` is called just before this, then the `NULL` values are dropped from the result.


## Flag words
These words can change the behavior of the words in this module

### !OVERWRITE
`( -- )`

Changes the behavior of `DATASET!` so it overwrites rather than merges records.

### !DROP-NULLS
`( -- )`

Changes the behavior of `RECORDS` so missing values are dropped rather than returned as `NULL`