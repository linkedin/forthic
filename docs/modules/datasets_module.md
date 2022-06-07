# datasets_module

The datasets module provides the ability to read/write/upsert arrays of records as coherent datasets.

## Example
```
["datasets"] USE-MODULES

: RECORDS [
    [["id" 1] ["name" "alpha"]] REC
    [["id" 2] ["name" "beta"]] REC
];
: FKEY   "'id' REC@";   # This is forthic to extract a key from each record
: DATALABEL   "greek";  # This identifies a dataset

# Store a dataset
RECORDS FKEY DATALABEL datasets.DATASET!

# Retrieve a dataset
DATALABEL datasets.DATASET
```

## Reference


### DATASET
`( dataset_label -- )`

Retrieves a dataset associated with the specified label.


### RECORDS
`( dataset_label keys -- records )`

Retrieves records from the specified dataset with the specified keys. Missing records are returned as NULL.


### DATASET!
`( records fkey dataset_label -- )`

Overwrites any dataset with the specified label with given records. The unique key for each record is extracted from each record by executing the `fkey` Forthic string.


### RECORDS!
`( records fkey dataset_label -- )`

Upserts the specified records into the specified specified dataset.