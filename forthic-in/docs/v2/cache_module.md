# cache_module

The cache module allows you to store the top of the stack on disk as JSON (the
stored object must be JSON serializable) and retrieve it later. This is useful
for storing expensive computations or API responses.

The data is stored in a `.cache` file in the working directory. The directory
can be set using the `CWD!` word.

Every object is stored and retrieved using a label.

## Example
```
["cache"] USE-MODULES

"~/my_stuff" cache.CWD!                    # Sets the current working directory
[1 2 3 "Howdy"]  "my_array" cache.CACHE!   # Stores the array in the cache

"my_array" cache.CACHE@                    # Retrieves the array from cache
3 NTH                                      # ([1 2 3 "Howdy"] -- "Howdy")
```

## Reference

### CACHE!
`(object key --)`

Stores `object` in the cache at the specified `key`. The object will be
serialized to JSON.


### CACHE@
`(key -- object)`

Retrieves an object from the cache at the specified `key`. The object will be
deserialized from JSON.