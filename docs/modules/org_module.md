# org_module

The org module can perform organizational lookups and groupings using user/manager
data.

## Example
```
["org"] USE-MODULES

# NOTE: You must set up an OrgContext first

'director1' org.FULL-ORG          # Returns all usernames reporting up to director1
'director1' org.DIRECT-STAFF      # Returns all direct reports of director1
'director1' org.DIRECT-MANAGERS   # Returns all managers who directly report to director1
'user1' org.MANAGER org.MANAGER   # Returns user1's manager's manager
```

## Reference

### PUSH-CONTEXT!
`( context -- )`

Pushes an OrgContext (See `org_module.py`) onto the org module's context stack.
The most recent context is used to perform org computations.


### POP-CONTEXT!
`( -- )`

Pops an OrgContext from the org module's context stack and throws it away.


### FULL-ORG
`( manager -- usernames )`

Returns all usernames that roll up to manager.


### DIRECT-MANAGERS
`( manager -- usernames )`

Returns all managers who report directly to a manager.

### DIRECTS
`( manager -- usernames )`

Returns `usernames` for everyone who report directly to a manager.


### GROUP-BY-LEADS
`( items field leads default_lead -- record )`

Groups a list of items by the specified leads.

The `field` is the username for each item that will be used to roll an item up
into one of the specified `leads`.

If an item can be rolled up into multiple leads, the lead that appears later in
the `leads` list will be used.

If an item doesn't roll up into any of the specified leads, it is grouped into
the `default_lead`.


### ITEM>LEAD
`( item field leads default_lead -- lead)`

Returns the `lead` that an `item` rolls up into. See `GROUP-BY-LEADS` for details on the arguments.


### MANAGER
`( username -- manager)`

Returns the manager for the specified user.


### CHAIN
`( username root_username -- usernames)`

Returns the management chain from the specified user to the specified root user.
If the root user is not in the chain, then only the chain up to effective root
is returned.

### CHAIN-KEY-FUNC
`( username root_username -- key_func )`

Returns a key function that takes a username and returns the number of management levels to the root username. This can be used as the `KEY-FUNC` to the `SORT` word.

Example:
```
: USERS-TO-SORT   ["user1" "mgr2" "mgr3" "director4"];
: ROOT-USER       "ceouser";

USERS-TO-SORT  ROOT-USER org.CHAIN-KEY-FUNC SORT-w/KEY-FUNC   # Sorts by most senior person first
```

### USERS-MANAGERS
`( -- users_managers )`

Returns an array of `[user manager]` pairs from the current org context.