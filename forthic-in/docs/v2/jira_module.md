# jira_module

The jira module provides access to the Jira API for pulling data and
manipulating tickets.

## Example
```
["jira"] USE-MODULES

# NOTE: You must set up a JiraContext first (see ../apps/examples/ex_jira.py)

: JQL      "assignee=currentUser() and resolution is null";
: FIELDS   ['Summary' 'Assignee'];

# Returns ticket records with the specified fields matching the specified JQL
JQL FIELDS jira.SEARCH

: TICKET-REC   [
   ["Project" "A-JIRA-PROJECT"]
   ["Summary" "A sample ticket"]
   ["Issue Type"  "Task"]
] REC;

# Creates a new Jira ticket
TICKET-REC jira.CREATE

["changes"] VARIABLES

# Gets assignee for "PROJECT-1234" as of 2020-07-25
"PROJECT-1234" ["Assignee"] jira.CHANGELOG changes !
2020-07-25 changes @ 'Assignee' jira.FIELD-AS-OF
```

## Reference

### PUSH-CONTEXT!
`( context -- )`

Pushes a JiraContext (See `jira_module.py`) onto the jira module's context stack.
The most recent context is used to provide credentials to access the Jira API.
The JiraContext must be configured in Python.

When a JiraContext is instantiated, it queries its configured Jira server to
pull custom field information. This is used to map custom field labels into
custom field IDs, which allows users to work entirely in terms of custom field
names.


### POP-CONTEXT!
`( -- )`

Pops a JiraContext from the jira module's context stack and throws it away.


### HOST
`( -- host )`

Returns the Jira host for the most recently pushed JiraContext.

This is useful for constructing links to tickets and queries.


### SEARCH
`( jql fields -- tickets )`

Given a JQL string and an array of field names, queries Jira and returns an array of
tickets with the specified field names.

This also parses the Jira response to return simple values (e.g., names instead
of records that wrap the names). To get the complete Jira response, use `DEFAULT-SEARCH`


### DEFAULT-SEARCH
`( jql fields -- tickets )`

Similar to `SEARCH` but does not attempt to simplify the Jira responses. The
resulting tickets will have default response values for the specified fields.


### RENDERED-SEARCH
`( jql fields -- tickets )`

Similar to `SEARCH` but expands `renderedFields` so that the resulting response contains values as rendered in the Jira Web UI.

### CREATE
`( record -- ticket_key )`

Given a record with Jira fields as keys, creates a new Jira ticket, returning
the new ticket key. Custom fields are also valid, but the values must be
consistent with the configured schema.

Example:
```
: TICKET-REC   [
   ["Project" "MYPROJ"]
   ["Summary" "A sample ticket"]
   ["Issue Type"  "Task"]
] REC;

TICKET-REC jira.CREATE
```

### UPDATE
`( ticket_key record -- )`

Given a record with Jira fields as keys, this updates the specified ticket with those values.

Example:
```
"MYPROJ-1234" [["Risk_Factor" "Yellow"]] REC jira.UPDATE
```

### ADD-WATCHER
`( ticket_key username -- )`

Adds `username` as watcher of the specified ticket.


### LINK-ISSUES
`( in_key out_key link_type -- )`

Creates a link between two tickets. The `in_key` refers to the `inwardIssue` for the link; the `out_key` for the `outwardIssue`.

The `link_type` must be one of the configured link types (e.g., "Dependency" or "Duplicate"). The jira module defines the following convenience words:
```
: DEPENDENCY            "Dependency";   # in_key "depends on" out_key
: ACTION-ITEM           "Action Item";  # in_key "has action item for" out_key
: CLONERS               "Cloners";      # in_key "cloned from" out_key
: DUPLICATE             "Duplicate";    # in_key "duplicates" out_key
: ISSUE-SPLIT           "Issue Split";  # in_key "split to" out_key
: RELATED               "Related";      # in_key "related to" out_key
: REQUIRE               "Require";      # in_key "requires" out_key
```


### VOTES
`( ticket_key -- votes )`

Returns an array of usernames who have voted on the specified ticket.


### COMMENTS
`( ticket_key -- comments )`

Returns an array of comments for the specified ticket


### ADD-COMMENT
`( ticket_key comment -- )`

Adds a new comment to a ticket.


### TRANSITIONS
`( ticket_key -- transitions )`

Returns an array of possible transitions for the specified ticket.


### TRANSITION!
`( ticket_key transition_id -- )`

Sets the state of a ticket to the specified `transition_id`




### CHANGELOG
`( ticket_key fields -- changes )`

Returns a list of ticket changes involving the specified fields. The `changes` will have this form:
```
[
   {
      'date': datetime.datetime(2020, 10, 5, 18, 39, 30, tzinfo=tzutc()),
      'field': 'Assignee',
      'from': '',
      'to': 'user1'
   }
]
```


### FIELD-AS-OF
`( date changes field -- value )`

Returns the value of a field as of a date given an array of `changes` from a `CHANGELOG` call.

Example:
```
['changes'] VARIABLES

"MYPROJ-1234" ["Assignee"] jira.CHANGELOG changes !

2020-07-25 changes @ "Assignee" jira.FIELD-AS-OF   # ( -- "user1" )
```

### FIELD-AS-OF-SINCE
`( as_of_date changes field since_date -- value )`

Returns the value of a `field` as of a date since another date given an array of `changes` from a `CHANGELOG` call.
If there was no change since the `since_date`, then this returns `NULL`

Example:
```
['changes'] VARIABLES

"MYPROJ-1234" ["Assignee"] jira.CHANGELOG changes !

2020-07-25 changes @ "Assignee" 2020-09-01 jira.FIELD-AS-OF-SINCE   # ( -- "user1" )
```

### FIELD-CHANGE-AS-OF
`( date changes field -- change )`

Returns the change record of a field as of a date given an array of `changes` from a `CHANGELOG` call.

Example:
```
['changes'] VARIABLES

"MYPROJ-1234" ["Assignee"] jira.CHANGELOG changes !

2020-07-25 changes @ "Assignee" jira.FIELD-CHANGE-AS-OF   # ( -- change_record )
```

### TIME-IN-STATE
`( resolution changes field -- record )`

Returns a record mapping a field value into number of hours that the ticket spent in that state. The `resolution`
comes from the Jira's `Resolution` field. The `changes` come from a `CHANGELOG` call. The `field` is the field
of interest.

If the ticket has been resolved, then duration of the final state be set 0. If the ticket
is unresolved, the duration of the final state will be the elapsed time since the final state was set.

If a ticket was moved back and forth between different states, the total time spent in each state will
be accumulated in the result.

Example:
```
['changes'] VARIABLES

"PROJ-1234" ["status"] jira.CHANGELOG changes !

"Resolved" changes @ "status" jira.TIME-IN-STATE

# Returns something like
# {'Open': 793.6, 'Scoping': 856.9, 'In Development': 900.7, 'Closed': 0.0})
```

### <FIELD-TAG!
`(ticket_rec field tag value -- ticket_rec )`

Adds a "field tag" to a ticket. A field tag is value that's stored in the text field of a
ticket for later extraction. It is like a poor man's custom field.

Example: This string has two field tags, `objective` and `poc`
```
This is a string as you might find in a ticket description.

Field tags can appear anywhere in the string.

                   [poc: user1]
 Here's a field tag  ^^^^^^^

[objective: This objective can extend
over multiple
lines ]
```

### FIELD-TAG
`( ticket_rec field tag -- value )`

Given a ticket record, extracts the a "field tag" from the specified field.


### REMOVE-FIELD-TAGS
`( string -- string )`

Removes any field tag from the given string. A field tag has the form `[tag: content]`. This does not remove any wiki markup links of the form `[http...|text]`

### SPRINTQUERY
`(rapidViewId -- data)`

Given the `rapidViewId` of a sprint board, this returns a record with the fields `rapidViewId` and `sprints`.
The value of `sprints` is an array of sprint records with the following fields:

* `id`: Sprint id
* `name`: Name of sprint
* `state`: State of sprint (e.g., `CLOSED`)


### RAPID-CHARTS-SPRINTREPORT
`(rapidViewId sprintId -- data)`

Given the `rapidViewId` of a sprint board and a `sprintId`, this returns a record of sprint retrospective information:
```
{
    "contents": {
        "completedIssues": [],
        "issuesNotCompletedInCurrentSprint": [],
        "puntedIssues": [],
        "issuesCompletedInAnotherSprint": [],
        "completedIssuesInitialEstimateSum": {},
        "completedIssuesEstimateSum": {},
        "issuesNotCompletedInitialEstimateSum": {},
        "issuesNotCompletedEstimateSum": {},
        "allIssuesEstimateSum": {},
        "puntedIssuesInitialEstimateSum": {},
        "puntedIssuesEstimateSum": {},
        "issuesCompletedInAnotherSprintInitialEstimateSum": {},
        "issuesCompletedInAnotherSprintEstimateSum": {},
        "issueKeysAddedDuringSprint": {}
    },
    "sprint": {}
}
```
