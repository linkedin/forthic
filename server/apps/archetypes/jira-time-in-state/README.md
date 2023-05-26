# archetypes/jira-time-in-state
This archetype shows how to use the jira.TIME-IN-STATE word to compute how many hours tickets were in each of their workflow states. The final result is written to a gsheet.

## What the app does
After configuring the app with JQL, ticket states of interest, and an output gsheet URL, the app will do the following:

1. Pull the Jira tickets
2. Add a `status` changelog to each ticket
3. Compute the time in state for each ticket, adding this to each ticket as `time_metrics`
4. Write the ticket data to the cache (for review)
5. Construct a gsheet report
6. Publish the gsheet report to the specified gsheet


## How to configure the app
To configure basic operation of the app, update the `TWEAK: Configuration` section:

```
: JQL             "TODO-REPLACE-THIS-WITH-YOUR-JQL";
: TICKET-STATES   ["Open" "In Progress" "Blocked" "On Hold" "Closed"];
: FIELDS          ["Resolution" "Summary" "Reporter" "Assignee" "Created" "Resolved" "Labels"];
: GSHEET          "https://docs.google.com/spreadsheets/d/TODO-REPLACE-WITH-YOUR-GSHEET";
```

* **JQL**: This specifies the tickets you want to pull for your report. Be sure to run this in Jira to check that you're getting what you expect.
* **TICKET-STATES**: These are the workflow states that you're interested in. You don't have to include them all (and you can include states from different Jira projects), but these are the ones that will be reported.
* **FIELDS**: For a basic configuration, leave these as is--we'll talk about how to add fields to your report below.
* **GSHEET**: This is the URL of the gsheet that you want to write to


## How to run the app
To get the app to do something, uncomment `UPDATE` in the `TWEAK: UPDATE` section and reload the page at http://localhost:8000/archetypes/jira-time-in-state/


## Tweak: Change the column order of the gsheet report
The order of the columns in your gsheet is determined by the `DATA-HEADERS` word. Note that this word includes `TICKET-STATES` in its definition, so if you want to change the order of the states, you'll need to revise that as well:

```
: DATA-HEADERS     ["Created" "Jira"  "Summary" TICKET-STATES "Reporter" "Assignee" "Labels" "Resolved"] FLATTEN;
```

## Tweak: Remove a column from the gsheet report
To remove a column, just remove it from the `DATA-HEADERS` array or from the `TICKET-STATES` array:

```
: TICKET-STATES    ["Open" "In Progress" "Closed"];
: DATA-HEADERS     ["Created" "Jira"  "Summary" TICKET-STATES "Assignee"] FLATTEN;
```

## Tweak: Add a column
To add a column, you'll need to do the following:

1. Add the Jira field of interest to the `FIELDS` array. NOTE that the field should match how the Jira UI displays the field
```
: FIELDS          ["Resolution" "Summary" "Reporter" "Assignee" "Created" "Priority"];
```

2. Add a word to return the value of the field from a ticket. NOTE that the field should match the one you added to the `FIELDS` array
```
: ticket-PRIORITY   ticket @ "Priority" REC@;
```

3. Add a call to your new word to the `TICKET>ROW-REC` word:
```
: TICKET>ROW-REC   (ticket !) [
    ["Jira"          ticket-KEY]
    ["Summary"       ticket-SUMMARY]
    ["Created"       ticket-CREATED]
    ["Reporter"      ticket-REPORTER]
    ["Assignee"      ticket-ASSIGNEE]
    ["Labels"        ticket-LABELS ", " JOIN]
    ["Priority"      ticket-PRIORITY]         # <-------- Add your word here
    TICKET-STATES "(state !) [state @  ticket+state-TIME-METRIC]" MAP UNPACK
] REC;
```

4. Add your new field to the `DATA-HEADERS` array:
```
: DATA-HEADERS     ["Priority" "Created" "Jira"  "Summary" TICKET-STATES "Reporter" "Assignee" "Labels"] FLATTEN;
```