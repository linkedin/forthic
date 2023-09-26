# intake_module

The intake module supports the configuration of Forthic-based intake forms and provides some
utility words for setting up multi-step forms and for aggregating the values of fields that
map values to the same Jira field (for instance, to map multiple fields to sections of a Jira Description).

## Example
See [intake-example](../../server/apps/coding-forthic/intake-example/main.forthic)

```
["cache" "intake"] USE-MODULES

# ----- Configuration -----------------------------------
: FORM-ADMINS   ['rino@forthix.com'];
: CONFIG-URL    "https://docs.google.com/spreadsheets/d/my-gsheet-path";

: SIMPLE-FORM-CONFIG   [
    ["tab"          "Simple"]
    ["Project"      'CF']
    ["Issue Type"   'Task']
    ["Labels"       ["forthic-intake"]]
] REC;

: FORM-CONFIGS [
    SIMPLE-FORM-CONFIG
];

: REFRESH-CONFIGS!   CONFIG-URL FORM-CONFIGS FORM-ADMINS intake.REFRESH-CONFIGS!;
```

## Reference

### REFRESH-CONFIGS!
`( url configs admins -- )`

Ensures that gsheet tabs exist for all configured forms and then stores the config data in the cache under the `intake__form_configs` key.

- `url` is the URL for your Google Sheet (any tab is fine)
- `configs` is an array of config records with *at least* a `tab` field that names a tab in the gsheet. If the tab does not exist, it will be created and filled out with a template. Other fields can be added to this config record. The config record will be passed back as part of the user submitting a ticket
- `admins` is an array of admin usernames (or emails, depending on how your Jira instance is configured). Any user in the `admins` array will see a "Refresh Configs" button that can be clicked to do a refresh of the gsheet configurations


### FORM-CONFIGS
`( -- form_configs )`

Returns an array of form config records from the cache. The `REFRESH-CONFIGS!` word must be called prior to calling this word.


### STEP
`( step_id step_fields transitions -- step-record )`

If desired, a form can be configured to be multi-step (as well as multi/branching steps). We use the `STEP` word to wrap the necessary information in a record.

- `step_id` is the unique ID of a step. This isn't user-visible; it's only used to refer to specific steps
- `step_fields` is an array of field IDs (these are the IDs from a form's gsheet tab). Only the specified fields will be part of the configured step
- `transitions` is an array of step transitions (see the `TRANSITION` word). A transition specifies a next possible step from the current step, which is enabled when a transition predicate evaluates to `TRUE`. If `transitions` is NULL, then this step is the final step in the form


### TRANSITION
`( next_step_id fcondition -- transition )`

This is a convenience word for creating a transition record.

- `next_step_id` is the ID of the next step for this transition
- `fcondition` is a Forthic string that when evaluates to `TRUE` indicates that the transition is active and will be the next step in the form after all required fields have values


### AGGREGATE-JIRA-FIELDS
`( ticket_info jira_field -- value_by_field_id )`

This aggregates configures fields that share the same Jira field

- `ticket_info` is a record with the following fields:
    - formConfig: This is a form configuration record defined in the application (see `REFRESH-CONFIGS!`)
    - valuesById: This is a record mapping "Field ID" to user entered value
    - fieldsById: This is a record mapping "Field ID" to configured values from the gsheet
- `jira_field` is the Jira field of interest