# intake_module

The intake module supports the configuration of Forthic-based intake forms and provides some
utility words for setting up multi-step forms and for aggregating the values of fields that
map values to the same Jira field (for instance, to map multiple fields to sections of a Jira Description).

In some of the words below, the "INFO" record is constructed by the React intake module's CREATE-TICKET word
and has the following fields:
- `formConfig`: This is a record of the form config record the user specified in their main.forthic file
- `fieldsById`: This is a record mapping field ID to the field information in the gsheets
- `valuesById`: This is a record mapping field ID to the values a user has filled out in the form

## Example
See [intake-simple](../../server/apps/coding-forthic/intake-simple/main.forthic)

## Reference

### REFRESH-CONFIGS!
`( google_context url configs admins -- )`

Ensures that gsheet tabs exist for all configured forms and then stores the config data in the cache under the `intake__form_configs` key.

- `google_context` is the context ("credentials") for accessing the Google Sheets API
- `url` is the URL for your Google Sheet (any tab is fine)
- `configs` is an array of config records with *at least* a `tab` field that names a tab in the gsheet. If the tab does not exist, it will be created and filled out with a template. Other fields can be added to this config record. The config record will be passed back as part of the user submitting a ticket
- `admins` is an array of admin usernames (or emails, depending on how your Jira instance is configured). Any user in the `admins` array will see a "Refresh Configs" button that can be clicked to do a refresh of the gsheet configurations


### FORM-CONFIGS
`( -- form_configs )`

Returns an array of form config records from the cache. The `REFRESH-CONFIGS!` word must be called prior to calling this word.


### INFO>TICKET-RECORD
`( info -- ticket_record )`

This constructs a ticket record by aggregating values and extracting values by Jira field. It should be suitable to use directly by jira.CREATE-TICKET


### INFO>ATTACHMENTS
`( info -- attachments_record )`

This constructs a record mapping filename to attachment.
It is suitable to use directly by jira.ADD-ATTACHMENTS


### APPEND-FORM-CONFIG-FIELD
`( ticket_record jira_field -- ticket_record )`

This is a utility word that appends a field specified in a
form config record to the appropriate field in a ticket
record. It handles the case where there are existing
values inputted by the user or if there are no values
