# TicketsModal

The `TicketsModal` is a specialized modal dialog used to display ticket records dynamically and, optionally, send custom emails to assignees/managers.

The `TicketsModal` subscribes to a MessageBroker and when it sees a message with ticket records in it (specified by the `records_field` prop), it shows the modal dialog.

NOTE: Configuring the email is somewhat intricate and should be considered an undocumented feature for now.

## Props
* `message_broker`: a MessageBroker the TicketsModal subscribes to for state change messages
* `records_field`: The name of the tickets records field in messages from the message_broker. This is used to retrieve ticket records from published messages.
* `column_infos`: List of all column infos that could be rendered. Only those contained in the records will be shown
* `records_table_props`: same props as RecordsTable, except that the "records" field is superceded by the records published by the message_broker
* `logged_in_username` (optional): Username of current user
* `frecipients` (optional): Forthic expecting (recipient_info tickets) and returns a list of recipient usernames
* `frender_user_email` (optional):  Forthic expecting (recipient_info draft_info tickets username) and returns rendered email
* `fgenerate_emails` (optional):  Forthic expecting (recipient_info draft_info tickets) and returns rendered emails
* `fsend_emails` (optional):  Forthic expecting test mode flag and email records and returns an array of send errors
* `interp`: Forthic interpreter. This is automatically set by the global module.

### recipient_info
This is a record that holds info about the email recipients. It has the following fields:

* `recipientType`: One of `assignee`, `manager`, `manager_manager`
* `ccType`: One of `assignee`, `manager`, `manager_manager`
* `ccWritein`: Comma separated usernames/emails

### draft_info
This is a record representing an email draft. It has the following fields:
* `subject`: The email subject
* `body`: The email body (in Markdown)

## Examples

### SimpleExample
```
: COLUM-INFOS [
    [["field" "key"] ["label" "Letter"]] REC
    [["field" "greek"]] REC
];

: TICKETS-MODAL   TicketsModal [
    ["message_broker"   PAGE-BROKER]
    ["records_field"    "tickets"]
    ["column_infos"     COLUMN-INFOS]
    ["record_table_props"   [
        ["pagination_info"  [["page_size"  10]] REC]
    ] REC]
] REC <PROPS;
```