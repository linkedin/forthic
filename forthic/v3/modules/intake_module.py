from ..module import Module
from ..interfaces import IInterpreter

# NOTE: This requires the gsheet module to be used in the app module
FORTHIC = """
# ----- gsheet setup -----------------------------------------------------------------------------------------
["gsheet" "cache"] USE-MODULES

["url" "configs" "config" "tab" "admins" "content" "type" "google_context" "info"] VARIABLES

: url-GSHEET    url @ gsheet.SPREADSHEET;
: tab-TAB   url-GSHEET tab @ gsheet.TAB@;

# NOTE: These are the fields used by the ConfigurableForm field records
: HEADER-INFO   [
    ["Field ID"            "A unique identifier for the given field"]
    ["Jira Field"          "The exact name of a Jira field as it appears in your Jira instance UI (or something like customfield_1234)"]
    ["Field Label"         "The of the field shown to users of the form"]
    ["Field Description"   "Extra  information that shows up beneat the field label in your form"]
    ["Is Required?"        "All required fields must be filled out in order to submit the form"]
    ["Field Type"          "The type of field control: Dropdown, TextInput, Textarea, RadioCheckbox, MultiCheckbox, DateInput, Attachment"]
    ["Field Content"       "Default content for a TextInput/Textarea. For Dropdowns and checkboxes, this specifies the options, one per line"]
    ["Max Input Length"    "The maximum number of characters for a TextInput or Textarea"]
    ["Condition"           "A Forthic predicate that indicates of a field should be hidden or shown"]
] REC;

: HEADERS                HEADER-INFO KEYS;
: HEADER-NOTE-CONTENTS   HEADER-INFO VALUES;

# ===== Adding headers and header notes
: A1                      [["startRowIndex" 0]  ["startColumnIndex" 0]] REC;
: content-CELL-DATA       [["note"    content @]] REC;
: HEADER-NOTES-ROW-DATA   [["values"  HEADER-NOTE-CONTENTS  "(content !) content-CELL-DATA" MAP]] REC;

: HEADER-NOTES-CHANGE   [
    ["range"    A1]
    ["rows"     [HEADER-NOTES-ROW-DATA]]
    ["fields"   "note"]
] REC;

: HEADER-NOTES-BATCH-UPDATE  [ ["updateCells"  HEADER-NOTES-CHANGE] ] REC;
: tab-ADD-HEADER             tab-TAB [HEADERS] gsheet.ROWS!;

# ===== Adding field type data validation
: FIELD-TYPES            ["Dropdown" "TextInput" "Textarea" "RadioCheckbox" "MultiCheckbox" "DateInput" "Attachment" "Markdown" "Html"];
: type-CONDITION-VALUE   [["userEnteredValue"   type @]] REC;
: FIELD-TYPE-COLUMN      HEADERS "Field Type" KEY-OF;

: FIELD-TYPE-RANGE   [
    ["startRowIndex"     1]
    ["startColumnIndex"  FIELD-TYPE-COLUMN]
    ["endColumnIndex"    FIELD-TYPE-COLUMN 1 +]
] REC;

: FIELD-TYPE-CONDITION   [
    ["type"    "ONE_OF_LIST"]
    ["values"  FIELD-TYPES "(type !) type-CONDITION-VALUE" MAP]
] REC;

: FIELD-TYPE-RULE   [
    ["condition"      FIELD-TYPE-CONDITION]
    ["showCustomUi"   TRUE]
] REC;

: FIELD-TYPE-DATA-VALIDATION-CHANGE   [
    ["range"   FIELD-TYPE-RANGE]
    ["rule"   FIELD-TYPE-RULE]
] REC;

: FIELD-TYPE-BATCH-UPDATE  [ ["setDataValidation"  FIELD-TYPE-DATA-VALIDATION-CHANGE] ] REC;

# ----- Is Required? validation
: IS-REQUIRED-COLUMN   HEADERS "Is Required?" KEY-OF;

: IS-REQUIRED-RANGE   [
    ["startRowIndex"     1]
    ["startColumnIndex"  IS-REQUIRED-COLUMN]
    ["endColumnIndex"    IS-REQUIRED-COLUMN 1 +]
] REC;

: IS-REQUIRED-CONDITION   [
    ["type"    "ONE_OF_LIST"]
    ["values"  ["Yes" "No" ""] "(type !) type-CONDITION-VALUE" MAP]
] REC;

: IS-REQUIRED-RULE   [["condition"  IS-REQUIRED-CONDITION]  ["showCustomUi"   TRUE]] REC;

: IS-REQUIRED-DATA-VALIDATION-CHANGE   [
    ["range"   IS-REQUIRED-RANGE]
    ["rule"   IS-REQUIRED-RULE]
] REC;

: IS-REQUIRED-BATCH-UPDATE  [["setDataValidation"  IS-REQUIRED-DATA-VALIDATION-CHANGE]] REC;


# ===== Style header
: HEADER-ROW-RANGE   [["startRowIndex"   0]  ["endRowIndex"   1]] REC;
: HEADER-BG-COLOR    [["red"  0.24]  ["green"  0.52]  ["blue"   0.38]] REC;
: HEADER-FG-COLOR    [["red"  1.0]  ["green"  1.0]  ["blue"   1.0]] REC;

: HEADER-TEXT-FORMAT   [
    ["foregroundColor"  HEADER-FG-COLOR]
    ["bold"             TRUE]
] REC;

: HEADER-CELL-STYLE   [
    ["userEnteredFormat" [
        ["backgroundColor"       HEADER-BG-COLOR]
        ["horizontalAlignment"   "CENTER"]
        ["textFormat"            HEADER-TEXT-FORMAT]
    ] REC]

] REC;

: HEADER-STYLE-CHANGE   [
    ["range"   HEADER-ROW-RANGE]
    ["cell"    HEADER-CELL-STYLE]
    ["fields"   "userEnteredFormat(backgroundColor,textFormat,horizontalAlignment)"]
] REC;

: HEADER-STYLE-BATCH-UPDATE  [["repeatCell"  HEADER-STYLE-CHANGE]] REC;
: COLUMN-RANGE               [["dimension"   "COLUMNS"]] REC;

: COLUMN-WIDTH-CHANGE   [
    ["range"       COLUMN-RANGE]
    ["properties"  [["pixelSize"   150]] REC]
    ["fields"      "pixelSize"]
] REC;

: COLUMN-WIDTH-BATCH-UPDATE   [ ["updateDimensionProperties"   COLUMN-WIDTH-CHANGE] ] REC;

: tab-STYLE-TAB   tab-TAB  [
    HEADER-NOTES-BATCH-UPDATE
    FIELD-TYPE-BATCH-UPDATE
    IS-REQUIRED-BATCH-UPDATE
    HEADER-STYLE-BATCH-UPDATE
    COLUMN-WIDTH-BATCH-UPDATE
] gsheet.BATCH-UPDATE-TAB!;

: tab-CREATE-IF-NEEDED         [[FALSE  "url-GSHEET tab @ gsheet.ENSURE-TAB! POP"]] REC tab-TAB >BOOL REC@ INTERPRET;
: tab-ADD-TEMPLATE-IF-NEEDED   [[FALSE  "tab-ADD-HEADER tab-STYLE-TAB"]] REC tab-TAB gsheet.ROWS >BOOL REC@ INTERPRET;
: tab-ENSURE-TAB               tab-CREATE-IF-NEEDED tab-ADD-TEMPLATE-IF-NEEDED;
: tab-FIELD-RECORDS            tab-TAB  HEADERS gsheet.!NULL-ON-ERROR gsheet.RECORDS []  DEFAULT;

: config-TAB                   config @ 'tab' REC@;
: config-STEP-TABS             config @ 'step_tabs' REC@;
: config-TABS                  [config-TAB config-STEP-TABS] FLATTEN;

: <ADD-FIELD-RECORDS           (config !@) config-TABS DUP "(tab ! tab-ENSURE-TAB) tab-FIELD-RECORDS" MAP ZIP REC "field_records" <REC!;
: config-FIELD-RECORDS         config @ 'field_records' REC@ VALUES FLATTEN;

: <NOTE-DUP-FIELDS             (config !@)  config-FIELD-RECORDS "Field ID" GROUP-BY-FIELD "LENGTH" MAP "1 >" SELECT "dup_fields" <REC!;
: admins-IS-ADMIN?              CURRENT-USER admins @ IN;

# NOTE: Run these whenever the forms need to be updated
# (url configs admins -- configs)
: REFRESH-CONFIGS!   (admins ! configs ! url ! google_context !) [
    [TRUE   "google_context @ gsheet.PUSH-CONTEXT!
             configs @ '<ADD-FIELD-RECORDS <NOTE-DUP-FIELDS' MAP 'intake__form_configs' cache.CACHE!"]
] REC admins-IS-ADMIN? REC@ INTERPRET;

: FORM-CONFIGS   'intake__form_configs' cache.CACHE@;

# ----- Support -----------------------------------------------------------------------------------------
: |CONDITION-LABELS     "' ' '-' REPLACE" MAP;
: |NON-NULL-VALUE       "'value' REC@ NULL !=" SELECT;
: |NON-NULL             "NULL !=" SELECT;

# ----- ticket_info Words ------------------------------------------------------------------------------------
["info" "fields" "field"] VARIABLES

: field-ID          field @ "Field ID" REC@;

# Add value to field record
: |ADD-VALUE        "(field !@) info @ ['valuesById'  field-ID] REC@ 'value' <REC!" MAP;

# This can be used to override any choices we make in the intake module
@: info-FIELDS-BY-JIRA-FIELD
    info @ ["formConfig" "field_records"] REC@ VALUES FLATTEN "Jira Field" GROUP-BY-FIELD "" <DEL "Attachment" <DEL
    "|ADD-VALUE |NON-NULL-VALUE" MAP
;
: field-HEADER   ["h4. " field @ "Field Label" REC@] CONCAT;
: field-VALUE     field @ "value" REC@;
: fields-FORMAT-DESCRIPTION   fields @ "(field !) [field-HEADER field-VALUE]" MAP FLATTEN /N JOIN;

: fields-JIRA-FIELD   fields @ 0 NTH "Jira Field" REC@;
: fields-IS-DESCRIPTION?   fields-JIRA-FIELD "Description" ==;
: fields-MULTI-VALUE   [
    [TRUE   "fields-FORMAT-DESCRIPTION"]
    [FALSE  '''fields @ "'value' REC@" MAP''']
] REC fields-IS-DESCRIPTION? REC@ INTERPRET;

: FIELDS>TICKET-VALUE   (fields !) [
    [0   "NULL"]
    [1   "fields @ 0 NTH 'value' REC@"]
    [2   "fields-MULTI-VALUE"]
] REC fields @ LENGTH [0 1 2] RANGE-INDEX REC@ INTERPRET;

["key" "value"] VARIABLES
: key/value-ARRAYIFY   [
    [TRUE   [value @] FLATTEN]
    [FALSE  value @]
] REC key @ ["Labels" "Component/s"] IN REC@;

: |ARRAYIFY-IF-NEEDED   "(value ! key !) key/value-ARRAYIFY" !WITH-KEY MAP;

: info-PROJECT       info @ ['formConfig'  'Project'] REC@;
: info-ISSUE-TYPE    info @ ['formConfig'  'Issue Type'] REC@;
: INFO>TICKET-RECORD   (info !)
    info-FIELDS-BY-JIRA-FIELD "FIELDS>TICKET-VALUE" MAP |ARRAYIFY-IF-NEEDED
    info-PROJECT "Project" <REC!
    info-ISSUE-TYPE "Issue Type" <REC!
;

: info-ATTACHMENT-FIELDS   info @ ["formConfig" "field_records"] REC@ VALUES FLATTEN "Field Type" GROUP-BY-FIELD "Attachment" REC@;
: INFO>ATTACHMENTS   (info !)
    info-ATTACHMENT-FIELDS "'Field ID' REC@" MAP "(key !) info @ ['valuesById' key @] REC@" MAP
    [] REC "UNION" REDUCE
;

["jira_field"] VARIABLES
: FORM-CONFIG-VALUE   info @ ['formConfig' jira_field @] REC@;
: APPEND-FORM-CONFIG-FIELD   (jira_field !)  [
        [TRUE   "DUP jira_field @ REC@ FORM-CONFIG-VALUE APPEND FLATTEN |NON-NULL jira_field @ <REC!"]
    ] REC FORM-CONFIG-VALUE NULL != REC@ INTERPRET
;

# ----- Public facing words ----------------------------------------------------------------------------------
[
    # ( google_context url configs admins -- )
    #   - Ensures that gsheet tabs exist for all configured forms
    #   - Loads information from each gsheet tab, constructs a record for each configured field, and stores to cache
    "REFRESH-CONFIGS!"

    # ( -- form-configs )
    #   - Returns array of cached form config records
    "FORM-CONFIGS"

    # The "INFO" record is constructed by the React intake module's CREATE-TICKET word:
    # It has the following fields
    #   formConfig: This is a record of the form config record the user specified in their main.forthic file
    #   fieldsById: This is a record mapping field ID to the field information in the gsheets
    #   valuesById: This is a record mapping field ID to the values a user has filled out in the form

    # ( info -- ticket_record )
    # This constructs a ticket record by aggregating values and extracting values by Jira field. It should
    # be suitable to use directly by jira.CREATE-TICKET
    "INFO>TICKET-RECORD"

    # ( info -- attachments_record )
    # This constructs a record mapping filename to attachment. It is suitable to use directly by jira.ADD-ATTACHMENTS
    "INFO>ATTACHMENTS"

    # ( ticket_record jira_field -- ticket_record )
    # This is a utility word that appends a field specified in a form config record to the appropriate field in a ticket record
    # It handles the case where there are existing values inputted by the user or if there are no values
    "APPEND-FORM-CONFIG-FIELD"
] EXPORT
"""


class IntakeModule(Module):
    """This implements code that supports the building of intake forms
    """

    def __init__(self, interp: IInterpreter):
        super().__init__('intake', interp, FORTHIC)
