from ..module import Module
from ..interfaces import IInterpreter

# NOTE: This requires the gsheet module to be used in the app module
FORTHIC = """
# ----- gsheet setup -----------------------------------------------------------------------------------------
["gsheet" "cache"] USE-MODULES

["url" "configs" "config" "tab" "admins" "content" "type"] VARIABLES

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
: FIELD-TYPES            ["Dropdown" "TextInput" "Textarea" "RadioCheckbox" "MultiCheckbox" "DateInput" "Attachment" "Markdown"];
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
: <ADD-FIELD-RECORDS           (config !@) (config-TAB tab ! tab-ENSURE-TAB)  tab-FIELD-RECORDS "field_records" <REC!;
: config-FIELD-RECORDS         config @ 'field_records' REC@;
: <NOTE-DUP-FIELDS             (config !@)  config-FIELD-RECORDS "Field ID" GROUP-BY-FIELD "LENGTH" MAP "1 >" SELECT "dup_fields" <REC!;
: admins-IS-ADMIN?              CURRENT-USER admins @ IN;

# NOTE: Run these whenever the forms need to be updated
# (url configs admins -- configs)
: REFRESH-CONFIGS!   (admins ! configs ! url !) [
    [TRUE   "contexts.GOOGLE gsheet.PUSH-CONTEXT!
             configs @ '<ADD-FIELD-RECORDS <NOTE-DUP-FIELDS' MAP 'intake__form_configs' cache.CACHE!"]
] REC admins-IS-ADMIN? REC@ INTERPRET;

: FORM-CONFIGS   'intake__form_configs' cache.CACHE@;

# ----- Support -----------------------------------------------------------------------------------------
["step_id" "step_fields" "transitions"] VARIABLES
: STEP   (transitions ! step_fields ! step_id !) [
    ["id"           step_id @]
    ["fields"       step_fields @]
    ["transitions"  transitions @]
] REC;

["fcondition" "next_step_id"] VARIABLES
: TRANSITION   (fcondition ! next_step_id !) [
    ["fcondition"    fcondition @]
    ["next_step_id"  next_step_id @]
] REC;

# ----- Public facing words ----------------------------------------------------------------------------------
[
    # ( url configs admins -- )
    #   - Ensures that gsheet tabs exist for all configured forms
    #   - Loads information from each gsheet tab, constructs a record for each configured field, and stores to cache
    "REFRESH-CONFIGS!"

    # ( -- form-configs )
    #   - Returns array of cached form config records
    "FORM-CONFIGS"

    # ( step_id step_fields transitions -- step-record )
    # This is a convenience word to wrap step information in a record:
    #   - `step_id` is the unique ID for the step
    #   - `step_fields` is an array of step field IDs corresponding to rows in a gsheet
    #   - `transitions` is an array of transitions created by the `TRANSITION` word (see below)
    "STEP"

    # (next_step_id fcondition -- transition )
    # This is a convenience word to wrap transition info in a record
    #   - `next_step_id` is the ID of the step to go to next if the condition is true
    #   - `fcondition` is a Forthic string that evaluates to `TRUE` if the `next_step_id` is enabled
    #
    # If multiple conditions are active, the first one (by order in array) is taken to be the next step
    "TRANSITION"
] EXPORT
"""


class IntakeModule(Module):
    """This implements code that supports the building of intake forms
    """

    def __init__(self, interp: IInterpreter):
        super().__init__('intake', interp, FORTHIC)
        self.add_module_word('AGGREGATE-JIRA-FIELDS', self.word_AGGREGATE_JIRA_FIELDS)

    # ( ticket_info jira_field -- value_by_field_id )
    #
    #    This returns a record that maps field ID to user-entered value for all fields that have the
    #    specified `jira_field`.
    #
    #    `ticket_info`: This is a record with the following fields:
    #        - formConfig: This is a form configuration record defined in the application. It has the following fields:
    #            - TODO
    #        - valuesById: This is a record mapping "Field ID" to user entered value
    #        - fieldsById: This is a record mapping "Field ID" to configured values from the gsheet
    def word_AGGREGATE_JIRA_FIELDS(self, interp: IInterpreter):
        jira_field = interp.stack_pop()
        ticket_info = interp.stack_pop()

        fieldsById = ticket_info["fieldsById"]
        valuesById = ticket_info["valuesById"]

        # Get field IDs with specified jira field
        field_ids = []
        for field_id, field_record in fieldsById.items():
            if field_record.get("Jira Field") == jira_field:
                field_ids.append(field_id)

        result = {}
        for f in field_ids:
            result[f] = valuesById.get(f)

        interp.stack_push(result)
