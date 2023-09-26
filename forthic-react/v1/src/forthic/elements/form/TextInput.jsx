import Form from 'react-bootstrap/Form';

export function TextInput({
    field_record,       // See (TODO) for the fields
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    // field_id,           // Unique field identifier
    // field_label,        // Display label for field
    // field_description,  // Markdown description of field
    // jira_field,         // Jira field associated with this field
    // is_required,        // Is this field required
    // field_content,      // Default content for the textarea
    // max_input_length,   // Max num chars
    }) {

    // TODO: Indicate required
    // TODO: Handle max num chars
    // TODO: Add field description
    // TODO: Style control
    const field_id = field_record['Field ID']
    const defaultValue = valuesByFieldId[field_id] ? valuesByFieldId[field_id] : field_record['Field Content']
    return (
        <Form.Control
            type="text"
            placeholder={defaultValue}
            onChange={(event) => {update_state(field_id, event.target.value)}}
        />
    )
}
