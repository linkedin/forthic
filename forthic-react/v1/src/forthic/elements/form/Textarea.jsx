import Form from 'react-bootstrap/Form';

export function Textarea({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    // TODO: Handle max num chars
    const field_id = field_record['Field ID']
    const defaultValue = valuesByFieldId[field_id] ? valuesByFieldId[field_id] : field_record['Field Content']
    return (
        <Form.Control
            as="textarea"
            rows={5}
            defaultValue={defaultValue}
            onChange={(event) => {update_state(field_id, event.target.value)}}
        />
    )
}
