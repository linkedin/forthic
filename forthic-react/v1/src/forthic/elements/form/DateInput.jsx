import Form from 'react-bootstrap/Form';

export function DateInput({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    const field_id = field_record['Field ID']
    const defaultValue = valuesByFieldId[field_id]
    return (
        <Form.Control
            className="w-50"
            type="date"
            defaultValue={defaultValue}
            onChange={(event) => {update_state(field_id, event.target.value)}}
        />
    )
}
