import Form from 'react-bootstrap/Form';

export function Dropdown({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    // TODO: Style control
    console.log("Dropdown content", field_record["Field Content"], field_record)

    const options_string = field_record["Field Content"] ? field_record["Field Content"] : ""
    const options = options_string.split("\n").map(o => <option>{o}</option>)
    const field_id = field_record['Field ID']
    const defaultValue = valuesByFieldId[field_id]
    return (
        <Form.Select className="w-50" onChange={(event) => {update_state(field_id, event.target.value)}} defaultValue={defaultValue}>
            <option></option>
            {options}
        </Form.Select>
    )
}
