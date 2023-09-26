import Form from 'react-bootstrap/Form';
import {useEffect, useState} from 'react'

export function Textarea({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    // TODO: Handle max num chars
    const field_id = field_record['Field ID']
    const [defaultValue, setDefaultValue] = useState("")

    // NOTE: All of this logic is needed because the text areas seem to be reused rather than reconstructed
    //       and when you move from step to step, the default values are not properly applied.
    useEffect(() => {
        if (valuesByFieldId[field_id] === defaultValue)   return

        if (valuesByFieldId[field_id]) {
            setDefaultValue(valuesByFieldId[field_id])
        }
        else if (field_record['Field Content']) {
            setDefaultValue(field_record['Field Content'])
        }
        else {
            setDefaultValue("")
        }
    })

    return (
        <Form.Control
            as="textarea"
            rows={5}
            value={defaultValue}
            onChange={(event) => {update_state(field_id, event.target.value)}}
        />
    )
}
