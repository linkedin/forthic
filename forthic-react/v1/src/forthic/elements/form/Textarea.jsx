import Form from 'react-bootstrap/Form';
import {useEffect, useState, useRef} from 'react'


export function Textarea({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    // Handle max num chars
    const max_length = field_record['Max Input Length']
    const chars_per_line = 80
    const num_rows = max_length ? Math.round(max_length / chars_per_line) + 1 : 5

    // NOTE: This logic is needed because textarea nodes seem to be reused and the states aren't cleared
    const field_id = field_record['Field ID']
    const element = useRef()
    const default_value = field_record['Field Content'] ? field_record['Field Content'] : ""
    const field_value = valuesByFieldId[field_id]

    useEffect(() => {
        if (field_value && field_value === element.current.value)   return

        if (!field_value) {
            element.current.value = default_value
        }
        else {
            element.current.value = field_value
        }
    })

    function update(value) {
        let new_value = value
        if (value && max_length && value.length > max_length) {
            new_value = value.substring(0, max_length)
        }
        update_state(field_id, new_value)
    }

    return (
        <Form.Control
            as="textarea"
            rows={num_rows}
            ref={element}
            onChange={(event) => {update(event.target.value)}}
        />
    )
}
