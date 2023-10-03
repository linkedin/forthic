import Form from 'react-bootstrap/Form';
import {useEffect, useState, useRef} from 'react'

export function TextInput({
    field_record,       // See (TODO) for the fields
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    // Handle max num chars
    const max_length = field_record['Max Input Length']

    const field_id = field_record['Field ID']
    const default_value = valuesByFieldId[field_id] ? valuesByFieldId[field_id] : field_record['Field Content']
    const field_value = valuesByFieldId[field_id]

    const element = useRef()

    useEffect(() => {
        if (field_value && field_value === element.current.value)   return

        if (!field_value) {
            element.current.value = ""
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
            type="text"
            ref={element}
            placeholder={default_value}
            onChange={(event) => {update(event.target.value)}}
        />
    )
}
