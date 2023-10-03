import React, { useEffect, useState } from "react"

import Form from 'react-bootstrap/Form';

export function RadioCheckbox({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    const field_id = field_record['Field ID']
    const [value, setValue] = useState(valuesByFieldId[field_id])

    function update(option) {
        setValue(option)
        update_state(field_id, option)
    }

    const options_string = field_record["Field Content"] ? field_record["Field Content"] : ""
    const checkboxes = options_string.split("\n").map(o => (
        <Form.Check
            type="radio"
            inline
            label={o}
            checked={o === value}
            id={`${field_id}-${o}`}
            onChange={() => update(o)}
        />
    ))

    return (
        <div key={field_id}>
            {checkboxes}
        </div>
    )
}
