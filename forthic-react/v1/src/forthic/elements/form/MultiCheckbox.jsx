import React, { useEffect, useState } from "react"

import Form from 'react-bootstrap/Form';

export function MultiCheckbox({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    const field_id = field_record['Field ID']
    const checked_options = valuesByFieldId[field_id] ? valuesByFieldId[field_id]: []
    const [value, setValue] = useState(checked_options)

    function update(option, event) {
        let new_value
        if (event.target.checked) {
            new_value = [...value, option]
        }
        else {
            new_value = value.filter(o => o !== option)
        }
        setValue(new_value)
        update_state(field_id, new_value)
    }

    const options_string = field_record["Field Content"] ? field_record["Field Content"] : ""
    const checkboxes = options_string.split("\n").map(o => (
        <Form.Check
            type="checkbox"
            inline
            label={o}
            checked={checked_options.includes(o)}
            id={`${field_id}-${o}`}
            onChange={(event) => update(o, event)}
        />
    ))

    return (
        <div key={field_id}>
            {checkboxes}
        </div>
    )
}
