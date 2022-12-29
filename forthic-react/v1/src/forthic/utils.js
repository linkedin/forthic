export function ensure_array(value) {
    if (!value)   return []

    if (value instanceof Array)   return value;
    else                          return [value];
}

export function render_content_array(content_array) {
    let result = []
    for (const c of content_array) {
        if (typeof c === 'function') {
            result.push(c())
        }
        else {
            result.push(c)
        }
    }
    return result
}

// Ensures search params have values
// We assume that ` search_params` and `set_ search_params` come from something like
//     import { useSearchParams } from 'react-router-dom'
export function ensure_search_params(default_params,  search_params, set_search_params) {
    let any_value_changed = false

    for (const field in default_params) {
        const orig_value =  search_params.get(field)
        let value = orig_value
        if (!value) {
            value = default_params[field]
             search_params.set(field, value)
            any_value_changed = true
        }
    }
    if (any_value_changed)   set_search_params( search_params)
    return  search_params
}