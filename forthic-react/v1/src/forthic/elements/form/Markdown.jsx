import ReactMarkdown from 'react-markdown'

export function Markdown({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    }) {

    return (
        <ReactMarkdown>
            {field_record["Field Content"]}
        </ReactMarkdown>
    )
}
