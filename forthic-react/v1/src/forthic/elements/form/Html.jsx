import RawHTML from "../RawHTML"

export function Html({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    }) {

    return (
        <RawHTML html={field_record["Field Content"]}/>
    )
}
