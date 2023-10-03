import Form from 'react-bootstrap/Form';


export function Attachment({
    field_record,       // See [HEADER-INFO](../../../../../../forthic/v3/modules/intake_module.py)
    update_state,       // A function taking a (field_id, value) and setting the data in the form
    valuesByFieldId,    // Field values
    interp,             // Forthic interpreter
    }) {

    async function read_file(file) {
        if(file.size > 2097152){
            alert("Attachment is too large. Please select files that are less than 2MB large");
            return
         };
        return new Promise((resolve, reject) => {
            const reader = new FileReader();
            reader.onloadend = (event) => {
                resolve(event.target.result)
            }
            reader.onerror = reject;
            reader.readAsDataURL(file);
      })
    }

    async function note_file_content(event) {
        const files = event.target.files
        let attachments = {}
        for (const file of files) {
            const contents = await read_file(file)
            attachments[file.name] = contents
        }
        update_state(field_record['Field ID'], attachments)
        console.log("=====> Attachments", attachments)
    }

    return (
        <Form.Control className="w-50" type="file" multiple onChange={event => note_file_content(event)} />
    )
}
