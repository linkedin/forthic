import { useState, useEffect } from 'react';
import Spinner from 'react-bootstrap/Spinner'
import Button from 'react-bootstrap/Button';


// ForthicButton: Runs a Forthic string on click, showing busy until complete
export function ForthicButton(props) {
    const {
        interp, // Forthic interpreter
        forthic,  // Execute on click
        busy_message,
    } = props

    const [busy, setBusy] = useState(false)

    function run_forthic() {
        console.log("TODO: Run forthic", forthic)
        setBusy(true)
        interp.run(forthic).then(() => {
            console.log("interp stack", interp.stack)
            setBusy(false)
        }).catch((error) => {
            console.error(error)
            alert(error)
        })
    }

    let content
    if (busy) {
        content =
        <div className='d-flex flex-row align-items-center'>
            <Spinner variant="light" className="me-2"/>
            {busy_message ? busy_message : "Working..."}
        </div>
    }
    else {
        content = <Button {...props} onClick={() => run_forthic()}/>
    }

    return content
}
