import React, { useEffect, useState } from "react"
import { useParams } from "react-router-dom"
import Button from 'react-bootstrap/Button';
import Form from 'react-bootstrap/Form';
import { TextInput } from "./TextInput";
import { Textarea } from "./Textarea";
import { DateInput } from "./DateInput";
import { Dropdown } from "./Dropdown";
import { Markdown } from "./Markdown";
import { Attachment } from "./Attachment";
import { RadioCheckbox } from "./RadioCheckbox";
import { MultiCheckbox } from "./MultiCheckbox";
import Spinner from 'react-bootstrap/Spinner'
import ReactMarkdown from 'react-markdown'

export function ConfigurableForm({interp, form_configs, form_id_field, fcreated_message, className}) {
    // Props
    // - interp:   Forthic interpreter
    // - form_configs:   An array of form config records with fields
    //    - tab:                The id of the form
    //    - Project:            The Jira project to create tickets for
    //    - Issue Type:         The type of ticket to create for this form
    //    - Labels:             Array of Jira labels to apply to each ticket
    //    - Component/s:        Array of Jira components to apply to each ticket
    //    - steps (optional):   An array of Step records, each with the following fields
    //        - id:             The id of the step
    //        - fields:         An array of field IDs that are part of the step
    //        - transitions:    An array of transitions, each with the following fields
    //            - fcondition:     Forthic code to decide if a transition should take place
    //            - next_step_id:   The step to transition to if the `fcondition` evaluates to TRUE
    // - form_id_field:       Right now, we rely on a dynamic segment of a route to select a form. This prop
    //                        gives us the name of that segment
    // - fcreated_message:    A Forthic string that expects a ticket key and returns a ticket created message
    const [formConfig, setFormConfig] = useState(null)
    const [fieldsById, setFieldsById] = useState({})
    const [curSteps, setCurSteps] = useState([])
    const [isWorking, setIsWorking] = useState(false)
    const [valuesByFieldId, setValuesByFieldId] = useState({})
    const [nextEnabledStepId, setNextEnabledStepId] = useState(null)
    const [createdMessage, setCreatedMessage] = useState(null)

    const params = useParams();
    const form_id = params[form_id_field]

    // ----- Set up formConfig -------------------------------------------------------------------------------
    useEffect(() => {
        if (!form_configs || !form_configs.length) return

        setTimeout(async () => {
            const form_config = select_form_config(form_id, form_configs)

            // If the form has no steps configured, create a single step that has all fields
            if (!form_config.steps) {
                // NOTE: This must match how steps are defined in the intake_module
                form_config.steps = [
                    {
                        id: "single_step",
                        fields: form_config.field_records.map(rec => rec["Field ID"])
                    }
                ]
            }

            const field_records_by_id = {}
            form_config.field_records.forEach(rec => {
                field_records_by_id[rec["Field ID"]] = rec
            })

            setFormConfig(form_config)
            setCurSteps([form_config.steps[0]])
            setFieldsById(field_records_by_id)
            await interp.run("['intake'] USE-MODULES")
        })
    }, [])

    // ----- Handle initialization ---------------------------------------------------------------------------
    if (!form_configs || !form_configs.length) {
        return <p>ConfigurableForm: No configured forms</p>
    }

    if (!formConfig) {
        return <p>Working...</p>
    }

    // Field control types
    const fieldType_to_class = {
        TextInput,
        Textarea,
        DateInput,
        Dropdown,
        Markdown,
        Attachment,
        RadioCheckbox,
        MultiCheckbox,
    }

    // TODO: Handle case where there is no step corresponding to nextEnabledStepId
    //
    // `update_state` is the callback used for all form controls. When any values change, we store them
    // in the valuesByFieldId record and we recompute any state transition conditions for the current step
    // and store these in nextEnabledStep
    async function update_state(field_id, value) {
        // Update valuesByFieldId
        let new_values_by_field_id = {...valuesByFieldId}
        new_values_by_field_id[field_id] = value
        setValuesByFieldId(new_values_by_field_id)

        console.log("New values", new_values_by_field_id)

        const form = {
            formConfig, fieldsById, curSteps, valuesByFieldId: new_values_by_field_id
        }

        // Check step transition conditions
        const cur_step = curSteps[curSteps.length - 1]
        const step_transitions = cur_step.transitions
        if (step_transitions && step_transitions.length > 0) {
            for (let i=0; i < step_transitions.length; i++) {
                const transition = step_transitions[i]

                // Set !FORM context
                interp.stack_push(form)
                await interp.run("intake.!FORM")

                // Evaluate step transition condition
                await interp.run(transition.fcondition)
                const condition_value = interp.stack_pop()
                if (condition_value) {
                    setNextEnabledStepId(transition.next_step_id)
                    break
                }
            }
        }
    }

    function make_input_field(field_record) {
        const field_type = field_record['Field Type']
        const field_class = fieldType_to_class[field_type]

        // Indicate required field
        let is_required_note;
        if (field_record["Is Required?"] === "Yes") {
            is_required_note = (
                <span className="text-danger">*</span>
            )
        }

        // Add field description
        let field_description;
        if (field_record["Field Description"] !== "") {
            field_description = (
                <div className="field-description">
                    <ReactMarkdown>{field_record["Field Description"]}</ReactMarkdown>
                </div>
            )
        }

        if (field_class) {
            const field_control = React.createElement(field_class, {field_record, update_state, valuesByFieldId, interp})
            return (
                <Form.Group>
                    <Form.Label className="mt-3 mb-0 fs-5">{field_record['Field Label']} {is_required_note}</Form.Label>
                    {field_description}
                    {field_control}
                </Form.Group>
            )
        }
        // TODO: Allow extension point here
        else {
            return <p className="text-danger mt-4">Unknown field type: {field_record['Field Type']}</p>
        }
    }

    function make_input_fields(field_records) {
        return field_records.map(fr => make_input_field(fr))
    }


    // ----- Prev/Next/Submit buttons ------------------------------------------------------------------------
    const cur_step = curSteps[curSteps.length - 1]
    console.log("=====> ", cur_step)
    const cur_field_records = cur_step.fields.map(field_id => fieldsById[field_id])
    const step_transitions = cur_step.transitions
    console.log("Step Transitions", step_transitions)

    let prev_button, next_button, submit_button

    function all_required_fields_have_values(field_records) {
        for (let i=0; i < field_records.length; i++) {
            const record = field_records[i]
            if (record["Is Required?"] !== 'Yes')       continue
            if (!valuesByFieldId[record["Field ID"]])   return false
        }
        return true
    }

    function is_next_enabled(field_records) {
        const result = !!nextEnabledStepId && all_required_fields_have_values(field_records)
        // const result = !!nextEnabledStepId
        return result
    }

    function is_submit_enabled(field_records, transitions) {
        const have_transitions = !!transitions && transitions.length > 0
        console.log("have transitions", have_transitions)
        console.log("nextEnabledStepId", nextEnabledStepId)
        console.log("all required done?", all_required_fields_have_values(field_records))
        const result = all_required_fields_have_values(field_records) && !have_transitions
        return result
    }

    function find_step(step_id) {
        const steps = formConfig.steps
        if (!steps)   throw "Can't find a step because they're aren't any!"

        for (let i=0; i < steps.length; i++) {
            const step = steps[i]
            if (step.id === step_id)   return step
        }
        throw `Couldn't find step: ${step_id}`
    }

    function transition_to_next_step() {
        const step = find_step(nextEnabledStepId)
        setCurSteps([...curSteps, step]);
        setTimeout(() => {
            window.scrollTo({ top: 0, left: 0})
        })
    }

    function transition_to_previous_step() {
        setCurSteps(curSteps.slice(0, -1));
        setTimeout(() => {
            window.scrollTo({ top: 0, left: 0})
        })
    }

    if (curSteps.length > 1) {
        prev_button = (
            <Button
                onClick={() => transition_to_previous_step()}
            >Prev</Button>
        )
    }

    if (step_transitions && step_transitions.length > 0) {
        next_button = (
            <Button
                disabled={!is_next_enabled(cur_field_records)}
                onClick={() => transition_to_next_step()}
            >Next</Button>
        )
    }

    if (!step_transitions || step_transitions.length == 0) {
        if (isWorking) {
            submit_button =
                <div className='d-flex flex-row align-items-center'>
                    <Spinner variant="primary" className="me-2"/>
                    {"Working..."}
                </div>
        }
        else {
            submit_button = (
                <Button
                    disabled={!is_submit_enabled(cur_field_records, step_transitions)}
                    onClick={() => create_ticket()}
                >Submit</Button>
            )
        }
    }

    // ----- Create ticket -----------------------------------------------------------------------------------
    async function create_ticket() {
        setIsWorking(true)
        interp.stack_push(formConfig)
        interp.stack_push(fieldsById)
        interp.stack_push(valuesByFieldId)

        interp.run("intake.CREATE-TICKET").then(async () => {
            setIsWorking(false)
            const ticket_key = interp.stack_pop()
            interp.stack_push(ticket_key)
            await interp.run(fcreated_message)
            const message = interp.stack_pop()
            setCreatedMessage(message)
        }).catch((error) => {
            setIsWorking(false)
            console.error(error)
            alert(error)
        })
    }


    // ----- UI ----------------------------------------------------------------------------------------------
    let content
    if (createdMessage) {
        content =
        <div>
            <ReactMarkdown className="p-4">{createdMessage}</ReactMarkdown>
        </div>
    }
    else {
        content =
        <div className={className}>
            <span></span>
            <Form>
                {make_input_fields(cur_field_records)}
            </Form>
            <div className="d-flex flex-row justify-content-between mt-4">
                {prev_button}
                <span></span>
                {next_button}
                {submit_button}
            </div>
        </div>
    }

    return content
}

function select_form_config(form_id, form_configs) {
    if (!form_configs || form_configs.length == 0)   return null;

    let result = form_configs[0]
    for (let i=0; i < form_configs.length; i++) {
        if (form_configs[i].tab == form_id) {
            result = form_configs[i]
            break
        }
    }
    return result
}