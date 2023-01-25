import React, { useState, useEffect } from "react"
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import Form from 'react-bootstrap/Form';
import RecordsTable from "./RecordsTable"
import Badge from 'react-bootstrap/Badge';
import { CSVLink } from "react-csv"
// import { get_app_state } from "./forthic/modules/react_module";

function arrayEquals(a, b) {
    return Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every((val, index) => val === b[index]);
}

const ModalActivities = {
    ViewTickets: 0,
    EmailCampaign: 1
}

// ----- ViewTicketsActivity ---------------------------------------------------------------------------------
function ViewTicketsActivity(props) {
    // Props:
    //    records_table_props: same props as RecordsTable
    //    set_modal_activity: Function to change activity for TicketsModal
    //    email_configured:  true if email has been configured
    let records_table = React.createElement(RecordsTable, props.records_table_props)

    function get_email_campaign_button() {
        if (!props.email_configured)   return
        return (
            <Button variant="primary" onClick={() => props.set_modal_activity(ModalActivities.EmailCampaign)}>
                Email Campaign
            </Button>
        )
    }

    function get_csv_link() {
        return <CSVLink data={props.records_table_props.records} filename="ticket-details.csv">Download CSV</CSVLink>
    }

    return (
        <>
            {get_csv_link()}
            {records_table}
            {get_email_campaign_button()}
        </>
    )
}

// ----- EmailCampaignActivity -------------------------------------------------------------------------------
const EmailCampaignSteps = {
    RecipientsStep: 0,
    DraftStep: 1,
    SendStep: 2
}

function RecipientsStep(props) {
    // Props:
    //    records: Ticket records
    //    recipient_info: Default values for recipientInfo
    //    set_recipient_info: Function to store results of this step
    //    set_step: Function to set the email campaign activity step
    const [recipientType, setRecipientType] = useState(props.recipient_info.recipientType);
    const [ccType, setCcType] = useState(props.recipient_info.ccType);
    const [ccWritein, setCcWritein] = useState(props.recipient_info.ccWritein);

    // Recipient Info state
    const recipientInfo = {
        recipientType: recipientType,
        ccType: ccType,
        ccWritein: ccWritein
    }

    function handle_next() {
        props.set_recipient_info(recipientInfo);
        props.set_step(EmailCampaignSteps.DraftStep)
    }

    function is_recipient_type_selected(value) {
        if (!props.recipient_info || !props.recipient_info.recipientType) return false
        return value == props.recipient_info.recipientType
    }

    function is_cc_type_checked(value) {
        if (!ccType)   return false
        return ccType[value]
    }

    function is_next_disabled() {
        return !recipientType
    }

    return (
        <>
            <h2>Step 1: Configure recipients</h2>
            <Form>
                <p>({props.records.length} tickets selected)</p>
                <div className="d-flex flex-row">
                    <div>
                        <Form.Group className="mb-3 me-5" controlId="formRecipients">
                            <h4>Recipient</h4>
                            <Form.Select onChange={(e) => setRecipientType(e.target.value)}>
                                <option value="">Select recipient type</option>
                                <option value="assignee" selected={is_recipient_type_selected("assignee")}>Assignee</option>
                                <option value="manager" selected={is_recipient_type_selected("manager")}>Manager</option>
                                <option value="manager_manager" selected={is_recipient_type_selected("manager_manager")}>Manager's Manager</option>
                            </Form.Select>
                        </Form.Group>
                    </div>

                    <div className="flex-grow-1">
                        <h4>CC</h4>
                        <div className="d-flex flex-row">
                            <Form.Group className="mt-1">
                                <Form.Check
                                    inline
                                    label="Assignee"
                                    onChange={(e) => setCcType({ ...ccType, assignee: e.target.checked })}
                                    name="cc_group"
                                    type="checkbox"
                                    checked={is_cc_type_checked("assignee")}
                                />
                                <Form.Check
                                    inline
                                    label="Manager"
                                    onChange={(e) => setCcType({ ...ccType, manager: e.target.checked })}
                                    name="cc_group"
                                    type="checkbox"
                                    checked={is_cc_type_checked("manager")}
                                />
                                <Form.Check
                                    inline
                                    label="Manager's Manager"
                                    onChange={(e) => setCcType({ ...ccType, manager_manager: e.target.checked })}
                                    name="cc_group"
                                    type="checkbox"
                                    checked={is_cc_type_checked("manager_manager")}
                                />
                            </Form.Group>

                            <Form.Group className="flex-grow-1">
                                <Form.Control
                                    className=""
                                    defaultValue={props.recipient_info.ccWritein}
                                    placeholder="Comma-separated usernames or emails"
                                    onChange={(e) => setCcWritein(e.target.value)}
                                />
                            </Form.Group>
                        </div>

                    </div>
                </div>
            </Form>

            <Button
                className="mt-4"
                variant="primary"
                onClick={() => handle_next()}
                disabled={is_next_disabled()}
            >Next: Draft email</Button>
        </>
    )
}

function DraftStep(props) {
    // Props:
    //    records: Ticket records
    //    recipient_info: Default values for recipientInfo
    //    recipients: List of usernames
    //    draft_info:  Default value for draftInfo
    //    set_draft_info:  Default value for draftInfo
    //    set_step: Function to set the email campaign activity step
    //    frender_user_email: Forthic expecting (recipient_info tickets username) and returns rendered email
    //    interp: Forthic interpreter
    const [subject, setSubject] = useState(props.draft_info.subject);
    const [body, setBody] = useState(props.draft_info.body);
    const [emailPreview, setEmailPreview] = useState(null);

    // Draft Info state
    const draftInfo = {
        subject: subject,
        body: body
    }

    function handle_next() {
        props.set_draft_info(draftInfo)
        props.set_step(EmailCampaignSteps.SendStep)
    }

    function handle_back() {
        props.set_draft_info(draftInfo)
        props.set_step(EmailCampaignSteps.RecipientsStep)
    }


    function get_recipient_option(username) {
        return (<option>{username}</option>)
    }

    async function render_preview(username) {
        if (!username) {
            setEmailPreview("")
            return
        }

        props.interp.stack_push(props.recipient_info)
        props.interp.stack_push(draftInfo)
        props.interp.stack_push(props.records)
        props.interp.stack_push(username)
        await props.interp.run(props.frender_user_email)
        let result = props.interp.stack_pop()
        setEmailPreview(result)
    }

    function get_preview() {
        if (!emailPreview) return

        let rendered_email = {__html: emailPreview}
        return (<div
            className="mt-2 bg-light p-3"
            dangerouslySetInnerHTML={rendered_email}
        />)

    }

    function is_next_disabled() {
        return (!subject || !body)
    }

    return (
        <>
            <h2>Step 2: Draft email</h2>
            <div className="d-flex">
                <div className="w-50 me-3">
                    <h3>Draft</h3>
                    <Form>
                        <Form.Group>
                            <Form.Label>Subject</Form.Label>
                            <Form.Control
                                placeholder="Email subject"
                                onChange={(e) => setSubject(e.target.value)}
                                defaultValue={props.draft_info.subject}
                            />
                        </Form.Group>

                        <Form.Group>
                            <Form.Label>Email Body</Form.Label>

                            <Form.Control
                                placeholder="Markdown with {{FIRST_NAME}} and {{TICKETS_TABLE}} macros"
                                as="textarea"
                                rows={20}
                                onChange={(e) => setBody(e.target.value)}
                                defaultValue={props.draft_info.body}
                            />
                        </Form.Group>
                    </Form>
                </div>

                <div className="w-50 border-start ps-3">
                    <h3>Preview</h3>
                    <Form>
                        <Form.Group className="mb-3" controlId="formRecipients">
                            <Form.Label>Recipient Preview</Form.Label>
                            <Form.Select onChange={(e) => render_preview(e.target.value)}>
                                <option value="">Select recipient</option>
                                {props.recipients.map((r) => get_recipient_option(r))}
                            </Form.Select>
                        </Form.Group>
                    </Form>

                    {get_preview()}
                </div>
            </div>
            <div className="mt-4 d-flex flex-row">
                <Button className="me-2" variant="secondary" onClick={() => handle_back()}>Back: Recipient setup</Button>
                <Button variant="primary" disabled={is_next_disabled()} onClick={() => handle_next()}>Next: Send emails</Button>
            </div>
        </>
    )
}

function EmailStep(props) {
    // Props:
    //    records: Ticket records
    //    recipient_info: Default values for recipientInfo
    //    draft_info:  Default value for draftInfo
    //    set_step: Function to set the email campaign activity step
    //    fgenerate_emails: Forthic expecting (recipient_info draft_info tickets) and returns rendered emails
    //    interp: Forthic interpreter
    const [emails, setEmails] = useState([])
    const [testMode, setTestMode] = useState(true)
    const [busyGeneratingEmails, setBusyGeneratingEmails] = useState(false)
    const [busySendingEmails, setBusySendingEmails] = useState(false)
    const [emailsSent, setEmailsSent] = useState(false)
    const [emailErrors, setEmailErrors] = useState([])
    const [emailBatches, setEmailBatches] = useState([])
    const [cancelSend, setCancelSend] = useState(false)

    useEffect(() => {
        setTimeout(async () => {
            // If busy, return
            if (busyGeneratingEmails)   return

            // If no records to render, return
            if (props.records.length == 0)   return

            // If we have finished generating emails, return
            if (emails.length > 0)   return

            // Generate emails
            setBusyGeneratingEmails(true)
            props.interp.stack_push(props.recipient_info)
            props.interp.stack_push(props.draft_info)
            props.interp.stack_push(props.records)
            await props.interp.run(props.fgenerate_emails)
            let _emails = props.interp.stack_pop()
            setEmails(_emails)
            setBusyGeneratingEmails(false)
        }, 0)
    })

    useEffect(() => {
        setTimeout(async() => {
            if (emailBatches.length == 0 || cancelSend) {
                setEmailsSent(true)
                setBusySendingEmails(false)
                return
            }
            let batch = emailBatches.pop()
            props.interp.stack_push(testMode)
            props.interp.stack_push(batch)
            await props.interp.run(props.fsend_emails)
            let email_results = props.interp.stack_pop()
            setEmailErrors(oldErrors => [...oldErrors, ...email_results])
            setEmailBatches([...emailBatches])
        })
    }, [emailBatches, cancelSend])

    function get_busy_message() {
        function make_message(message) {
            return <p><div className="spinner-border text-primary" role="status"></div> {message}</p>
        }

        if (busyGeneratingEmails) {
            return make_message("Rendering emails...")
        }
        if (busySendingEmails) {
            return make_message("Sending emails...")
        }
    }

    function get_content() {
        if (!emails || busyGeneratingEmails) return

        function add_status(emails) {
            let result = []
            for (let i=0; i < emails.length; i++) {
                let email_with_status = {...emails[i]}
                result.push(email_with_status)

                if (i >= emailErrors.length)   continue
                if (emailErrors[i]) {
                    email_with_status.Status = <Badge pill bg="danger">Error</Badge>
                }
                else {
                    email_with_status.Status = <Badge pill bg="success">Sent</Badge>
                }
            }
            return result
        }

        let column_info = [
            {field: "recipient"},
            {field: "cc"},
            {field: "tickets"},
            {field: "Status"},
        ]
        let table_props = {
            records: add_status(emails),
            column_info: column_info,
            pagination_info: {page_size: 10}
        }
        let records_table = React.createElement(RecordsTable, table_props)

        function handle_back() {
            props.set_draft_info(props.draft_info)
            props.set_step(EmailCampaignSteps.DraftStep)
        }

        function handle_done() {
            props.handle_close()
        }

        async function send_emails() {
            setEmailsSent(false)
            setBusySendingEmails(true)
            setEmailErrors([])

            const batch_size = 5;
            props.interp.stack_push(emails)
            props.interp.stack_push(batch_size)
            await props.interp.run("GROUPS-OF")
            let email_batches = props.interp.stack_pop()
            email_batches.reverse()
            setEmailBatches(email_batches)
        }

        function handle_test_mode_change(checked) {
            setEmailsSent(false)
            setTestMode(checked)
        }

        function get_send_button() {
            if (busySendingEmails)  return
            return (
                <Button variant="outline-primary" onClick={() => send_emails()}>Send Emails!</Button>
            )
        }

        function get_cancel_send_button() {
            if (!busySendingEmails)  return
            return (
                <Button variant="danger" onClick={() => setCancelSend(true)}>Cancel Send</Button>
            )
        }

        function is_done_disabled() {
            return !emailsSent || testMode
        }

        function get_send_result() {
            if (busySendingEmails || emailsSent) {
                return (
                    <div>
                        <h4>Email Results</h4>
                        <p>{emailErrors.length} emails processed.</p>
                    </div>
                )
            }
        }

        return (
            <>
                <h2>Step 3: Send emails</h2>
                <div className="d-flex flex-column">
                    <div>{records_table}</div>
                    <div>{get_send_result()}</div>
                    <Form.Group className="mb-3">
                        <Form.Check
                            inline
                            label="Test mode? (Sends all emails only to you)"
                            onChange={(e) => handle_test_mode_change(e.target.checked)}
                            name="test_mode"
                            type="checkbox"
                            checked={testMode}
                        />
                        {get_send_button()}
                        {get_cancel_send_button()}
                    </Form.Group>
                </div>
                <div className="mt-4 d-flex flex-row">
                    <Button className="me-2" variant="secondary" onClick={() => handle_back()}>Back: Draft email</Button>
                    <Button variant="primary" disabled={is_done_disabled()} onClick={() => handle_done()}>Done</Button>
                </div>
            </>
        )
    }

    return (
        <>
        {get_busy_message()}
        {get_content()}
        </>
    )
}

function EmailCampaignActivity(props) {
    // Props:
    //    logged_in_username: Current username
    //    records: same props as RecordsTable
    //    set_modal_activity: Function to change activity for TicketsModal
    //    interp: Forthic interpreter
    //    frecipients: Forthic expecting (recipient_info tickets) and returns a list of recipient usernames
    //    frender_user_email:  Forthic expecting (recipient_info tickets username) and returns rendered email
    //    fgenerate_emails:  Forthic expecting (recipient_info draft_info tickets) and returns rendered emails
    //    fsend_emails:  Forthic expecting test mode flag and email records and returns an array of send errors
    //    handle_close: Closes TicketModal

    const [step, setStep] = useState(EmailCampaignSteps.RecipientsStep);
    const [recipientInfo, setRecipientInfo] = useState({"ccWritein": props.logged_in_username})
    const [draftInfo, setDraftInfo] = useState({})
    const [content, setContent] = useState({})

    useEffect(() => {
        setTimeout(async () => {
            let _content = {};
            switch (step) {
                case EmailCampaignSteps.RecipientsStep:
                    _content = {
                        step: "RecipientsStep",
                        stepContent: <RecipientsStep
                            records={props.records}
                            set_step={(s) => setStep(s)}
                            recipient_info={recipientInfo}
                            set_recipient_info={(info) => setRecipientInfo(info)}
                        />
                    }
                    break

                case EmailCampaignSteps.DraftStep:
                    props.interp.stack_push(recipientInfo)
                    props.interp.stack_push(props.records)
                    await props.interp.run(props.frecipients)
                    const recipients = props.interp.stack_pop()
                    _content = {
                        step: "DraftStep",
                        stepContent: <DraftStep
                            records={props.records}
                            set_step={(s) => setStep(s)}
                            recipient_info={recipientInfo}
                            recipients={recipients}
                            draft_info={draftInfo}
                            set_draft_info={(info) => setDraftInfo(info)}
                            frender_user_email={props.frender_user_email}
                            interp={props.interp}
                        />
                    }
                    break

                case EmailCampaignSteps.SendStep:
                    _content = {
                        step: "EmailStep",
                        stepContent: <EmailStep
                            records={props.records}
                            set_step={(s) => setStep(s)}
                            recipient_info={recipientInfo}
                            set_draft_info={(info) => setDraftInfo(info)}
                            draft_info={draftInfo}
                            handle_close={props.close_modal}
                            fgenerate_emails={props.fgenerate_emails}
                            fsend_emails={props.fsend_emails}
                            interp={props.interp}
                        />
                    }
                    break

            }
            if (_content.step != content.step)   setContent(_content)
        }, 0)
    })

    return content.stepContent
}

// ----- TicketsModal ----------------------------------------------------------------------------------------
function TicketsModal(props) {
    // Props:
    //
    //    message_broker: a MessageBroker the TicketsModal subscribes to for state change messages
    //    records_field: The name of the tickets records in messages from the message_broker
    //    column_infos: List of all column infos that could be rendered. Only those in the specified records will be shown
    //    records_table_props: same props as RecordsTable, except that records is ignored
    //    logged_in_username (optional): Username for current user
    //    frecipients (optional): Forthic expecting (recipient_info tickets) and returns a list of recipient usernames
    //    frender_user_email (optional):  Forthic expecting (recipient_info draft_info tickets username) and returns rendered email
    //    fgenerate_emails (optional):  Forthic expecting (recipient_info draft_info tickets) and returns rendered emails
    //    fsend_emails (optional):  Forthic expecting test mode flag and email records and returns an array of send errors
    //    interp: Forthic interpreter

    const [initialized, set_initialized] = useState(false)
    const [records, setRecords] = useState([]);
    const [modalActivity, setModalActivity] = useState(ModalActivities.ViewTickets);

    if (!props.message_broker)  throw "TicketsModal expects a message_broker prop"
    let email_configured = props.logged_in_username && props.frecipients && props.frender_user_email && props.fgenerate_emails

    useEffect(() => {
        if (initialized)   return

        // Update the records based on messages from the message_broker
        let subscription = props.message_broker.subscribe((message) => {
            let _records = message[props.records_field]
            if (!_records)   _records = []
            setRecords(_records)
        })
        set_initialized(true)

        return () => {
            if (subscription)   props.message_broker.unsubscribe(subscription)
        }
    }, []);


    // Returns the modal title based on the current activity
    function get_modal_title() {
        switch (modalActivity) {
            case ModalActivities.ViewTickets:
                return "Ticket Details"

            case ModalActivities.EmailCampaign:
                return "Email Campaign"

            default:
                return "Tickets Modal"
        }
    }

    // Filters props.column_infos to return only those that are in the records
    function get_column_info() {
        if (!records || records.length == 0)   return []
        const record = records[0]
        const result = props.column_infos.filter((col_info) => record[col_info.field])
        return result
    }


    // Returns the modal body based on the current activity
    function get_modal_body() {
        switch (modalActivity) {
            case ModalActivities.ViewTickets:
                let records_table_props = { ...props.record_table_props, records: records, column_info: get_column_info(), interp: props.interp }
                return (
                    <ViewTicketsActivity
                        records_table_props={records_table_props}
                        set_modal_activity={(activity) => setModalActivity(activity)}
                        email_configured={email_configured}
                    />
                )

            case ModalActivities.EmailCampaign:
                return (
                    <EmailCampaignActivity
                        logged_in_username={props.logged_in_username}
                        records={records}
                        set_modal_activity={(activity) => setModalActivity(activity)}
                        interp={props.interp}
                        frecipients={props.frecipients}
                        frender_user_email={props.frender_user_email}
                        fgenerate_emails={props.fgenerate_emails}
                        fsend_emails={props.fsend_emails}
                        close_modal={() => handleClose()}
                    />
                )
        }
    }

    const handleClose = () => {
        setModalActivity(ModalActivities.ViewTickets)
        setRecords([])
    };


    return (
        <>
            <Modal show={records.length && records.length > 0} onHide={handleClose}>
                <Modal.Header closeButton>
                    <Modal.Title>{get_modal_title()}</Modal.Title>
                </Modal.Header>
                <Modal.Body>
                    {get_modal_body()}
                </Modal.Body>
            </Modal>
        </>
    )
}

export default TicketsModal;
