import { Module, ModuleWord } from '../module';

// ----- MODULE FLAGS --------------------------------------------------------------------------------------------------
// Module Flags: These are all None but are settable for one-time use to change the behavior
// of module words
let FLAGS = {
    form: null,
}

// Retrieves current value of FLAGS and then clears flags
function get_flags() {
    let result = {...FLAGS}
    FLAGS = {}
    return result
}


class IntakeModule extends Module {
    constructor(interp) {
        super("intake", interp);

        let self = this;
        this.add_exportable_word(new ModuleWord("!FORM", (interp) => this.word_bang_FORM(interp), self));
        this.add_exportable_word(new ModuleWord("FIELD-VALUE", (interp) => this.word_FIELD_VALUE(interp), self));
        this.add_exportable_word(new ModuleWord("CREATE-TICKET", (interp) => this.word_CREATE_TICKET(interp), self));

        // TODO: Convert dollars
        // TODO: Convert number
    }

    // (ConfigurableForm -- )
    word_bang_FORM(interp) {
        const form = interp.stack_pop()
        FLAGS.form = form
    }

    // (field_id -- value)
    word_FIELD_VALUE(interp) {
        const field_id = interp.stack_pop()

        const flags = get_flags()
        if (!flags.form)   throw "intake.FIELD-VALUE requires !FORM to be called to set the form"

        const valuesByFieldId = flags.form.valuesByFieldId
        const value = valuesByFieldId[field_id]
        console.log("intake.VALUE", flags.form, field_id, value)
        interp.stack_push(value);
    }

    // (form_config field_records_by_id values_by_id -- ticket_key)
    // NOTE: We assume the application has defined a `CREATE-TICKET` word
    async word_CREATE_TICKET(interp) {
        const valuesById = interp.stack_pop()
        const fieldsById = interp.stack_pop()
        const formConfig = interp.stack_pop()

        const ticketInfo = {formConfig, fieldsById, valuesById}

        console.log("ticketInfo", ticketInfo)

        interp.stack_push([ticketInfo])

        // NOTE: The `CREATE-TICKET` word should be defined by the app because any custom things that need
        //       to happen should be done there.
        await interp.run("'CREATE-TICKET' SERVER-INTERPRET")
        console.log("CREATE-TICKET result", interp.stack)
    }

}

export default IntakeModule;
