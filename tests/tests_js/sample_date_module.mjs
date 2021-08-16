import { Module, Word, ModuleWord } from '../../forthic-js/module.mjs';


class SampleDateModule extends Module {
    constructor(interp) {
        super("sample_date", interp);
        this.add_exportable_word(new ModuleWord("TODAY", this.word_TODAY));
        this.add_exportable_word(new ModuleWord("NORMALIZED-TODAY", this.word_NORMALIZED_TODAY));
    }

    // ( -- today )
    word_TODAY(interp) {
        let result = new Date();
        interp.stack_push(result);
    }

    // ( -- today )
    word_NORMALIZED_TODAY(interp) {
        let result = normalize_date(new Date());
        interp.stack_push(result);
    }
}

function normalize_date(date) {
    let year = date.getFullYear();
    let month = date.getMonth() + 1;
    let day = date.getDate();
    let result = new Date(year + "-" + month + "-" + day);
    return result;
}

function new_module(interp) {
    return new SampleDateModule(interp);
}

export { SampleDateModule, normalize_date, new_module };
