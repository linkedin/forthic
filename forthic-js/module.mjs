// -------------------------------------
// Variable
class Variable {
    constructor(value=null) {
        this.value = value;
        this.value_stack = [];
    }

    set_value(val) {
        this.value = val;
    }

    get_value() {
        return this.value;
    }

    push_value(val) {
        this.value_stack.push(this.value);
        this.value = val;
    }

    pop_value() {
        if (this.value_stack.length == 0)   throw "Variable: pop_value underflow";
        this.value = this.value_stack.pop();
    }
}


// -------------------------------------
// Words

class Word {
    constructor(name) {
        this.name = name;
    }

    execute(interp) {
        throw "Must override Word.execute";
    }
}


class PushValueWord extends Word {
    constructor(name, value) {
        super(name);
        this.value = value;
    }

    execute(interp) {
        interp.stack_push(this.value);
    }
}

class DefinitionWord extends Word {
    constructor(name) {
        super(name);
        this.words = [];
    }

    add_word(word) {
        this.words.push(word);
    }

    async execute(interp) {
        for (let i=0; i < this.words.length; i++) {
            let word = this.words[i];
            await word.execute(interp);
        }
    }
}


class ModuleWord extends Word {
    constructor(name, handler) {
        super(name);
        this.handler = handler;
    }

    async execute(interp) {
        await this.handler(interp)
    }
}

class ImportedWord extends Word {
    constructor(module_word, prefix, module) {
        if (prefix != "")   prefix = prefix + ".";
        super(`${prefix}${module_word.name}`)
        this.module_word = module_word;
        this.imported_module = module;
        return;
    }

    async execute(interp) {
        interp.module_stack_push(this.imported_module);
        await this.module_word.execute(interp);
        interp.module_stack_pop();
        return;
    }
}

class ModuleMemoWord extends Word {
    constructor(word) {
        super(word.name);
        this.word = word;
        this.has_value = false;
        this.value = null;
    }

    async refresh(interp) {
        await this.word.execute(interp)
        this.value = interp.stack_pop()
        this.has_value = true
    }

    async execute(interp) {
        if (!this.has_value)   await this.refresh(interp);
        interp.stack_push(this.value)
    }
}

class ModuleMemoBangWord extends Word {
    constructor(memo_word) {
        super(`${memo_word.name}!`);
        this.memo_word = memo_word;
    }

    async execute(interp) {
        await this.memo_word.refresh(interp)
    }
}

class ModuleMemoBangAtWord extends Word {
    constructor(memo_word) {
        super(`${memo_word.name}!@`);
        this.memo_word = memo_word;
    }

    async execute(interp) {
        await this.memo_word.refresh(interp)
        interp.stack_push(this.memo_word.value)
    }
}


// -------------------------------------
// Module

class Module {
    constructor(name, interp, forthic_code="") {
        this.words = [];
        this.exportable = [];  // Word names
        this.variables = {};
        this.modules = {};
        this.required_modules = [];
        this.name = name;
        this.forthic_code = forthic_code;
        return
    }

    dup() {
        let self = this;
        let result = new Module(self.name);
        result.words = self.words.slice();
        result.exportable = self.exportable.slice();
        Object.keys(self.variables).forEach(key => result.variables[key] = self.variables[key]);
        Object.keys(self.modules).forEach(key => result.modules[key] = self.modules[key]);
        result.required_modules = self.required_modules.slice();
        result.forthic_code = self.forthic_code;
        return result;
    }

    require_module(prefix, module) {
        this.required_modules.push({
            "prefix": prefix,
            "module": module
        });
    }

    find_module(name) {
        return this.modules[name];
    }

    // Convenience function for adding exportable module words
    add_module_word(word_name, word_func) {
        this.add_exportable_word(new ModuleWord(word_name, word_func))
        return
    }

    add_word(word) {
        this.words.push(word);
    }

    add_memo_words(word) {
        const memo_word = new ModuleMemoWord(word)
        this.words.push(memo_word)
        this.words.push(new ModuleMemoBangWord(memo_word))
        this.words.push(new ModuleMemoBangAtWord(memo_word))
    }

    add_exportable(names) {
        this.exportable = this.exportable.concat(names);
    }

    exportable_words() {
        let self = this;
        let result = [];
        self.words.forEach(word => {
            if (self.exportable.indexOf(word.name) >= 0)   result.push(word);
        });
        return result;
    }

    add_exportable_word(word) {
        this.words.push(word);
        this.exportable.push(word.name);
    }

    add_variable(name, value=null) {
        if (!this.variables[name])   this.variables[name] = new Variable(value);
    }

    async initialize(interp) {
        let self = this;
        let promises = [];
        self.required_modules.forEach(rec => {
            promises.push(self.import_module(rec.prefix, rec.module, interp));
        });

        // Import required modules, then run module code
        return Promise.all(promises).then(() => {
            return interp.run_module_code(self);
        });
    }

    register_module(module_name, module) {
        this.modules[module_name] = module;
    }

    async import_module(module_name, module, interp) {
        let self = this;
        let new_module = module.dup();
        await new_module.initialize(interp);

        let words = new_module.exportable_words();
        words.forEach(word => {
            self.add_word(new ImportedWord(word, module_name, new_module));
        });
        self.register_module(module_name, new_module);
    }

    find_word(name) {
        var result = this.find_dictionary_word(name);
        if (!result) result = this.find_variable(name);
        return result;
    }

    find_dictionary_word(word_name) {
        for (var i=this.words.length-1; i >= 0; i--) {
            var w = this.words[i];
            if (w.name == word_name) return w;
        }
        return null;
    }

    find_variable(varname) {
        var result = this.variables[varname];
        if (result) result = new PushValueWord(varname, result);
        return result;
    }
}


export { Module, Word, ModuleWord, PushValueWord, DefinitionWord };
