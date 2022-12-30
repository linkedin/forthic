import { TOK_STRING, TOK_COMMENT, TOK_START_ARRAY, TOK_END_ARRAY, TOK_START_MODULE, TOK_END_MODULE,
         TOK_START_DEF, TOK_END_DEF, TOK_START_MEMO, TOK_WORD, TOK_EOS,
         Token, Tokenizer } from './tokenizer';
import { Module, Word, PushValueWord, DefinitionWord } from './module';
import { GlobalModule } from './global_module';


class StartModuleWord extends Word {
    async execute(interp) {
        let self = this;

        // The app module is the only module with a blank name
        if (self.name == "") {
            interp.module_stack_push(interp.app_module);
            return;
        }

        // If the module is used by the current module, push it onto the stack, otherwise
        // create a new module.
        let module = interp.cur_module().find_module(self.name);
        if (!module) {
            module = new Module(self.name);
            await interp.cur_module().register_module(module.name, module);
        }
        interp.module_stack_push(module);
    }
}

class EndModuleWord extends Word {
    constructor() {
        super("}");
    }

    async execute(interp) {
        interp.module_stack_pop();
    }
}

class EndArrayWord extends Word {
    constructor() {
        super("]");
    }

    execute(interp) {
        var items = []
        var item = interp.stack_pop()
        // NOTE: This won't infinite loop because interp.stack_pop() will eventually fail
        while (1) {
            if (item instanceof Token && item.type == TOK_START_ARRAY) break;
            items.push(item);
            item = interp.stack_pop();
        }
        items.reverse();
        interp.stack_push(items);
        return
    }
}


class Interpreter {
    constructor() {
        this.stack = [];
        this.global_module = new GlobalModule(this);
        this.app_module = new Module("", this);
        this.module_stack = [this.app_module];
        this.registered_modules = {};
        this.is_compiling = false;
        this.is_memo_definition = false;
        this.cur_definition = null;

        // Profiling support
        this.word_counts = {};
        this.is_profiling = false;
        this.start_profile_time = null;
        this.timestamps = [];
    }


    async run(string) {
        let tokenizer = new Tokenizer(string);
        var token = tokenizer.next_token();
        while (token.type !== TOK_EOS) {
            await this.handle_token(token);
            token = tokenizer.next_token();
        }
    }

    async run_in_module(module, string) {
        this.module_stack_push(module);
        await this.run(string);
        this.module_stack_pop();
    }

    cur_module() {
        let result = this.module_stack[this.module_stack.length-1];
        return result;
    }


    find_module(name) {
        let result = this.registered_modules[name];
        if (result === undefined) throw ("Can't find module: " + name);
        return result;
    }

    stack_push(val) {
        this.stack.push(val);
    }

    stack_pop() {
        if (this.stack.length == 0)  throw "Stack underflow";
        let result = this.stack.pop();
        return result;
    }

    module_stack_push(module) {
        this.module_stack.push(module);
    }

    module_stack_pop() {
        return this.module_stack.pop();
    }

    async register_module(module) {
        this.registered_modules[module.name] = module;
    }

    async run_module_code(module) {
        this.module_stack_push(module);
        await this.run(module.forthic_code);
        this.module_stack_pop();
    }

    find_word(name) {
        let result = null;
        for (let i=this.module_stack.length-1; i >= 0; i--) {
            let m = this.module_stack[i];
            result = m.find_word(name);
            if (result)   break;
        }
        if (!result) {
            result = this.global_module.find_word(name);
        }
        return result;
    }


    // ======================
    // Profiling
    start_profiling() {
        this.is_profiling = true;
        this.timestamps = [];
        this.start_profile_time = Date.now();
        this.add_timestamp("START");
        this.word_counts = {};
    }

    count_word(word) {
        if (!this.is_profiling)   return;
        var name = word.name;
        if (!this.word_counts[name])   this.word_counts[name] = 0;
        this.word_counts[name] += 1;
    }

    stop_profiling() {
        this.add_timestamp("END");
        this.is_profiling = false;
    }

    add_timestamp(label) {
        if (!this.is_profiling)   return;
        var timestamp = {"label": label, "time_ms": Date.now() - this.start_profile_time};
        this.timestamps.push(timestamp);
    }

    word_histogram() {
        var items = [];
        Object.keys(this.word_counts).forEach(name => {
            items.push({"word": name, "count": this.word_counts[name]});
        });
        var result = items.sort((l, r) => r["count"] - l["count"]);
        return result;
    }

    profile_timestamps() {
        return this.timestamps;
    }


    // ======================
    // Handle tokens

    async handle_token(token) {
        if (token.type == TOK_STRING) this.handle_string_token(token);
        else if (token.type == TOK_COMMENT) this.handle_comment_token(token);
        else if (token.type == TOK_START_ARRAY) this.handle_start_array_token(token);
        else if (token.type == TOK_END_ARRAY) this.handle_end_array_token(token);
        else if (token.type == TOK_START_MODULE) await this.handle_start_module_token(token);
        else if (token.type == TOK_END_MODULE) this.handle_end_module_token(token);
        else if (token.type == TOK_START_DEF) this.handle_start_definition_token(token);
        else if (token.type == TOK_START_MEMO) this.handle_start_memo_token(token);
        else if (token.type == TOK_END_DEF) this.handle_end_definition_token(token);
        else if (token.type == TOK_WORD) await this.handle_word_token(token);
        else throw ("Unknown token: " + token);
    }

    handle_string_token(token) {
        this.handle_word(new PushValueWord("<string>", token.string));
    }

    // Start/end module tokens are treated as IMMEDIATE words *and* are also compiled
    async handle_start_module_token(token) {
        let self = this;
        let word = new StartModuleWord(token.string);

        if (self.is_compiling)   self.cur_definition.add_word(word);
        self.count_word(word);
        word.execute(self);
    }

    async handle_end_module_token(token) {
        let self = this;
        let word = new EndModuleWord();

        if (self.is_compiling)   self.cur_definition.add_word(word);
        self.count_word(word);
        word.execute(self);
    }

    handle_start_array_token(token) {
        this.handle_word(new PushValueWord("<start_array_token>", token));
    }

    handle_end_array_token(token) {
        this.handle_word(new EndArrayWord());
    }

    handle_comment_token(token) {
        // console.log("Comment:", token.string);
    }

    handle_start_definition_token(token) {
        if (this.is_compiling) throw "Can't have nested definitions";
        this.cur_definition = new DefinitionWord(token.string);
        this.is_compiling = true;
        this.is_memo_definition = false;
    }

    handle_start_memo_token(token) {
        if (this.is_compiling) throw "Can't have nested definitions";
        this.cur_definition = new DefinitionWord(token.string);
        this.is_compiling = true;
        this.is_memo_definition = true;
    }

    handle_end_definition_token(token) {
        if (!this.is_compiling)   throw "Unmatched end definition";
        if (!this.cur_definition) throw "Cannot finish definition because there is no 'cur_definition'"

        if (this.is_memo_definition) {
            this.cur_module().add_memo_words(this.cur_definition);
        }
        else {
            this.cur_module().add_word(this.cur_definition);
        }
        this.is_compiling = false;
    }

    async handle_word_token(token) {
        let word = this.find_word(token.string);
        if (!word) throw ("Unknown word: " + token.string);
        await this.handle_word(word);
        return
    }

    async handle_word(word) {
        if (this.is_compiling) {
            this.cur_definition.add_word(word);
        }
        else {
            this.count_word(word);
            await word.execute(this);
        }
    }

}

export { Interpreter };
