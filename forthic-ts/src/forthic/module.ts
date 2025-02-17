import { WordExecutionError } from "./errors";
import { Interpreter } from "./interpreter";
import { CodeLocation } from "./tokenizer";
import { IntentionalStopError } from "./errors";

export type WordHandler =
  | ((interp: Interpreter) => Promise<void>)
  | ((interp: Interpreter) => void);

// -------------------------------------
// Variable
export class Variable {
  name: string;
  value: any;

  constructor(name: string, value: any = null) {
    this.name = name;
    this.value = value;
  }

  get_name(): string {
    return this.name;
  }

  set_value(val: any): void {
    this.value = val;
  }

  get_value(): any {
    return this.value;
  }

  dup(): Variable {
    const result = new Variable(this.name, this.value);
    return result;
  }
}

// -------------------------------------
// Words

export class Word {
  name: string;
  string: string;
  location: CodeLocation | null;

  constructor(name: string) {
    this.name = name;
    this.string = name;
    this.location = null;
  }

  set_location(location: any): void {
    this.location = location;
  }

  get_location(): any {
    return this.location;
  }

  async execute(_interp: Interpreter, _options?: any): Promise<void> {
    throw "Must override Word.execute";
  }
}

export class PushValueWord extends Word {
  value: any;

  constructor(name: string, value: any) {
    super(name);
    this.value = value;
  }

  async execute(interp: Interpreter): Promise<void> {
    interp.stack_push(this.value);
  }
}

export class DefinitionWord extends Word {
  words: Word[];
  cur_index: number;

  constructor(name: string) {
    super(name);
    this.words = [];
    this.cur_index = 0;
  }

  add_word(word: Word): void {
    this.words.push(word);
  }

  async execute(
    interp: Interpreter,
  ): Promise<void> {

    for (let i = 0; i < this.words.length; i++) {
      const word = this.words[i];
      try {
        await word.execute(interp);
      } catch (e) {
        const tokenizer = interp.get_tokenizer();
        throw new WordExecutionError(interp.get_top_input_string(), this.name, e, tokenizer.get_token_location());
      }
    }
  }
}

export class ModuleWord extends Word {
  handler: WordHandler;

  constructor(name: string, handler: WordHandler) {
    super(name);
    this.handler = handler;
  }

  async execute(interp: Interpreter): Promise<void> {
    try {
      await this.handler(interp);
    }
    catch (e) {
      if (e instanceof IntentionalStopError) {
        throw e;
      }
      const tokenizer = interp.get_tokenizer();
      throw new WordExecutionError(interp.get_top_input_string(), this.name, e, tokenizer.get_token_location());
    }
  }
}

export class ImportedWord extends Word {
  module_word: Word;
  imported_module: Module;

  constructor(module_word: Word, prefix: string, module: Module) {
    if (prefix != "") prefix = prefix + ".";
    super(`${prefix}${module_word.name}`);
    this.module_word = module_word;
    this.imported_module = module;
  }

  async execute(interp: Interpreter): Promise<void> {
    interp.module_stack_push(this.imported_module);
    await this.module_word.execute(interp);
    interp.module_stack_pop();
  }
}

export class ModuleMemoWord extends Word {
  word: Word;
  has_value: boolean;
  value: any;

  constructor(word: Word) {
    super(word.name);
    this.word = word;
    this.has_value = false;
    this.value = null;
  }

  async refresh(interp: Interpreter): Promise<void> {
    await this.word.execute(interp, { ignore_debug: true });
    this.value = interp.stack_pop();
    this.has_value = true;
  }

  async execute(interp: Interpreter): Promise<void> {
    if (!this.has_value) await this.refresh(interp);
    interp.stack_push(this.value);
  }

  reset(): void {
    this.has_value = false;
    this.value = null;
  }
}

export class ModuleMemoBangWord extends Word {
  memo_word: ModuleMemoWord;

  constructor(memo_word: ModuleMemoWord) {
    super(`${memo_word.name}!`);
    this.memo_word = memo_word;
  }

  async execute(interp: Interpreter): Promise<void> {
    await this.memo_word.refresh(interp);
  }
}

export class ModuleMemoBangAtWord extends Word {
  memo_word: ModuleMemoWord;

  constructor(memo_word: ModuleMemoWord) {
    super(`${memo_word.name}!@`);
    this.memo_word = memo_word;
  }

  async execute(interp: Interpreter): Promise<void> {
    await this.memo_word.refresh(interp);
    interp.stack_push(this.memo_word.value);
  }
}

// -------------------------------------
// Module

export class Module {
  words: Word[];
  exportable: string[];
  variables: { [key: string]: Variable };
  modules: { [key: string]: Module };
  module_prefixes: { [key: string]: Set<string> };
  required_modules: { prefix: string; module: Module }[];
  name: string;
  forthic_code: string;
  module_id: string;

  constructor(name: string, interp?: any, forthic_code: string = "") {
    this.words = [];
    this.exportable = [];
    this.variables = {};
    this.modules = {};
    this.module_prefixes = {};
    this.required_modules = [];
    this.name = name;
    this.forthic_code = forthic_code;
    this.module_id = `${name}-${Math.floor(Math.random() * 1000000)}`;
  }

  dup(): Module {
    const self = this;
    const result = new Module(self.name);
    result.words = self.words.slice();
    result.exportable = self.exportable.slice();
    Object.keys(self.variables).forEach(
      (key) => (result.variables[key] = self.variables[key].dup()),
    );
    Object.keys(self.modules).forEach(
      (key) => (result.modules[key] = self.modules[key]),
    );
    result.required_modules = self.required_modules.slice();
    result.forthic_code = self.forthic_code;
    return result;
  }

  copy(interp: Interpreter): Module {
    const self = this;
    const result = new Module(self.name);
    result.words = self.words.slice();
    result.exportable = self.exportable.slice();
    Object.keys(self.variables).forEach(
      (key) => (result.variables[key] = self.variables[key].dup()),
    );
    Object.keys(self.modules).forEach(
      (key) => (result.modules[key] = self.modules[key]),
    );

    Object.entries(self.module_prefixes).forEach(([module_name, prefixes]) => {
      prefixes.forEach((prefix) => {
        result.import_module(prefix, self.modules[module_name], interp);
      });
    });

    result.forthic_code = self.forthic_code;
    return result;
  }

  require_module(prefix: string, module: Module): void {
    this.required_modules.push({
      prefix: prefix,
      module: module,
    });
  }

  find_module(name: string): Module | undefined {
    return this.modules[name];
  }

  add_module_word(word_name: string, word_func: WordHandler): void {
    this.add_exportable_word(new ModuleWord(word_name, word_func));
  }

  add_word(word: Word): void {
    this.words.push(word);
  }

  add_memo_words(word: Word): ModuleMemoWord {
    const memo_word = new ModuleMemoWord(word);
    this.words.push(memo_word);
    this.words.push(new ModuleMemoBangWord(memo_word));
    this.words.push(new ModuleMemoBangAtWord(memo_word));
    return memo_word;
  }

  add_exportable(names: string[]): void {
    this.exportable = this.exportable.concat(names);
  }

  exportable_words(): Word[] {
    const self = this;
    const result: Word[] = [];
    self.words.forEach((word) => {
      if (self.exportable.indexOf(word.name) >= 0) result.push(word);
    });
    return result;
  }

  add_exportable_word(word: Word): void {
    this.words.push(word);
    this.exportable.push(word.name);
  }

  add_variable(name: string, value: any = null): void {
    if (!this.variables[name]) this.variables[name] = new Variable(name, value);
  }

  initialize(interp: Interpreter): void {
    const self = this;
    self.required_modules.forEach((rec) => {
      self.import_module(rec.prefix, rec.module, interp);
    });
  }

  register_module(module_name: string, prefix: string, module: Module): void {
    this.modules[module_name] = module;

    if (!this.module_prefixes[module_name])
      this.module_prefixes[module_name] = new Set();
    this.module_prefixes[module_name].add(prefix);
  }

  import_module(prefix: string, module: Module, interp: Interpreter): void {
    const self = this;
    const new_module = module.dup();
    new_module.initialize(interp);

    const words = new_module.exportable_words();
    words.forEach((word) => {
      self.add_word(new ImportedWord(word, prefix, new_module));
    });
    self.register_module(module.name, prefix, new_module);
  }

  find_word(name: string): Word | PushValueWord | null {
    let result = this.find_dictionary_word(name);
    if (!result) result = this.find_variable(name);
    return result;
  }

  find_dictionary_word(word_name: string): Word | null {
    for (let i = this.words.length - 1; i >= 0; i--) {
      const w = this.words[i];
      if (w.name == word_name) return w;
    }
    return null;
  }

  find_variable(varname: string): PushValueWord | null {
    const var_result = this.variables[varname];
    let result: PushValueWord | null = null;
    if (var_result) {
      result = new PushValueWord(varname, var_result);
    }
    return result;
  }
}
