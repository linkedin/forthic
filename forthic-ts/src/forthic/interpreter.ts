import { TokenType, Token, Tokenizer, CodeLocation } from "./tokenizer";
import { Module, Word, PushValueWord, DefinitionWord } from "./module";
import { GlobalModule } from "./global_module";
import { PositionedString } from "./tokenizer";
import {
  UnknownWordError,
  UnknownScreenError,
  UnknownModuleError,
  StackUnderflowError,
  UnknownTokenError,
  MissingSemicolonError,
  ExtraSemicolonError,
  ModuleError,
  TooManyAttemptsError
 } from "./errors";

type Timestamp = {
  label: string;
  time_ms: number;
};

type HandleErrorFunction = (e: Error) => Promise<void>;

type RunOptions = {
  reference_location?: CodeLocation;
  maxAttempts?: number;
  handleError?: HandleErrorFunction;
}

class StartModuleWord extends Word {
  async execute(interp: Interpreter): Promise<void> {
    const self = this;

    // The app module is the only module with a blank name
    if (self.name === "") {
      interp.module_stack_push(interp.get_app_module());
      return;
    }

    // If the module is used by the current module, push it onto the stack, otherwise
    // create a new module.
    let module = interp.cur_module().find_module(self.name);
    if (!module) {
      module = new Module(self.name);
      interp.cur_module().register_module(module.name, module.name, module);

      // If we're at the app module, also register with interpreter
      if (interp.cur_module().name === "") {
        interp.register_module(module);
      }
    }
    interp.module_stack_push(module);
  }
}

class EndModuleWord extends Word {
  constructor() {
    super("}");
  }

  async execute(interp: Interpreter): Promise<void> {
    interp.module_stack_pop();
  }
}

class EndArrayWord extends Word {
  constructor() {
    super("]");
  }

  async execute(interp: Interpreter): Promise<void> {
    const items = [];
    let item = interp.stack_pop();
    // NOTE: This won't infinite loop because interp.stack_pop() will eventually fail
    while (true) {
      if (item instanceof Token && item.type == TokenType.START_ARRAY) break;
      items.push(item);
      item = interp.stack_pop();
    }
    items.reverse();
    interp.stack_push(items);
    return;
  }
}

export class Interpreter {
  private timestamp_id: number;
  private stack: any[];
  private global_module: GlobalModule;
  private app_module: Module;
  private module_stack: Module[];
  private registered_modules: { [key: string]: Module };
  private tokenizer: Tokenizer;
  private is_compiling: boolean;
  private should_stop: boolean;
  private is_memo_definition: boolean;
  private cur_definition: any; // Assuming this can be of any type, update if you have a specific type
  private screens: { [key: string]: any }; // Assuming screens can be of any type, update if you have a specific type
  private default_module_flags: { [key: string]: any }; // Assuming flags can be of any type, update if you have a specific type
  private module_flags: { [key: string]: any }; // Assuming flags can be of any type, update if you have a specific type
  private string_location?: CodeLocation; // Assuming string_location can be of any type, update if you have a specific type
  private word_counts: { [key: string]: number };
  private is_profiling: boolean;
  private start_profile_time: number | null;
  private timestamps: Timestamp[];

  constructor() {
    this.timestamp_id = Math.random();

    this.stack = [];

    // We've split the global module functionality into a "pure" global module and a React global module
    this.global_module = new GlobalModule(this);

    this.app_module = new Module("", this);
    this.module_stack = [this.app_module];
    this.registered_modules = {};
    this.is_compiling = false;
    this.should_stop = false;
    this.is_memo_definition = false;
    this.cur_definition = null;
    this.screens = {};

    // Module flags
    this.default_module_flags = {};
    this.module_flags = {};

    // Debug support
    this.string_location = null;

    // Profiling support
    this.word_counts = {};
    this.is_profiling = false;
    this.start_profile_time = null;
    this.timestamps = [];
  }

  get_stack(): any[] {
    return this.stack;
  }

  halt() {
    this.should_stop = true;
  }

  get_app_module(): Module {
    return this.app_module;
  }

  // Getter method for string_location
  get_string_location(): CodeLocation | null {
    return this.string_location;
  }

  // Module flag support
  set_flags(module_id: string, flags: { [key: string]: any }): void {
    if (!this.default_module_flags) {
      this.default_module_flags = {};
    }
    if (!this.module_flags) {
      this.module_flags = {};
    }

    this.default_module_flags[module_id] = flags;
    this.module_flags[module_id] = flags;
  }

  get_flags(module_id: string): { [key: string]: any } {
    const module_flags = this.module_flags[module_id] || {};
    const result = {};
    Object.keys(module_flags).forEach((k) => {
      result[k] = module_flags[k];
    });
    this.module_flags[module_id] = { ...this.default_module_flags[module_id] };
    return result;
  }

  modify_flags(module_id: string, flags: { [key: string]: any }): void {
    const module_flags = this.module_flags[module_id] || {};
    this.module_flags[module_id] = { ...module_flags, ...flags };
  }

  reset() {
    this.stack = [];
    this.app_module.variables = {};

    this.module_stack = [this.app_module];
    this.is_compiling = false;
    this.is_memo_definition = false;
    this.cur_definition = null;

    // Debug support
    this.string_location = null;
  }


  get_screen_forthic(screen_name: string): string {
    const screen = this.screens[screen_name];
    if (!screen) {
      throw new UnknownScreenError(screen_name, this.string_location);
    }

    const result = screen;
    return result;
  }

  async run(
    string: string,
    options: RunOptions = {}
  ): Promise<boolean|number> {
    const tokenizer = new Tokenizer(string, options.reference_location);

    if (options.handleError) {
      this.tokenizer = tokenizer;
      return await this.execute_with_recovery(options);
    } else {
      return await this.run_with_tokenizer(tokenizer);
    }
  }

  async execute_with_recovery(options: RunOptions, numAttempts: number = 0): Promise<number> {
    const maxAttempts = options.maxAttempts || 3;
    try {
        numAttempts++;
        if (numAttempts > maxAttempts) {
          throw new TooManyAttemptsError(numAttempts, maxAttempts);
        }
        await this.continue();
        return numAttempts;
    }
    catch (e) {
        await options.handleError(e);
        return await this.execute_with_recovery(options, numAttempts);
    }
}

  async continue() {
    await this.run_with_tokenizer(this.tokenizer);
    return
  }

  async run_with_tokenizer(tokenizer: Tokenizer): Promise<boolean> {
    this.tokenizer = tokenizer;
    let token: Token;
    do {
      token = tokenizer.next_token();
        await this.handle_token(token);
        if (token.type === TokenType.EOS) {
          break;
        }
        if (this.should_stop) {
          this.should_stop = false;
          break;
        }

        // Regardless of debug mode, we don't want to stop when compiling definitions or going through comments
        if (
          token.type === TokenType.START_DEF ||
          token.type === TokenType.END_DEF ||
          token.type == TokenType.COMMENT ||
          this.is_compiling
        ) {
          continue;
        }

      // eslint-disable-next-line no-constant-condition
    } while (true);
    return true; // Done executing
  }

  cur_module(): Module {
    const result = this.module_stack[this.module_stack.length - 1];
    return result;
  }

  find_module(name: string): Module {
    const result = this.registered_modules[name];
    if (result === undefined)   throw new UnknownModuleError(name, this.string_location);
    return result;
  }

  stack_push(val: any) {
    this.stack.push(val);
  }

  stack_pop(): any {
    if (this.stack.length == 0) {
      throw new StackUnderflowError(this.string_location);
    }
    let result = this.stack.pop();

    // If we have a PositionedString, we need to record the location
    this.string_location = undefined;
    if (result instanceof PositionedString) {
      const positioned_string = result;
      result = positioned_string.valueOf();
      this.string_location = positioned_string.location;
    }
    return result;
  }

  module_stack_push(module: Module) {
    this.module_stack.push(module);
  }

  module_stack_pop(): Module {
    return this.module_stack.pop();
  }

  register_module(module: Module) {
    this.registered_modules[module.name] = module;
  }

  async run_module_code(module: Module): Promise<void> {
    this.module_stack_push(module);
    try {
      await this.run(module.forthic_code);
    } catch (e) {
      throw new ModuleError(module.name, e, this.string_location);
    }

    this.module_stack_pop();
  }

  find_word(name: string): Word {
    let result = null;
    for (let i = this.module_stack.length - 1; i >= 0; i--) {
      const m = this.module_stack[i];
      result = m.find_word(name);
      if (result) break;
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

  count_word(word: Word) {
    if (!this.is_profiling) return;
    const name = word.name;
    if (!this.word_counts[name]) this.word_counts[name] = 0;
    this.word_counts[name] += 1;
  }

  stop_profiling() {
    this.add_timestamp("END");
    this.is_profiling = false;
  }

  add_timestamp(label: string) {
    if (!this.is_profiling) return;
    const timestamp: Timestamp = {
      label: label,
      time_ms: Date.now() - this.start_profile_time,
    };
    this.timestamps.push(timestamp);
  }

  word_histogram(): any[] {
    const items = [];
    Object.keys(this.word_counts).forEach((name) => {
      items.push({ word: name, count: this.word_counts[name] });
    });
    const result = items.sort((l, r) => r["count"] - l["count"]);
    return result;
  }

  profile_timestamps(): Timestamp[] {
    return this.timestamps;
  }

  // ======================
  // Handle tokens

  async handle_token(token: Token) {
    if (token.type == TokenType.STRING) this.handle_string_token(token);
    else if (token.type == TokenType.COMMENT) this.handle_comment_token(token);
    else if (token.type == TokenType.START_ARRAY)
      this.handle_start_array_token(token);
    else if (token.type == TokenType.END_ARRAY)
      this.handle_end_array_token(token);
    else if (token.type == TokenType.START_MODULE)
      await this.handle_start_module_token(token);
    else if (token.type == TokenType.END_MODULE)
      this.handle_end_module_token(token);
    else if (token.type == TokenType.START_DEF)
      this.handle_start_definition_token(token);
    else if (token.type == TokenType.START_MEMO)
      this.handle_start_memo_token(token);
    else if (token.type == TokenType.END_DEF)
      this.handle_end_definition_token(token);
    else if (token.type == TokenType.WORD) await this.handle_word_token(token);
    else if (token.type == TokenType.EOS) {
      return;
    } else {
      throw new UnknownTokenError(token.string, this.string_location);
    }
  }

  handle_string_token(token: Token) {
    const value = new PositionedString(token.string, token.location);
    this.handle_word(new PushValueWord("<string>", value));
  }

  // Start/end module tokens are treated as IMMEDIATE words *and* are also compiled
  async handle_start_module_token(token: Token) {
    const self = this;
    const word = new StartModuleWord(token.string);

    if (self.is_compiling) self.cur_definition.add_word(word);
    self.count_word(word); // For profiling
    word.execute(self);
  }

  async handle_end_module_token(_token: Token) {
    const self = this;
    const word = new EndModuleWord();

    if (self.is_compiling) self.cur_definition.add_word(word);
    self.count_word(word);
    word.execute(self);
  }

  handle_start_array_token(token: Token) {
    this.handle_word(new PushValueWord("<start_array_token>", token));
  }

  handle_end_array_token(_token: Token) {
    this.handle_word(new EndArrayWord());
  }

  handle_comment_token(_token: Token) {
    // console.log("Comment:", token.string);
  }

  handle_start_definition_token(token: Token) {
    if (this.is_compiling) {
      throw new MissingSemicolonError(token.location);
    }
    this.cur_definition = new DefinitionWord(token.string);
    this.is_compiling = true;
    this.is_memo_definition = false;
  }

  handle_start_memo_token(token: Token) {
    if (this.is_compiling) {
      throw new MissingSemicolonError(token.location);
    }
    this.cur_definition = new DefinitionWord(token.string);
    this.is_compiling = true;
    this.is_memo_definition = true;
  }

  handle_end_definition_token(token: Token) {
    if (!this.is_compiling || !this.cur_definition) {
      throw new ExtraSemicolonError(token.location);
    }

    if (this.is_memo_definition) {
      this.cur_module().add_memo_words(this.cur_definition);
    } else {
      this.cur_module().add_word(this.cur_definition);
    }
    this.is_compiling = false;
  }

  async handle_word_token(token: Token) {
    const word = this.find_word(token.string);
    if (!word) throw new UnknownWordError(token.string, token.location);
    await this.handle_word(word, token.location);
    return;
  }

  async handle_word(word: Word, location: CodeLocation | null = null) {
    if (this.is_compiling) {
      word.set_location(location);
      this.cur_definition.add_word(word);
    } else {
      this.count_word(word);
      await word.execute(this);
    }
  }
}

export function dup_interpreter(interp: Interpreter) {
  const interp_any = interp as any;
  const result = new Interpreter() as any;
  result.app_module = interp_any.app_module.dup();
  result.module_stack = [result.app_module];
  result.stack = interp_any.stack.slice();
  result.registered_modules = interp_any.registered_modules;
  result.screens = { ...interp_any.screens };
  return result;
}
