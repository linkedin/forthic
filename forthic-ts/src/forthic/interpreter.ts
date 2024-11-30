import { TokenType, Token, Tokenizer, CodeLocation } from "./tokenizer";
import { Module, Word, PushValueWord, DefinitionWord } from "./module";
import { GlobalModule } from "./global_module";
import { ForthicError } from "./ForthicError";
import { PositionedString } from "./tokenizer";

type Timestamp = {
  label: string;
  time_ms: number;
};

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
    if (!screen)
      throw new ForthicError(
        "interpreter-199",
        `Unable to find screen "${screen_name}"`,
        "Hmmm...something went wrong. Please file a ticket if this continues to happen",
      );

    const result = screen;
    return result;
  }


  async run(
    string: string,
    reference_location: CodeLocation | null = null,
  ): Promise<boolean> {
    const tokenizer = new Tokenizer(string, reference_location);
    return await this.run_with_tokenizer(tokenizer);
  }

  async run_with_tokenizer(tokenizer: Tokenizer): Promise<boolean> {
    let token: Token;
    do {
      token = tokenizer.next_token();
      try {
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

      } catch (e) {
        console.error(
          "run_with_tokenizer",
          token,
          this.string_location,
          tokenizer.reference_location,
        );
        const error = new ForthicError(
          "interpreter-213",
          `Ran into an error executing this '${token.string}'`,
          "If there is an unknown error in the stack details, please file a ticket so we can resolve it.",
          token.location,
        );
        error.set_caught_error(e);
        throw error;
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
    if (result === undefined) {
      throw new ForthicError(
        "interpreter-236",
        `Couldn't find '${name}' module`,
        `This is most likely a typo in your Forthic code. Please check to see if '${name}' is properly spelled and that you have permission to access it`,
      );
    }
    return result;
  }

  stack_push(val: any) {
    this.stack.push(val);
  }

  stack_pop(): any {
    if (this.stack.length == 0) {
      throw new ForthicError(
        "interpreter-251",
        "Stack underflow",
        "This happens when we expect something to be on the stack, but it's empty. This is caused by a logical error in the Forthic and can be resolved through debugging.",
      );
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
      const error = new ForthicError(
        "interpreter-278",
        `Something went wrong when running the module ${module.name}`,
        "TODO: File a ticket",
      );
      error.set_caught_error(e);
      throw error;
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
      throw new ForthicError(
        "interpreter-362",
        `Hmmm...the interpreter doesn't know what to make of '${token.string}'`,
        "This is most likely caused by a typo in the Forthic code and can be resolved by debugging.",
        token.location,
      );
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
      throw new ForthicError(
        "interpreter-407",
        "A definition was started while an existing definition was not ended",
        "This is probably caused by a missing semicolon. To resolve, ensure that all word definitions end with semicolons.",
        token.location,
      );
    }
    this.cur_definition = new DefinitionWord(token.string);
    this.is_compiling = true;
    this.is_memo_definition = false;
  }

  handle_start_memo_token(token: Token) {
    if (this.is_compiling) {
      throw new ForthicError(
        "interpreter-420",
        "A memo definition was started while an existing definition was not ended",
        "This is probably caused by a missing semicolon. To resolve, ensure that all word definitions end with semicolons.",
        token.location,
      );
    }
    this.cur_definition = new DefinitionWord(token.string);
    this.is_compiling = true;
    this.is_memo_definition = true;
  }

  handle_end_definition_token(token: Token) {
    if (!this.is_compiling) {
      throw new ForthicError(
        "interpreter-433",
        "A definition was ended when one hadn't been started yet",
        "This is probably caused by an extra semicolon. To resolve, ensure that there are no spurious semicolons in the Forthic code.",
        token.location,
      );
    }
    if (!this.cur_definition) {
      throw new ForthicError(
        "interpreter-440",
        "Cannot finish definition because there is no current definition",
        "Please file a ticket",
        token.location,
      );
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
    if (!word) {
      throw new ForthicError(
        "interpreter-458",
        `Could not find word: ${token.string}`,
        "Check to see if you have a typo in your word or the definition of that word",
        token.location,
      );
    }
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
