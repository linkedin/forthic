import { CodeLocation } from "./tokenizer";

class ForthicError extends Error {
    constructor(private note: string, private location?: CodeLocation) {
        const location_note = location ? ` at ${location.screen_name}:${location.line}:${location.column}` : "";
        const message = `${note}${location_note}`;

        super(message);
    }

    getLocation(): CodeLocation {
        return this.location;
    }

    getNote(): string {
        return this.note;
    }

    getMessage(): string {
        return this.message;
    }
}

// ============================================================
// Interpreter errors

// Interpreter couldn't find word in dictionaries
export class UnknownWordError extends ForthicError {
  constructor(private word: string, location?: CodeLocation) {
    const note = `Unknown word: ${word}`;
    super(note, location);
    this.name = "UnknownWordError";
  }

  getWord(): string {
    return this.word;
  }
}

// Variable name is not allowed
export class InvalidVariableNameError extends ForthicError {
    constructor(private var_name: string, private addl_note: string, location?: CodeLocation) {
        const note = `Invalid variable name: ${var_name}(${addl_note})`;
        super(note, location);
        this.name = "InvalidVariableNameError";
    }

    getVarName(): string {
        return this.var_name;
    }

    getAddlNote(): string {
        return this.addl_note;
    }
}


// Interpreter couldn't find screen
export class UnknownScreenError extends ForthicError {
    constructor(private screen_name: string, location?: CodeLocation) {
        const note = `Unknown screen: ${screen_name}`;
        super(note, location);
        this.name = "UnknownScreenError";
    }

    getScreenName(): string {
        return this.screen_name;
    }
}

// Interpreter couldn't find module
export class UnknownModuleError extends ForthicError {
    constructor(private module_name: string, location?: CodeLocation) {
        const note = `Unknown module: ${module_name}`;
        super(note, location);
        this.name = "UnknownModuleError";
    }

    getModuleName(): string {
        return this.module_name;
    }
}

export class TooManyAttemptsError extends ForthicError {
  constructor(private num_attempts: number, private max_attempts: number, location?: CodeLocation) {
    const note = `Too many recovery attempts: ${num_attempts} of ${max_attempts}`;
    super(note, location);
    this.name = "TooManyAttemptsError";
  }

  getNumAttempts(): number {
    return this.num_attempts;
  }

  getMaxAttempts(): number {
    return this.max_attempts;
  }
}


export class WordExecutionError extends ForthicError {
    constructor(private word_name: string, private error: Error, location?: CodeLocation) {
        const note = `Error executing word: ${word_name}`;
        super(note, location);
        this.name = "WordExecutionError";
    }

    getWordName(): string {
        return this.word_name;
    }

    getError(): Error {
        return this.error;
    }
}


export class ModuleError extends ForthicError {
    constructor(private module_name: string, private error: Error, location?: CodeLocation) {
        const note = `Error in module ${module_name}`;
        super(note, location);
        this.name = "ModuleError";
    }

    getModuleName(): string {
        return this.module_name;
    }

    getError(): Error {
        return this.error;
    }
}


export class StackUnderflowError extends ForthicError {
    constructor(location?: CodeLocation) {
        const note = `Stack underflow`;
        super(note, location);
        this.name = "StackUnderflowError";
    }
}

export class UnknownTokenError extends ForthicError {
    constructor(private token: string, location?: CodeLocation) {
        const note = `(Should never happen) Unknown type of token: ${token}`;
        super(note, location);
        this.name = "UnknownTokenError";
    }

    getToken(): string {
        return this.token;
    }
}

export class MissingSemicolonError extends ForthicError {
    constructor(location?: CodeLocation) {
        const note = `Missing semicolon -- new definition started before previous one was complete`;
        super(note, location);
        this.name = "MissingSemicolonError";
    }
}

export class ExtraSemicolonError extends ForthicError {
    constructor(location?: CodeLocation) {
        const note = `Extra semicolon -- no definition in progress`;
        super(note, location);
        this.name = "ExtraSemicolonError";
    }
}

// ============================================================
// Misc errors

// Error indicating intentional stop
export class IntentionalStopError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "IntentionalStopError";
  }
}

