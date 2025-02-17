import { CodeLocation } from "./tokenizer";

export class ForthicError extends Error {
    constructor(private forthic: string, private note: string, private location?: CodeLocation) {
        const message = `${note}`;

        super(message);
    }

    getDescription(): string {
        return get_error_description(this.forthic, this);
    }

    getLocation(): CodeLocation {
        return this.location;
    }

    getForthic(): string {
        return this.forthic;
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
  constructor(forthic: string, private word: string, location?: CodeLocation) {
    const note = `Unknown word: ${word}`;
    super(forthic, note, location);
    this.name = "UnknownWordError";
  }

  getWord(): string {
    return this.word;
  }
}

// Variable name is not allowed
export class InvalidVariableNameError extends ForthicError {
    constructor(forthic: string, private var_name: string, private addl_note: string, location?: CodeLocation) {
        const note = `Invalid variable name: ${var_name}(${addl_note})`;
        super(forthic, note, location);
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
    constructor(forthic: string, private screen_name: string, location?: CodeLocation) {
        const note = `Unknown screen: ${screen_name}`;
        super(forthic, note, location);
        this.name = "UnknownScreenError";
    }

    getScreenName(): string {
        return this.screen_name;
    }
}

// Interpreter couldn't find module
export class UnknownModuleError extends ForthicError {
    constructor(forthic: string, private module_name: string, location?: CodeLocation) {
        const note = `Unknown module: ${module_name}`;
        super(forthic, note, location);
        this.name = "UnknownModuleError";
    }

    getModuleName(): string {
        return this.module_name;
    }
}

export class TooManyAttemptsError extends ForthicError {
  constructor(forthic: string, private num_attempts: number, private max_attempts: number, location?: CodeLocation) {
    const note = `Too many recovery attempts: ${num_attempts} of ${max_attempts}`;
    super(forthic, note, location);
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
    constructor(forthic: string, private word_name: string, private error: Error, location?: CodeLocation) {
        const note = `(${error.message}) when executing word ${word_name}`;
        super(forthic, note, location);
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
    constructor(forthic: string, private module_name: string, private error: Error, location?: CodeLocation) {
        const note = `Error in module ${module_name}`;
        super(forthic, note, location);
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
    constructor(forthic: string, location?: CodeLocation) {
        const note = `Stack underflow`;
        super(forthic, note, location);
        this.name = "StackUnderflowError";
    }
}

export class UnknownTokenError extends ForthicError {
    constructor(forthic: string, private token: string, location?: CodeLocation) {
        const note = `(Should never happen) Unknown type of token: ${token}`;
        super(forthic, note, location);
        this.name = "UnknownTokenError";
    }

    getToken(): string {
        return this.token;
    }
}

export class MissingSemicolonError extends ForthicError {
    constructor(forthic: string, location?: CodeLocation) {
        const note = `Definition was missing its semicolon`;
        super(forthic, note, location);
        this.name = "MissingSemicolonError";
    }
}

export class ExtraSemicolonError extends ForthicError {
    constructor(forthic: string, location?: CodeLocation) {
        const note = `Unexpected semicolon -- no definition in progress`;
        super(forthic, note, location);
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

// ============================================================
// Utility functions
export function get_error_description(forthic: string, forthicError: ForthicError): string {
    // If don't have any extra info, just return the note
    if (!forthic || forthic === "" || forthicError.getLocation() === undefined) {
        return forthicError.getNote();
    }

    // Otherwise, return the note and indicate where the error occurred
    const location = forthicError.getLocation();

    // Extract line where error occurred and highlight the error
    const line_num = location.line;

    // Get lines up to line_num
    const lines = forthic.split("\n").slice(0, line_num);

    // Error line is the line with the error, but with the error location highlighted and everything else a space
    const error_line = " ".repeat(location.column-1) + "^".repeat(location.end_pos - location.start_pos);

    // Print all forthic lines with numbers adding the error line
    const error_message = `${forthicError.getNote()} at line ${line_num}:\n\`\`\`\n${lines.map((line) => `${line}`).join("\n")}\n${error_line}\n\`\`\``;
    return error_message;
}