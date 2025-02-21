// -------------------------------------
// Tokenizer
export enum TokenType {
  STRING = 1,
  COMMENT,
  START_ARRAY,
  END_ARRAY,
  START_MODULE,
  END_MODULE,
  START_DEF,
  END_DEF,
  START_MEMO,
  WORD,
  EOS,
}

export class Token {
  type: TokenType;
  string: string;
  location: CodeLocation;

  constructor(type: TokenType, string: string, location: CodeLocation) {
    this.type = type;
    this.string = string;
    this.location = location;
  }
}

// 'Data Link Escape'
export const DLE = String.fromCharCode(16);

interface StringDelta {
  start: number;
  end: number;
}

export class Tokenizer {
  reference_location: CodeLocation;
  line: number;
  column: number;
  input_string: string;
  input_pos: number;
  whitespace: string[];
  quote_chars: string[];
  token_start_pos: number;
  token_end_pos: number;
  token_line: number;
  token_column: number;
  token_string: string;
  string_delta: StringDelta | undefined;
  private streaming: boolean;

  constructor(
    string: string,
    reference_location: CodeLocation | null = null,
    streaming: boolean = false,
  ) {
    if (!reference_location) {
      reference_location = new CodeLocation({ screen_name: "<string>" });
    }
    this.reference_location = reference_location;
    this.line = reference_location.line;
    this.column = reference_location.column;
    this.input_string = this.unescape_string(string);
    this.input_pos = 0; // This is the index into the input_string
    this.whitespace = [" ", "\t", "\n", "\r", "(", ")", ","];
    this.quote_chars = ['"', "'", "^", "\u0010"]; // DLE (Data Link Escape) character

    // Token info
    this.token_start_pos = 0;
    this.token_end_pos = 0;
    this.token_line = 0;
    this.token_column = 0;
    this.token_string = "";
    this.string_delta = undefined;
    this.streaming = streaming;
  }

  next_token(): Token {
    this.clear_token_string();
    return this.transition_from_START();
  }

  // ===================
  // Helper functions

  unescape_string(string: string): string {
    let result = string.replace(/&lt;/g, "<");
    result = result.replace(/&gt;/g, ">");
    return result;
  }

  clear_token_string(): void {
    this.token_string = "";
  }

  note_start_token(): void {
    this.token_start_pos = this.input_pos + this.reference_location.start_pos;
    this.token_line = this.line;
    this.token_column = this.column;
  }

  is_whitespace(char: string): boolean {
    return this.whitespace.indexOf(char) >= 0;
  }

  is_quote(char: string): boolean {
    return this.quote_chars.indexOf(char) >= 0;
  }

  is_triple_quote(index: number, char: string): boolean {
    if (!this.is_quote(char)) return false;
    if (index + 2 >= this.input_string.length) return false;
    return (
      this.input_string[index + 1] === char &&
      this.input_string[index + 2] === char
    );
  }

  is_start_memo(index: number): boolean {
    if (index + 1 >= this.input_string.length) return false;
    return (
      this.input_string[index] === "@" && this.input_string[index + 1] === ":"
    );
  }

  advance_position(num_chars: number): number {
    let i;
    if (num_chars >= 0) {
      for (i = 0; i < num_chars; i++) {
        if (this.input_string[this.input_pos] === "\n") {
          this.line += 1;
          this.column = 1;
        } else {
          this.column += 1;
        }
        this.input_pos += 1;
      }
    } else {
      for (i = 0; i < -num_chars; i++) {
        this.input_pos -= 1;
        if (this.input_pos < 0 || this.column < 0) {
          throw new InvalidInputPositionError(
            this.input_string,
            this.get_token_location(),
          );
        }
        if (this.input_string[this.input_pos] === "\n") {
          this.line -= 1;
          this.column = 1;
        } else {
          this.column -= 1;
        }
      }
      i = -i;
    }
    return i;
  }

  get_token_location(): CodeLocation {
    return new CodeLocation({
      screen_name: this.reference_location.screen_name,
      line: this.token_line,
      column: this.token_column,
      start_pos: this.token_start_pos,
      end_pos: this.token_start_pos + this.token_string.length,
    });
  }

  get_input_string(): string {
    return this.input_string;
  }

  get_string_delta(): string {
    if (!this.string_delta) return "";
    return this.input_string.slice(
      this.string_delta.start,
      this.string_delta.end,
    );
  }

  transition_from_START(): Token {
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.note_start_token();
      this.advance_position(1);

      if (this.is_whitespace(char)) continue;
      else if (char === "#") return this.transition_from_COMMENT();
      else if (char === ":") return this.transition_from_START_DEFINITION();
      else if (this.is_start_memo(this.input_pos - 1)) {
        this.advance_position(1); // Skip over ":" in "@:"
        return this.transition_from_START_MEMO();
      } else if (char === ";") {
        this.token_string = char;
        return new Token(TokenType.END_DEF, char, this.get_token_location());
      } else if (char === "[") {
        this.token_string = char;
        return new Token(
          TokenType.START_ARRAY,
          char,
          this.get_token_location(),
        );
      } else if (char === "]") {
        this.token_string = char;
        return new Token(TokenType.END_ARRAY, char, this.get_token_location());
      } else if (char === "{") return this.transition_from_GATHER_MODULE();
      else if (char === "}") {
        this.token_string = char;
        return new Token(TokenType.END_MODULE, char, this.get_token_location());
      } else if (this.is_triple_quote(this.input_pos - 1, char)) {
        this.advance_position(2); // Skip over 2nd and 3rd quote chars
        return this.transition_from_GATHER_TRIPLE_QUOTE_STRING(char);
      } else if (this.is_quote(char))
        return this.transition_from_GATHER_STRING(char);
      else {
        this.advance_position(-1); // Back up to beginning of word
        return this.transition_from_GATHER_WORD();
      }
    }
    return new Token(TokenType.EOS, "", this.get_token_location());
  }

  transition_from_COMMENT(): Token {
    this.note_start_token();
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.token_string += char;
      this.advance_position(1);
      if (char === "\n") {
        this.advance_position(-1);
        break;
      }
    }
    return new Token(
      TokenType.COMMENT,
      this.token_string,
      this.get_token_location(),
    );
  }

  transition_from_START_DEFINITION(): Token {
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);

      if (this.is_whitespace(char)) continue;
      else if (this.is_quote(char)) {
        throw new InvalidWordNameError(
          this.input_string,
          this.get_token_location(),
          "Definition names can't have quotes in them",
        );
      } else {
        this.advance_position(-1);
        return this.transition_from_GATHER_DEFINITION_NAME();
      }
    }

    throw new InvalidWordNameError(
      this.input_string,
      this.get_token_location(),
      "Got EOS in START_DEFINITION",
    );
  }

  transition_from_START_MEMO(): Token {
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);

      if (this.is_whitespace(char)) continue;
      else if (this.is_quote(char))
        throw new InvalidWordNameError(
          this.input_string,
          this.get_token_location(),
          "Memo names can't have quotes in them",
        );
      else {
        this.advance_position(-1);
        return this.transition_from_GATHER_MEMO_NAME();
      }
    }

    throw new InvalidWordNameError(
      this.input_string,
      this.get_token_location(),
      "Got EOS in START_MEMO",
    );
  }

  gather_definition_name(): void {
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);
      if (this.is_whitespace(char)) break;
      if (this.is_quote(char)) {
        throw new InvalidWordNameError(
          this.input_string,
          this.get_token_location(),
          "Definition names can't have quotes in them",
        );
      }
      if (["[", "]", "{", "}"].indexOf(char) >= 0) {
        throw new InvalidWordNameError(
          this.input_string,
          this.get_token_location(),
          `Definition names can't have '${char}' in them`,
        );
      }
      this.token_string += char;
    }
  }

  transition_from_GATHER_DEFINITION_NAME(): Token {
    this.note_start_token();
    this.gather_definition_name();
    return new Token(
      TokenType.START_DEF,
      this.token_string,
      this.get_token_location(),
    );
  }

  transition_from_GATHER_MEMO_NAME(): Token {
    this.note_start_token();
    this.gather_definition_name();
    return new Token(
      TokenType.START_MEMO,
      this.token_string,
      this.get_token_location(),
    );
  }

  transition_from_GATHER_MODULE(): Token {
    this.note_start_token();
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);
      if (this.is_whitespace(char)) break;
      else if (char === "}") {
        this.advance_position(-1);
        break;
      } else this.token_string += char;
    }
    return new Token(
      TokenType.START_MODULE,
      this.token_string,
      this.get_token_location(),
    );
  }

  transition_from_GATHER_TRIPLE_QUOTE_STRING(delim: string): Token {
    this.note_start_token();
    const string_delimiter = delim;
    this.string_delta = {
      start: this.input_pos,
      end: this.input_pos,
    };

    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      if (
        char === string_delimiter &&
        this.is_triple_quote(this.input_pos, char)
      ) {
        this.advance_position(3);
        const token = new Token(
          TokenType.STRING,
          this.token_string,
          this.get_token_location(),
        );
        this.string_delta = undefined;
        return token;
      } else {
        this.advance_position(1);
        this.token_string += char;
        this.string_delta.end = this.input_pos;
      }
    }

    if (this.streaming) {
      return null;
    }
    throw new UnterminatedStringError(
      this.input_string,
      this.get_token_location(),
    );
  }

  transition_from_GATHER_STRING(delim: string): Token {
    this.note_start_token();
    const string_delimiter = delim;
    this.string_delta = {
      start: this.input_pos,
      end: this.input_pos,
    };

    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);
      if (char === string_delimiter) {
        const token = new Token(
          TokenType.STRING,
          this.token_string,
          this.get_token_location(),
        );
        this.string_delta = undefined;
        return token;
      } else {
        this.token_string += char;
        this.string_delta.end = this.input_pos;
      }
    }

    if (this.streaming) {
      return null;
    }
    throw new UnterminatedStringError(
      this.input_string,
      this.get_token_location(),
    );
  }

  transition_from_GATHER_WORD(): Token {
    this.note_start_token();
    while (this.input_pos < this.input_string.length) {
      const char = this.input_string[this.input_pos];
      this.advance_position(1);
      if (this.is_whitespace(char)) break;
      if ([";", "[", "]", "{", "}", "#"].indexOf(char) >= 0) {
        this.advance_position(-1);
        break;
      } else this.token_string += char;
    }
    return new Token(
      TokenType.WORD,
      this.token_string,
      this.get_token_location(),
    );
  }
}

export class CodeLocation {
  screen_name: string;
  line: number;
  column: number;
  start_pos: number;
  end_pos: number;

  constructor({
    screen_name = "<ad-hoc>",
    line = 1,
    column = 1,
    start_pos = 0,
    end_pos = 0,
  }) {
    this.screen_name = screen_name;
    this.line = line;
    this.column = column;
    this.start_pos = start_pos;
    this.end_pos = end_pos;
  }
}

export class PositionedString {
  string: string;
  location: CodeLocation;

  constructor(string: string, location: CodeLocation) {
    this.string = string;
    this.location = location;
  }

  valueOf(): string {
    return this.string;
  }
}

class TokenizerError extends Error {
  constructor(
    private note: string,
    private input: string,
    private location: CodeLocation,
  ) {
    const message = `${note} from ${location.start_pos} to ${location.end_pos} in '${input}'`;
    super(message);
    this.name = "TokenizerError";
  }

  getNote(): string {
    return this.note;
  }

  getInput(): string {
    return this.input;
  }

  getLocation(): CodeLocation {
    return this.location;
  }

  getMessage(): string {
    return this.message;
  }
}

export class InvalidInputPositionError extends TokenizerError {
  constructor(input: string, location: CodeLocation) {
    super("Invalid input position", input, location);
    this.name = "InvalidInputPositionError";
  }
}

export class InvalidWordNameError extends TokenizerError {
  constructor(input: string, location: CodeLocation, note: string) {
    const message = `Invalid word name: ${note}`;
    super(message, input, location);
    this.name = "InvalidWordNameError";
  }
}

export class UnterminatedStringError extends TokenizerError {
  constructor(input: string, location: CodeLocation) {
    super("Unterminated string", input, location);
    this.name = "UnterminatedStringError";
  }
}
