// -------------------------------------
// Tokenizer
let TOK_STRING = 1;
let TOK_COMMENT = 2;
let TOK_START_ARRAY = 3;
let TOK_END_ARRAY = 4;
let TOK_START_MODULE = 5;
let TOK_END_MODULE = 6;
let TOK_START_DEF = 7;
let TOK_END_DEF = 8;
let TOK_WORD = 9;
let TOK_EOS = 10;

class Token {
    constructor(type, string) {
        this.type = type;
        this.string = string;
    }
}

// 'Data Link Escape'
const DLE = String.fromCharCode(16);

class Tokenizer {
    constructor(string) {
        this.input_string = this.unescape_string(string);
        this.position = 0;
        this.whitespace = [' ', '\t', '\n', '\r', '(', ')'];
        this.quote_chars = ['"', "'", '^', DLE];
        this.token_string = '';
    }

    next_token() {
        this.clear_token_string();
        return this.transition_from_START();
    }

    // ===================
    // Helper functions

    unescape_string(string) {
        var result = string.replace(/&lt;/g, "<");
        result = result.replace(/&gt;/g, ">");
        return result;
    }

    clear_token_string() {
        this.token_string = '';
    }

    is_whitespace(char) {
        return this.whitespace.indexOf(char) >= 0;
    }

    is_quote(char) {
        return this.quote_chars.indexOf(char) >= 0;
    }

    is_triple_quote(index, char) {
        if (!this.is_quote(char)) return false;
        if (index + 2 >= this.input_string.length) return false;
        return this.input_string[index+1] == char && this.input_string[index+2] == char;
    }

    transition_from_START() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;
            if (this.is_whitespace(char)) continue;
            else if (char == '#') return this.transition_from_COMMENT();
            else if (char == ':') return this.transition_from_START_DEFINITION();
            else if (char == ';') return new Token(TOK_END_DEF, char);
            else if (char == '[') return new Token(TOK_START_ARRAY, char);
            else if (char == ']') return new Token(TOK_END_ARRAY, char);
            else if (char == '{') return this.transition_from_GATHER_MODULE();
            else if (char == '}') return new Token(TOK_END_MODULE, char);
            else if (this.is_triple_quote(this.position-1, char)) {
                this.position += 2  // Skip over 2nd and 3rd quote chars
                return this.transition_from_GATHER_TRIPLE_QUOTE_STRING(char);
            }
            else if (this.is_quote(char)) return this.transition_from_GATHER_STRING(char);
            else {
                this.position -= 1  // Back up to beginning of word
                return this.transition_from_GATHER_WORD();
            }
        }
        return new Token(TOK_EOS, '');
    }

    transition_from_COMMENT() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.token_string += char;
            this.position += 1;
            if (char == '\n') break;
        }
        return new Token(TOK_COMMENT, this.token_string);
    }

    transition_from_START_DEFINITION() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;

            if (this.is_whitespace(char)) continue;
            else if (this.is_quote(char)) throw "Definitions shouldn't have quotes in them";
            else {
                this.position -= 1;
                return this.transition_from_GATHER_DEFINITION_NAME();
            }
        }

        throw "Got EOS in START_DEFINITION";
    }

    transition_from_GATHER_DEFINITION_NAME() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;
            if (this.is_whitespace(char)) break;
            if (this.is_quote(char))
                throw "Definitions can't have quotes in them";
            if (['[', ']', '{', '}' ].indexOf(char) >= 0)
                throw "Definitions can't have '" + char +"' in them";
            this.token_string += char;
        }
        return new Token(TOK_START_DEF, this.token_string)
    }

    transition_from_GATHER_MODULE() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;
            if (this.is_whitespace(char)) break;
            else if (char == '}') {
                this.position -= 1;
                break;
            }
            else this.token_string += char;
        }
        return new Token(TOK_START_MODULE, this.token_string);
    }

    transition_from_GATHER_TRIPLE_QUOTE_STRING(delim) {
        let string_delimiter = delim;

        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            if (char == string_delimiter && this.is_triple_quote(this.position, char)) {
                this.position += 3;
                return new Token(TOK_STRING, this.token_string);
            }
            else {
                this.position += 1;
                this.token_string += char;
            }
        }
        throw "Unterminated triple quote string";
    }

    transition_from_GATHER_STRING(delim) {
        let string_delimiter = delim;

        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;
            if (char == string_delimiter) return new Token(TOK_STRING, this.token_string);
            else this.token_string += char;
        }
        throw "Unterminated string";
    }

    transition_from_GATHER_WORD() {
        while (this.position < this.input_string.length) {
            var char = this.input_string[this.position];
            this.position += 1;
            if (this.is_whitespace(char)) break;
            if ([';', '[', ']', '}'].indexOf(char) >= 0) {
                this.position -= 1;
                break;
            }
            else this.token_string += char;
        }
        return new Token(TOK_WORD, this.token_string);
    }
}


export { TOK_STRING, TOK_COMMENT, TOK_START_ARRAY, TOK_END_ARRAY, TOK_START_MODULE,
         TOK_END_MODULE, TOK_START_DEF, TOK_END_DEF, TOK_WORD, TOK_EOS,
         Token, Tokenizer, DLE };
