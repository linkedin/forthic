import { TOK_STRING, TOK_COMMENT, TOK_START_ARRAY, TOK_END_ARRAY, TOK_START_MODULE, TOK_END_MODULE,
         TOK_START_DEF, TOK_END_DEF, TOK_WORD, TOK_EOS,
         Token, Tokenizer, DLE} from '../../forthic-js/tokenizer.mjs';
import { run_tests } from './utils.mjs';


function get_tokens(tokenizer) {
    let result = [];

    let token = null;
    do {
        token = tokenizer.next_token();
        result.push(token);
    } while (token.type != TOK_EOS)

    return result;
}

function is_word_token(token, string) {
    return (token.type == TOK_WORD && token.string == string);
}

function is_start_mod_token(token, string) {
    return (token.type == TOK_START_MODULE && token.string == string);
}

function is_string_token(token, string) {
    return (token.type == TOK_STRING && token.string == string);
}

// Checks to see that all basic tokens are recognized
function test_basic() {
    let tokenizer = new Tokenizer("[ ] : DEFINITION ; { } '' WORD");
    let tokens = get_tokens(tokenizer);
    let expected = [TOK_START_ARRAY, TOK_END_ARRAY, TOK_START_DEF, TOK_END_DEF,
                    TOK_START_MODULE, TOK_END_MODULE, TOK_STRING, TOK_WORD, TOK_EOS];

    let result = tokens.every((token, index) => token.type == expected[index]);
    return result;
}


// Checks that end definition (;) is recognized even at end of word
function test_end_definition() {
    let tokenizer = new Tokenizer("WORD; WORD2")
    let tokens = get_tokens(tokenizer)

    if (!is_word_token(tokens[0], "WORD"))   return false;
    if (tokens[1].type != TOK_END_DEF)       return false;
    if (!is_word_token(tokens[2], "WORD2"))  return false;

    return true;
}


function test_start_module() {
    let tokenizer = new Tokenizer("{ {my-mod")
    let tokens = get_tokens(tokenizer);

    if (!is_start_mod_token(tokens[0], ""))        return false;
    if (!is_start_mod_token(tokens[1], "my-mod"))  return false;

    return true;
}

function test_strings() {
    let tokenizer = new Tokenizer(`'Single' ^Caret^ '''Triple Single''' ^^^Triple Caret^^^ ${DLE}Single DLE${DLE}`);
    let tokens = get_tokens(tokenizer);
    if (!is_string_token(tokens[0], "Single"))         return false;
    if (!is_string_token(tokens[1], "Caret"))          return false;
    if (!is_string_token(tokens[2], "Triple Single"))  return false;
    if (!is_string_token(tokens[3], "Triple Caret"))   return false;
    if (!is_string_token(tokens[4], "Single DLE"))     return false;

    tokenizer = new Tokenizer('"Double" """Triple Double"""')
    tokens = get_tokens(tokenizer)
    if (!is_string_token(tokens[0], "Double"))         return false;
    if (!is_string_token(tokens[1], "Triple Double"))  return false;

    return true;
}

function test_arrays() {
    let tokenizer = new Tokenizer("[1 2] [3[4]]");
    let tokens = get_tokens(tokenizer);

    if (tokens[0].type != TOK_START_ARRAY)  return false;
    if (!is_word_token(tokens[1], "1"))     return false;
    if (!is_word_token(tokens[2], "2"))     return false;
    if (tokens[3].type != TOK_END_ARRAY)    return false;
    if (tokens[4].type != TOK_START_ARRAY)  return false;
    if (!is_word_token(tokens[5], "3"))     return false;
    if (tokens[6].type != TOK_START_ARRAY)  return false;
    if (!is_word_token(tokens[7], "4"))     return false;
    if (tokens[8].type != TOK_END_ARRAY)    return false;
    if (tokens[9].type != TOK_END_ARRAY)    return false;

    return true;
}

function test_end_module() {
    let tokenizer = new Tokenizer("WORD1}WORD2");
    let tokens = get_tokens(tokenizer);

    if (!is_word_token(tokens[0], "WORD1"))  return false;
    if (tokens[1].type != TOK_END_MODULE)    return false;
    if (!is_word_token(tokens[2], "WORD2"))  return false;

    return true;
}

let tests = {
    "test_basic": test_basic,
    "test_end_definition": test_end_definition,
    "test_start_module": test_start_module,
    "test_strings": test_strings,
    "test_arrays": test_arrays,
    "test_end_module": test_end_module,
}

export { tests };

// run_tests(tests);
