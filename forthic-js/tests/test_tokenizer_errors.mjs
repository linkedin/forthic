import { Tokenizer } from '../../forthic-js/tokenizer.mjs';
import { run_tests } from './utils.mjs';

// Raise exception if strings are unterminated
function test_unterminated_string() {
    let tokenizer = new Tokenizer("'Unterminated")
    try {
        tokenizer.next_token();
    }
    catch(e) {
        console.log("PASS: ", e);
        return true;
    }

    return false;
}

// Can't have an empty definition
function test_start_definition_eos() {
    let tokenizer = new Tokenizer(":")
    try {
        tokenizer.next_token();
    }
    catch(e) {
        console.log("PASS: ", e);
        return true;
    }

    return false;
}

// Can't have definition with Forthic special chars
function test_start_definition_special_chars() {

    function check_for_exception(input) {
        let res = false;
        let tokenizer = new Tokenizer(input);
        try {
            let token = tokenizer.next_token();
            console.log(token);
        }
        catch(e) {
            console.log("PASS: ", e);
            res = true;
        }
        return res;
    }

    let start_defs = [": 'HOWDY", ": HOW'DY", ": HOW[DY", ": HOW]DY", ": HOW{DY", ": HOW}DY"];

    let result = start_defs.every(input => check_for_exception(input));
    return result;
}

let tests = {
    "test_unterminated_string": test_unterminated_string,
    "test_start_definition_eos": test_start_definition_eos,
    "test_start_definition_special_chars": test_start_definition_special_chars,
}


export { tests };
// run_tests(tests);
