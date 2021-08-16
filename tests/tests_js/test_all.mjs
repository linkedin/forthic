import * as TokTests from './test_tokenizer.mjs';
import * as TokErrorTests from './test_tokenizer_errors.mjs';
import * as InterpTests from './test_interpreter.mjs';
import * as GlobalModTests from './test_global_module.mjs';
import { run_tests } from './utils.mjs';

run_tests([
    TokTests.tests,
    TokErrorTests.tests,
    InterpTests.tests,
    GlobalModTests.tests,
]).then(done, error)

function done() {
    console.log("DONE")
}

function error() {
    console.error("ERROR")
}

