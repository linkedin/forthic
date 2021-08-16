import * as HtmlModTests from './tests/modules/test_html_module.mjs';
import { run_tests } from './tests/utils.mjs';

run_tests([
    HtmlModTests.tests,
]).then(done, error)

function done() {
    console.log("DONE")
}

function error() {
    console.error("ERROR")
}

