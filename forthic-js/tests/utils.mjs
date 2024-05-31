// Runs a single tests object or an array of them
export async function run_tests(tests) {
    let num_fail = 0, num_pass = 0;

    let tests_array = null;
    if (Array.isArray(tests)) {
        tests_array = tests;
    }
    else {
        tests_array = [tests];
    }

    function passed() {
        num_pass++;
    }

    function failed(key) {
        num_fail++;
        console.error("FAILED: " + key);
    }

    function handle_exception(key, e) {
        num_fail++;
        console.error("FAILED: " + key + ", with exception:", e);
    }

    for (let i=0; i <tests_array.length; i++) {
        let tests = tests_array[i];
        let test_keys = Object.keys(tests);
        for (let j=0; j < test_keys.length; j++) {
            let key = test_keys[j];
            let test = tests[key];
            try {
                if (await test())   passed();
                else                failed(key);
            }
            catch(e) {
                console.log("Got an exception");
                handle_exception(key, e);
            }
        }
    }
    console.log("Num passed: " + num_pass + ", Num failed: " + num_fail);
}

export function stack_top(interp) {
    return interp.stack[interp.stack.length-1];
}

export function assert(value, msg) {
    if (!value) throw (msg);
}

export function arrays_equal(arr1, arr2) {
    if (arr1.length != arr2.length)   return false;
    for (let i=0; i < arr1.length; i++) {
        if (arr1[i] != arr2[i])   return false;
    }
    return true;
}
