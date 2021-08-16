import { Interpreter } from '../../forthic-js/interpreter.mjs';
import { SampleDateModule, normalize_date } from './sample_date_module.mjs';
import { Module, ModuleWord } from '../../forthic-js/module.mjs';
import { run_tests } from './utils.mjs';


async function test_initial_state() {
    let interp = new Interpreter();
    if (interp.stack.length != 0)            return false;
    if (interp.module_stack[0].name != "")   return false;

    return true;
}


async function test_push_string() {
    let interp = new Interpreter();
    await interp.run("'Howdy'")
    if (interp.stack[0] != "Howdy")   return false;
    return true;
}

async function test_comment() {
    let interp = new Interpreter();
    await interp.run("# A comment")
    await interp.run("#A comment")
    if (interp.stack.length != 0)   return false;
    return true;
}

async function test_empty_array() {
    let interp = new Interpreter();
    await interp.run("[]")
    let value = interp.stack[0];
    if (!Array.isArray(value) || value.length != 0)   return false;
    return true;
}



async function test_start_module() {
    let interp = new Interpreter();
    await interp.run("{");
    if (interp.module_stack.length != 2)   return false;
    if (interp.module_stack[0] != interp.module_stack[1])   return false;

    interp = new Interpreter();
    await interp.run("{module-A");
    if (interp.module_stack.length != 2)             return false;
    if (interp.module_stack[1].name != "module-A")   return false;
    if (!interp.app_module.modules["module-A"])      return false;

    interp = new Interpreter();
    await interp.run("{module-A {module-B");
    if (interp.module_stack.length != 3)             return false;
    if (interp.module_stack[1].name != "module-A")   return false;
    if (interp.module_stack[2].name != "module-B")   return false;
    let module_A = interp.app_module.modules["module-A"]
    if (!module_A.modules["module-B"])               return false;

    await interp.run("}}");
    if (interp.module_stack.length != 1)             return false;
    if (interp.module_stack[0] != interp.app_module) return false;

    return true;
}


async function test_definition() {
    let interp = new Interpreter();
    await interp.run(": NOTHING   ;");
    let word = interp.app_module.find_word("NOTHING");
    if (!word)   return false;

    interp = new Interpreter();
    await interp.run("{module-A   : NOTHING   ;}");
    word = interp.app_module.find_word("NOTHING")
    if (word)   return false;

    let module_A = interp.app_module.modules["module-A"];
    word = module_A.find_word("NOTHING")
    if (!word)   return false;

    return true;
}

async function test_scope() {
    let interp = new Interpreter();
    await interp.run(`
        : APP-MESSAGE   "Hello (from app)";
        {module1
            APP-MESSAGE
        }
    `);
    if (interp.stack[0] != "Hello (from app)") return false;
    return true;
}

async function test_open_module() {
    let interp = new Interpreter();
    await interp.run(`
        {mymodule
            : MESSAGE   "Hello (from mymodule)";
        }
        : MESSAGE   {mymodule MESSAGE };
        MESSAGE     
    `);
    if (interp.stack[0] != "Hello (from mymodule)") return false;

    // Test memo
    interp = new Interpreter();
    await interp.run(`
        {mymodule
        'MESSAGE-MEMO'   '"Hello (from mymodule memo)"'   MEMO
        }
        : MESSAGE   {mymodule MESSAGE-MEMO };
        MESSAGE
    `);
    if (interp.stack[0] != "Hello (from mymodule memo)") return false;

    return true;
}


async function test_word() {
    let interp = new Interpreter();
    await interp.run(": MESSAGE   'Howdy' ;");
    await interp.run("MESSAGE");
    if (interp.stack[0] != "Howdy")   return false;

    interp = new Interpreter();
    await interp.run("{module-A {module-B   : MESSAGE   'In module-B' ;}}");
    await interp.run("{module-A {module-B   MESSAGE}}");
    if (interp.stack[0] != "In module-B")   return false;

    return true;
}


async function test_search_global_module() {
    let interp = new Interpreter();
    await interp.run("'Hi'");
    if (interp.stack.length != 1)   return false;

    await interp.run("POP");
    if (interp.stack.length != 0)   return false;

    return true;
}

let tests = {
    "test_initial_state": test_initial_state,
    "test_push_string": test_push_string,
    "test_comment": test_comment,
    "test_empty_array": test_empty_array,
    "test_start_module": test_start_module,
    "test_definition": test_definition,
    "test_open_module": test_open_module,
    "test_scope": test_scope,
    "test_word": test_word,
    "test_search_global_module": test_search_global_module,
}

export { tests };
