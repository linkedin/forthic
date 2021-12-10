import { Interpreter } from '../../forthic-js/interpreter.mjs';
import { run_tests, stack_top, assert, arrays_equal } from './utils.mjs';

async function test_literal() {
    let interp = new Interpreter();
    await interp.run("true  2  3.14 2020-06-05 9:00 11:30 PM 22:15 AM")

    if (interp.stack[0] != true)   return false;
    if (interp.stack[1] != 2)      return false;
    if (interp.stack[2] != 3.14)   return false;

    function check_date(date, year, month, day) {
        if (date.getFullYear() != year)    return false;
        if (date.getMonth() + 1 != month)  return false;
        if (date.getDate() != day)         return false;
        return true;
    }

    function check_time(time, hours, minutes) {
        if (time.getHours() != hours)      return false;
        if (time.getMinutes() != minutes)  return false;
        return true;
    }

    if (check_date(interp.stack[3], 2020, 6, 5) == false)   return false;
    if (check_time(interp.stack[4], 9, 0) == false)         return false;
    if (check_time(interp.stack[5], 23, 30) == false)       return false;
    if (check_time(interp.stack[6], 10, 15) == false)       return false;
    return true;
}


async function test_variables() {
    let interp = new Interpreter();
    await interp.run("['x' 'y']  VARIABLES");
    let vars = interp.app_module.variables;
    if (!vars['x'])   return false;
    if (!vars['y'])   return false;
    return true;
}


async function test_set_get_variables() {
    let interp = new Interpreter();
    await interp.run("['x']  VARIABLES");
    await interp.run("24 x !");
    let x_var = interp.app_module.variables['x'];

    if (x_var.get_value() != 24)   return false;

    await interp.run("x @")
    if (interp.stack[0] != 24)     return false;

    return true;
}

async function test_bang_at() {
    let interp = new Interpreter();
    await interp.run("['x']  VARIABLES");
    await interp.run("24 x !@");
    let x_var = interp.app_module.variables['x'];

    if (x_var.get_value() != 24)   return false;
    if (interp.stack[0] != 24)     return false;

    return true;
}

async function test_interpret() {
    let interp = new Interpreter();
    await interp.run("'24' INTERPRET");

    if (interp.stack[0] != 24)   return false;

    await interp.run(`'{module-A  : MESSAGE   "Hi" ;}' INTERPRET`);
    await interp.run("{module-A MESSAGE}");
    if (interp.stack[1] != 'Hi')   return false;
    return true;
}


async function test_memo() {
    let interp = new Interpreter();
    await interp.run(`
    ['count'] VARIABLES
    0 count !
    'COUNT' 'count @ 1 +  count !  count @'   MEMO
    `);

    await interp.run("COUNT");
    if (stack_top(interp) != 1)    return false;

    await interp.run("COUNT");
    if (stack_top(interp) != 1)    return false;

    await interp.run("COUNT! COUNT");
    if (stack_top(interp) != 2)    return false;

    if (interp.stack.length != 3)  return false;

    return true;
}

async function test_rec() {
    let interp = new Interpreter();
    await interp.run(`
    [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
    `)

    assert(interp.stack.length == 1)

    let rec = interp.stack[interp.stack.length-1];
    assert(rec["alpha"] == 2);
    assert(rec["gamma"] == 4);
    return true;
}

async function test_rec_at() {
    let interp = new Interpreter();
    await interp.run(`
    [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
    'beta' REC@
    `)

    assert(interp.stack.length == 1);
    assert(interp.stack[0] == 3);

    return true;
}

async function test_l_rec_bang() {
    let interp = new Interpreter();
    await interp.run(`
    [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
    700 'beta' <REC! 'beta' REC@
    `)

    assert(interp.stack.length == 1);
    assert(interp.stack[0] == 700);

    return true;
}

async function test_load_module() {
    let today = new Date()

    let interp = new Interpreter();
    await interp.run(`
    ['sample_date'] '../tests/tests_js' USE-MODULES
    sample_date.TODAY
    `)
    let stack = interp.stack
    assert(stack[0].getFullYear() == today.getFullYear())
    assert(stack[0].getMonth() == today.getMonth())
    assert(stack[0].getDate() == today.getDate())

    interp = new Interpreter();
    await interp.run(`
    [["sample_date" ""]] "../tests/tests_js" USE-MODULES
    TODAY
    `)
    stack = interp.stack
    assert(stack[0].getFullYear() == today.getFullYear())
    assert(stack[0].getMonth() == today.getMonth())
    assert(stack[0].getDate() == today.getDate())

    interp = new Interpreter();
    await interp.run(`
    [["sample_date" "d"]] "../tests/tests_js" USE-MODULES
    d.TODAY
    `)
    stack = interp.stack
    assert(stack[0].getFullYear() == today.getFullYear())
    assert(stack[0].getMonth() == today.getMonth())
    assert(stack[0].getDate() == today.getDate())

    return true;
}


async function test_append() {
    // Test append to array
    let interp = new Interpreter();
    await interp.run(`
    [ 1 2 3 ] 4 APPEND
    `)
    assert(interp.stack.length == 1, "One array");

    let array = interp.stack[0];
    assert(arrays_equal(array, [1,2,3,4]), "Array vals match")

    // Test append to record
    interp = new Interpreter();
    await interp.run(`
    [["a" 1] ["b" 2]] REC  ["c" 3] APPEND
    `)
    assert(interp.stack.length == 1, "One rec");

    let rec = interp.stack[0];
    let values = ["a", "b", "c"].map(k => rec[k]);
    assert(arrays_equal(values, [1,2,3]), "Rec vals match");

    return true;
}

async function test_reverse() {
    let interp = new Interpreter();
    await interp.run(`
    [ 1 2 3 ] REVERSE
    `);
    assert(interp.stack.length == 1);

    let array = interp.stack[0];
    assert(arrays_equal(array, [3,2,1]))

    // Reverse record (no-op for records)
    interp = new Interpreter();
    await interp.run(`
    [["a" 1] ["b" 2]] REC  REVERSE
    `)
    let rec = interp.stack[0];
    let values = ["a", "b"].map(k => rec[k]);
    assert(arrays_equal(values, [1,2]))

    return true;
}

async function test_unique() {
    let interp = new Interpreter();
    await interp.run(`
    [ 1 2 3 3 2 ] UNIQUE
    `);

    let array = interp.stack[0];
    assert(arrays_equal(array, [1,2,3]))

    interp = new Interpreter();
    await interp.run(`
    [["a" 1] ["b" 2] ["c" 2] ["d" 1]] REC  UNIQUE
    `)
    let rec = interp.stack[0];
    let values = Object.keys(rec).map(k => rec[k]).sort();
    assert(arrays_equal(values, [1,2]));

    return true;
}

async function test_l_del() {
    let interp = new Interpreter();
    await interp.run(`
    [ "a" "b" "c" ] 1 <DEL
    `);

    let array = interp.stack[0]
    assert(arrays_equal(array, ["a", "c"]))

    interp = new Interpreter();
    await interp.run(`
    [["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL
    `);
    let rec = interp.stack[0]
    assert(arrays_equal(Object.keys(rec).sort(), ["a", "c"]));

    return true;
}

async function test_relabel() {
    let interp = new Interpreter();
    await interp.run(`
    [ "a" "b" "c" ] [0 2] [25 23] RELABEL
    `);

    assert(interp.stack.length == 1);
    let array = interp.stack[0];
    assert(arrays_equal(array, ["c", "a"]));

    interp = new Interpreter();
    await interp.run(`
    [["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL
    `);
    assert(interp.stack.length == 1);
    let rec = interp.stack[0];
    assert(arrays_equal(Object.keys(rec).sort(), ["alpha", "gamma"]));
    assert(arrays_equal(["alpha", "gamma"].map(k => rec[k]), [1, 3]));

    return true;
}

function make_records() {
    let data = [ [100, "user1", "OPEN"],
                 [101, "user1", "OPEN"],
                 [102, "user1", "IN PROGRESS"],
                 [103, "user1", "CLOSED"],
                 [104, "user2", "IN PROGRESS"],
                 [105, "user2", "OPEN"],
                 [106, "user2", "CLOSED"],
               ];

    let result = [];
    data.forEach(d => {
        let rec = {"key": d[0], "assignee": d[1], "status": d[2]};
        result.push(rec);
    });
    return result;
}


async function test_group_by_field() {
    let interp = new Interpreter();
    interp.stack_push(make_records());
    await interp.run("'assignee' GROUP-BY-FIELD");
    let grouped = interp.stack[0];
    assert(arrays_equal(Object.keys(grouped).sort(), ["user1", "user2"]));
    assert(grouped["user1"].length == 4)
    assert(grouped["user2"].length == 3)

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec;
    });
    interp.stack_push(by_key)

    // Now group a record
    await interp.run("'assignee' GROUP-BY-FIELD");
    let grouped_rec = interp.stack[0];
    assert(Object.keys(grouped_rec).sort(), ["user1", "user2"]);
    assert(grouped_rec["user1"].length == 4);
    assert(grouped_rec["user2"].length == 3);

    return true;
}


async function test_group_by() {
    let interp = new Interpreter();
    interp.stack_push(make_records());
    await interp.run(`
    "'assignee' REC@" GROUP-BY
    `)
    let grouped = interp.stack[0]
    assert(arrays_equal(Object.keys(grouped).sort(), ["user1", "user2"]));
    assert(grouped["user1"].length == 4)
    assert(grouped["user2"].length == 3)

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec;
    });
    interp.stack_push(by_key)

    // Now group a record
    await interp.run(`
    "'assignee' REC@" GROUP-BY
    `)
    let grouped_rec = interp.stack[0];
    assert(Object.keys(grouped_rec).sort(), ["user1", "user2"]);
    assert(grouped_rec["user1"].length == 4);
    assert(grouped_rec["user2"].length == 3);

    return true;
}


async function test_group_by_w_key() {
    let interp = new Interpreter();
    interp.stack_push(make_records())
    await interp.run(`
    ['key' 'val'] VARIABLES
    "val ! key ! key @ 3 MOD" GROUP-BY-w/KEY
    `)
    let grouped = interp.stack[0]
    assert(arrays_equal(Object.keys(grouped).sort(), [0, 1, 2]));
    assert(grouped[0].length == 3);
    assert(grouped[1].length == 2);
    assert(grouped[2].length == 2);

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec;
    });
    interp.stack_push(by_key)

    // Now group a record
    await interp.run(`
    ['key' 'val'] VARIABLES
    "val ! key ! key @ 2 *" GROUP-BY-w/KEY
    `)
    let grouped_rec = interp.stack[0];
    assert(Object.keys(grouped_rec).sort(), [200, 202, 204, 206, 208, 210, 212]);

    return true;
}


async function test_groups_of() {
    let interp = new Interpreter();
    await interp.run(`
    [1 2 3 4 5 6 7 8] 3 GROUPS-OF
    `)
    let groups = interp.stack[0];
    assert(arrays_equal(groups[0], [1, 2, 3]));
    assert(arrays_equal(groups[1], [4, 5, 6]));
    assert(arrays_equal(groups[2], [7, 8]));

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec;
    });
    interp.stack_push(by_key)


    // Now group a record
    await interp.run("3 GROUPS-OF");
    let recs = interp.stack[0];
    assert(Object.keys(recs[0]).length == 3);
    assert(Object.keys(recs[1]).length == 3);
    assert(Object.keys(recs[2]).length == 1);

    return true;
}

async function test_map() {
    let interp = new Interpreter();
    await interp.run(`
    [1 2 3 4 5] '2 *' MAP
    `)
    let array = interp.stack[0];
    assert(arrays_equal(array, [2, 4, 6, 8, 10]));

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec
    });
    interp.stack_push(by_key);

    await interp.run(`
    "'status' REC@" MAP
    `);
    let record = interp.stack[0];
    assert(record[100] == "OPEN");
    assert(record[102] == "IN PROGRESS");
    assert(record[106] == "CLOSED");

    return true;
}


async function test_map_w_key() {
    let interp = new Interpreter();
    await interp.run(`
    [1 2 3 4 5] '+ 2 *' MAP-w/KEY
    `)
    let array = interp.stack[0];
    assert(arrays_equal(array, [2, 6, 10, 14, 18]));

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec
    });
    interp.stack_push(by_key);

    await interp.run(`
    ["k" "v"] VARIABLES
    "v ! k ! k @ >STR v @ 'status' REC@ CONCAT" MAP-w/KEY
    `);
    let record = interp.stack[0];
    assert(record[100] == "100OPEN");
    assert(record[102] == "102IN PROGRESS");
    assert(record[106] == "106CLOSED");

    return true;
}


async function test_foreach() {
    let interp = new Interpreter();
    await interp.run(`
    0 [1 2 3 4 5] '+' FOREACH
    `);
    let sum = interp.stack[0];
    assert(sum == 15);

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec
    });
    interp.stack_push(by_key);

    await interp.run(`
    "" SWAP "'status' REC@ CONCAT" FOREACH
    `);
    let string = interp.stack[0];
    assert(string == "OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED");

    return true;
}

async function test_foreach_w_key() {
    let interp = new Interpreter();
    await interp.run(`
    0 [1 2 3 4 5] '+ +' FOREACH-w/KEY
    `)
    let sum = interp.stack[0];
    assert(sum == 25);

    // Test grouping a record
    interp = new Interpreter();

    // First, set up the record
    let records = make_records();
    let by_key = {};
    records.forEach(rec => {
        by_key[rec["key"]] = rec
    });
    interp.stack_push(by_key);

    await interp.run(`
    "" SWAP "'status' REC@ CONCAT CONCAT" FOREACH-w/KEY
    `);
    let string = interp.stack[0];
    assert(string == "100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED");

    return true;
}


async function test_zip() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b'] [1 2] ZIP
    `)
    let array = interp.stack[0];
    assert(arrays_equal(array[0], ['a', 1]));
    assert(arrays_equal(array[1], ['b', 2]));

    // Zip a record
    interp = new Interpreter();

    // First, set up the record
    await interp.run(`
    [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
    `)
    let record = interp.stack[0];
    assert(arrays_equal(Object.keys(record).sort(), ['a', 'b', 'z']));
    assert(arrays_equal(record['a'], [100, 'Hi']));
    assert(arrays_equal(record['b'], [200, 'Bye']));
    assert(arrays_equal(record['z'], [300, null]));

    return true;
}

async function test_zip_with() {
    let interp = new Interpreter();
    await interp.run(`
    [10 20] [1 2] "+" ZIP-WITH
    `)
    let array = interp.stack[0]
    assert(array[0] == 11);
    assert(array[1] == 22);

    // Zip a record
    interp = new Interpreter();

    // First, set up the record
    await interp.run(`
    [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+" ZIP-WITH
    `)
    let record = interp.stack[0];
    assert(arrays_equal(Object.keys(record).sort(), ['a', 'b']));
    assert(record['a'] == 11);
    assert(record['b'] == 22);

    return true;
}

async function test_keys() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b' 'c'] KEYS
    `)
    let array = interp.stack[0];
    assert(arrays_equal(array, [0,1,2]));

    // Test record
    interp = new Interpreter();

    // First, set up the record
    await interp.run(`
    [['a' 1] ['b' 2]] REC KEYS
    `)
    array = interp.stack[0];
    assert(arrays_equal(array.sort(), ['a', 'b']));

    return true;
}


async function test_values() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b' 'c'] VALUES
    `)
    let array = interp.stack[0];
    assert(arrays_equal(array, ['a', 'b', 'c']));

    // Test record
    interp = new Interpreter();

    // First, set up the record
    await interp.run(`
    [['a' 1] ['b' 2]] REC VALUES
    `)
    array = interp.stack[0];
    assert(arrays_equal(array.sort(), [1, 2]));

    return true;
}

async function test_length() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b' 'c'] LENGTH
    "Howdy" LENGTH
    `)
    assert(interp.stack[0] == 3);
    assert(interp.stack[1] == 5);

    // Test record
    interp = new Interpreter();

    await interp.run(`
    [['a' 1] ['b' 2]] REC LENGTH
    `)
    assert(interp.stack[0] == 2)

    return true;
}

async function test_slice() {
    let interp = new Interpreter();
    await interp.run(`
    ['x'] VARIABLES
    ['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !
    x @ 0 2 SLICE
    x @ 1 3 SLICE
    x @ 5 3 SLICE
    x @ -1 -2 SLICE
    x @ 4 -2 SLICE
    x @ 5 8 SLICE
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], ['a', 'b', 'c']));
    assert(arrays_equal(stack[1], ['b', 'c', 'd']));
    assert(arrays_equal(stack[2], ['f', 'e', 'd']));
    assert(arrays_equal(stack[3], ['g', 'f']));
    assert(arrays_equal(stack[4], ['e', 'f']));
    assert(arrays_equal(stack[5], ['f', 'g', null, null]));

    // Slice records
    interp = new Interpreter();
    await interp.run(`
    ['x'] VARIABLES
    [['a' 1] ['b' 2] ['c' 3]] REC x !
    x @ 0 1 SLICE
    x @ -1 -2 SLICE
    x @ 5 7 SLICE
    `);
    stack = interp.stack;
    assert(arrays_equal(Object.keys(stack[0]).sort(), ['a', 'b']));
    assert(arrays_equal(Object.keys(stack[1]).sort(), ['b', 'c']));
    assert(Object.keys(stack[2]).length == 0);

    return true;
}

async function test_difference() {
    let interp = new Interpreter();
    await interp.run(`
    ['x' 'y'] VARIABLES
    ['a' 'b' 'c'] x !
    ['a' 'c' 'd'] y !
    x @ y @ DIFFERENCE
    y @ x @ DIFFERENCE
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], ['b']));
    assert(arrays_equal(stack[1], ['d']));

    // Slice records
    interp = new Interpreter();
    await interp.run(`
    ['x' 'y'] VARIABLES
    [['a' 1] ['b' 2] ['c' 3]] REC x !
    [['a' 20] ['c' 40] ['d' 10]] REC y !
    x @ y @ DIFFERENCE
    y @ x @ DIFFERENCE
    `)
    stack = interp.stack
    assert(arrays_equal(Object.keys(stack[0]), ['b']));
    assert(arrays_equal(Object.keys(stack[0]).map(k => stack[0][k]), [2]));
    assert(arrays_equal(Object.keys(stack[1]), ['d']));
    assert(arrays_equal(Object.keys(stack[1]).map(k => stack[1][k]), [10]));

    return true;
}

async function test_select() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [1, 3, 5]));

    // Slice records
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  "2 MOD 0 ==" SELECT
    `)
    stack = interp.stack;
    assert(arrays_equal(Object.keys(stack[0]), ['b']));
    assert(arrays_equal(Object.keys(stack[0]).map(k => stack[0][k]), [2]));

    return true;
}


async function test_select_w_key() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] "+ 3 MOD 1 ==" SELECT-w/KEY
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [2, 5]));

    // Slice records
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  "CONCAT 'c3' ==" SELECT-w/KEY
    `);
    stack = interp.stack;
    assert(arrays_equal(Object.keys(stack[0]), ['c']));
    assert(arrays_equal(Object.keys(stack[0]).map(k => stack[0][k]), [3]));

    return true;
}

async function test_take() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] 3 TAKE
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [3, 4, 5, 6]));
    assert(arrays_equal(stack[1], [0, 1, 2]));

    // Take records
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 TAKE
    `);
    stack = interp.stack;
    assert(stack[0].length == 1)
    assert(stack[1].length == 2)

    return true;
}

async function test_drop() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] 4 DROP
    `)
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [4, 5, 6]));

    // Drop records
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 DROP
    `)
    stack = interp.stack;
    assert(stack[0].length == 1);

    return true;
}

async function test_rotate() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b' 'c' 'd'] ROTATE
    ['b'] ROTATE
    [] ROTATE
`)
    let stack = interp.stack;
    assert(arrays_equal(stack[0], ['d', 'a', 'b', 'c']));
    assert(arrays_equal(stack[1], ['b']));
    assert(arrays_equal(stack[2], []));
    return true;
}

async function test_rotate_element() {
    let interp = new Interpreter();
    await interp.run(`
    ['a' 'b' 'c' 'd'] 'c' ROTATE-ELEMENT
    ['a' 'b' 'c' 'd'] 'x' ROTATE-ELEMENT
`)
    let stack = interp.stack;
    assert(arrays_equal(stack[0], ['c', 'a', 'b', 'd']));
    assert(arrays_equal(stack[1], ['a', 'b', 'c', 'd']));

    return true;
}

async function test_shuffle() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] SHUFFLE
    `);
    let stack = interp.stack;
    assert(stack[0].length == 7);

    // Shuffle record (no-op)
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  SHUFFLE
    `)
    stack = interp.stack;
    assert(Object.keys(stack[0]).length == 3);

    return true;
}

async function test_sort() {
    let interp = new Interpreter();
    await interp.run(`
    [2 8 1 4 7 3] SORT
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [1, 2, 3, 4, 7, 8]));

    // Sort record (no-op)
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  SORT
    `)
    stack = interp.stack;
    assert(Object.keys(stack[0]).length == 3);

    return true;
}

async function test_sort_w_forthic() {
    let interp = new Interpreter();
    await interp.run(`
    [2 8 1 4 7 3] "-1 *" SORT-w/FORTHIC
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [8, 7, 4, 3, 2, 1]));

    // Sort record (no-op)
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  SORT
    `);
    stack = interp.stack;
    assert(Object.keys(stack[0]).length == 3)

    return true;
}


async function test_sort_w_key_func() {
    let interp = new Interpreter();
    interp.stack_push(make_records())
    await interp.run(`
    'status' FIELD-KEY-FUNC SORT-w/KEY-FUNC
    `);
    let stack = interp.stack;
    assert(stack[0][0]["status"] == "CLOSED")
    assert(stack[0][1]["status"] == "CLOSED")
    assert(stack[0][2]["status"] == "IN PROGRESS")
    assert(stack[0][3]["status"] == "IN PROGRESS")
    assert(stack[0][4]["status"] == "OPEN")
    assert(stack[0][5]["status"] == "OPEN")
    assert(stack[0][6]["status"] == "OPEN")

    // Sort record (no-op)
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC NULL SORT-w/KEY-FUNC
    `)
    stack = interp.stack;
    assert(Object.keys(stack[0]).length == 3)

    return true;
}

async function test_nth() {
    let interp = new Interpreter();
    await interp.run(`
    ["x"] VARIABLES
    [0 1 2 3 4 5 6] x !
    x @ 0 NTH
    x @ 5 NTH
    x @ 55 NTH
    `);
    let stack = interp.stack;
    assert(stack[0] == 0)
    assert(stack[1] == 5)
    assert(stack[2] == null)

    // For record
    interp = new Interpreter();
    await interp.run(`
    ["x"] VARIABLES
    [['a' 1] ['b' 2] ['c' 3]] REC  x !
    x @ 0 NTH
    x @ 2 NTH
    x @ 55 NTH
    `);
    stack = interp.stack
    assert(stack[0] == 1)
    assert(stack[1] == 3)
    assert(stack[2] == null)

    return true;
}

async function test_last() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2 3 4 5 6] LAST
    `);
    let stack = interp.stack;
    assert(stack[0] == 6);

    // For record
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  LAST
    `)
    stack = interp.stack;
    assert(stack[0] == 3);

    return true;
}

async function test_unpack() {
    let interp = new Interpreter();
    await interp.run(`
    [0 1 2] UNPACK
    `);
    let stack = interp.stack;
    assert(stack[0] == 0);
    assert(stack[1] == 1);
    assert(stack[2] == 2);

    // For record
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC UNPACK
    `);
    stack = interp.stack;
    assert(stack[0] == 1);
    assert(stack[1] == 2);
    assert(stack[2] == 3);

    return true;
}

async function test_flatten() {
    let interp = new Interpreter();
    await interp.run(`
    [0 [1 2 [3 [4]] ]] FLATTEN
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], [0, 1, 2, 3, 4]));

    // For record
    interp = new Interpreter();
    await interp.run(`
    ['uno' 'alpha'] VARIABLES
    [['uno' 4] ['duo' 8]] REC uno !
    [['alpha' uno @]] REC alpha !
    [['a' 1] ['b' alpha @] ['c' 3]] REC FLATTEN
    `);
    stack = interp.stack;
    let record = stack[0];
    assert(arrays_equal(Object.keys(record).sort(), ['a', 'b\talpha\tduo', 'b\talpha\tuno', 'c']));
    return true;
}

async function test_key_of() {
    let interp = new Interpreter();
    await interp.run(`
    ['x'] VARIABLES
    ['a' 'b' 'c' 'd'] x !
    x @  'c' KEY-OF
    x @  'z' KEY-OF
    `);
    let stack = interp.stack;
    assert(stack[0] == 2);
    assert(stack[1] == null);

    // For record
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  LAST
    `);
    stack = interp.stack;
    assert(stack[0] == 3);

    return true;
}

async function test_reduce() {
    let interp = new Interpreter();
    await interp.run(`
    [1 2 3 4 5] 10 "+" REDUCE
    `);
    let stack = interp.stack;
    assert(stack[0] == 25);

    // For record
    interp = new Interpreter();
    await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  20 "+" REDUCE
    `);
    stack = interp.stack;
    assert(stack[0] == 26);

    return true;
}

async function test_pop() {
    let interp = new Interpreter();
    await interp.run(`
    1 2 3 4 5 POP
    `)
    let stack = interp.stack;
    assert(stack.length == 4);
    assert(stack[3] == 4)
    return true;
}

async function test_dup() {
    let interp = new Interpreter()
    await interp.run(`
    5 DUP
    `)
    let stack = interp.stack
    assert(stack.length == 2)
    assert(stack[0] == 5)
    assert(stack[1] == 5)
    return true;
}

async function test_swap() {
    let interp = new Interpreter()
    await interp.run(`
    6 8 SWAP
    `)
    let stack = interp.stack
    assert(stack.length == 2)
    assert(stack[0] == 8)
    assert(stack[1] == 6)
    return true;
}

async function test_split() {
    let interp = new Interpreter()
    await interp.run(`
    'Now is the time' ' ' SPLIT
    `)
    let stack = interp.stack
    assert(stack.length == 1)
    assert(arrays_equal(stack[0], ["Now", "is", "the", "time"]));
    return true;
}


async function test_join() {
    let interp = new Interpreter()
    await interp.run(`
    ["Now" "is" "the" "time"] "--" JOIN
    `)
    let stack = interp.stack
    assert(stack.length == 1)
    assert(stack[0] == "Now--is--the--time")
    return true;
}

async function test_special_chars() {
    let interp = new Interpreter()
    await interp.run(`
    /R /N /T
    `)
    let stack = interp.stack
    assert(stack[0] == "\r")
    assert(stack[1] == "\n")
    assert(stack[2] == "\t")
    return true;
}

async function test_pipe_lower() {
    let interp = new Interpreter()
    await interp.run(`
    "HOWDY, Everyone!" |LOWER
    `)
    let stack = interp.stack;
    assert(stack[0] == "howdy, everyone!")
    return true;
}


async function test_pipe_ascii() {
    let interp = new Interpreter()
    await interp.run(`
    "“HOWDY, Everyone!”" |ASCII
    `)
    let stack = interp.stack
    assert(stack[0] == "HOWDY, Everyone!")
    return true;
}

async function test_strip() {
    let interp = new Interpreter()
    await interp.run(`
    "  howdy  " STRIP
    `)
    let stack = interp.stack
    assert(stack[0] == "howdy")
    return true;
}

async function test_replace() {
    let interp = new Interpreter()
    await interp.run(`
    "1-40 2-20" "-" "." REPLACE
    `)
    let stack = interp.stack
    assert(stack[0] == "1.40 2.20")
    return true;
}

async function test_match() {
    let interp = new Interpreter()
    await interp.run(`
    "123message456" "\\d{3}.*\\d{3}" RE-MATCH
    `)
    let stack = interp.stack
    assert(stack[0])
    return true;
}

async function test_match_group() {
    let interp = new Interpreter()
    await interp.run(`
    "123message456" "\\d{3}(.*)\\d{3}" RE-MATCH 1 RE-MATCH-GROUP
    `)
    let stack = interp.stack
    assert(stack[0] == "message")
    return true;
}

async function test_match_all() {
    let interp = new Interpreter()
    await interp.run(`
    "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL
    `)
    let stack = interp.stack
    assert(arrays_equal(stack[0], ['android', 'ios', 'android', 'web', 'web']), "Array vals match")

    return true;
}

async function test_default() {
    let interp = new Interpreter()
    await interp.run(`
    NULL 22.4 DEFAULT
    0 22.4 DEFAULT
    "" "Howdy" DEFAULT
    `)
    let stack = interp.stack
    assert(stack[0] == 22.4)
    assert(stack[1] == 0)
    assert(stack[2] == "Howdy")
    return true;
}

async function test_l_repeat() {
    let interp = new Interpreter()
    await interp.run(`
    [0 "1 +" 6 <REPEAT]
    `)
    let stack = interp.stack
    assert(arrays_equal(stack[0], [0, 1, 2, 3, 4, 5, 6]))
    return true;
}

async function test_to_fixed() {
    let interp = new Interpreter()
    await interp.run(`
    22 7 / 2 >FIXED
    `)
    let stack = interp.stack
    assert(stack[0] == "3.14")
    return true;
}


async function test_to_json() {
    let interp = new Interpreter()
    await interp.run(`
    [["a" 1] ["b" 2]] REC >JSON
    `)
    let stack = interp.stack
    assert(stack[0] == '{"a":1,"b":2}')
    return true;
}

async function test_json_to() {
    let interp = new Interpreter()
    await interp.run(`
    '{"a": 1, "b": 2}' JSON>
    `)
    let stack = interp.stack
    assert(arrays_equal(Object.keys(stack[0]).sort(), ['a', 'b']))
    assert(stack[0]['a'] == 1)
    assert(stack[0]['b'] == 2)
    return true;
}

async function test_quoted() {
    let DLE = String.fromCharCode(16);

    let interp = new Interpreter()
    await interp.run(`
    "howdy" QUOTED
    "sinister${DLE}INJECT-BADNESS" QUOTED
    `)
    let stack = interp.stack
    assert(stack[0] == `${DLE}howdy${DLE}`)
    assert(stack[1] == `${DLE}sinister INJECT-BADNESS${DLE}`)
    return true;
}


async function test_now() {
    let now = new Date();
    let interp = new Interpreter()
    await interp.run("NOW")
    let stack = interp.stack
    assert(stack[0].getHours() == now.getHours())
    assert(stack[0].getMinutes() == now.getMinutes())
    return true;
}

async function test_to_time() {
    let interp = new Interpreter()
    await interp.run("'10:52 PM' >TIME")
    let stack = interp.stack
    assert(stack[0].getHours() == 22)
    assert(stack[0].getMinutes() == 52)
    return true;
}

async function test_time_to_str() {
    let interp = new Interpreter()
    await interp.run(`
    '10:52 AM' >TIME TIME>STR
    `)
    let stack = interp.stack
    assert(stack[0] == "10:52")
    return true;
}


async function test_to_date() {
    let interp = new Interpreter()
    await interp.run(`
    "Oct 21, 2020" >DATE
    `)
    let stack = interp.stack
    assert(stack[0].getFullYear() == 2020)
    assert(stack[0].getMonth() == 9)
    assert(stack[0].getDate() == 21)
    return true;
}

async function test_today() {
    let interp = new Interpreter()
    await interp.run(`
    TODAY
    `)
    let today = new Date()
    let stack = interp.stack
    assert(stack[0].getFullYear() == today.getFullYear())
    assert(stack[0].getMonth() == today.getMonth())
    assert(stack[0].getDate() == today.getDate())
    return true;
}

async function test_days_of_week() {
    let today = new Date();
    let interp = new Interpreter()
    await interp.run(`
    MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY
    `)
    let stack = interp.stack
    assert(stack[0] <= today)
    assert(stack[6] >= today)
    return true;
}

async function test_add_days() {
    let interp = new Interpreter()
    await interp.run(`
    2020-10-21 12 +DAYS
    `)
    let stack = interp.stack
    assert(stack[0].getFullYear() == 2020)
    assert(stack[0].getMonth() == 10)
    assert(stack[0].getDate() == 2)
    return true;
}

async function test_subtract_dates() {
    let interp = new Interpreter()
    await interp.run(`
    2020-10-21 2020-11-02 SUBTRACT-DATES
    `)
    let stack = interp.stack
    return true;
    assert(stack[0] == -12)
    return true;
}

async function test_date_to_str() {
    let interp = new Interpreter()
    await interp.run(`
    2020-11-02 DATE>STR
    `)
    let stack = interp.stack
    return true;
}


async function test_date_time_to_datetime() {
    let interp = new Interpreter()
    await interp.run(`
    2020-11-02 10:25 PM DATE-TIME>DATETIME
    2020-11-02 10:25 PM DATE-TIME>DATETIME >DATE
    2020-11-02 10:25 PM DATE-TIME>DATETIME >TIME
    `)
    let stack = interp.stack
    assert(stack[0].getFullYear() == 2020)
    assert(stack[0].getMonth() == 10)
    assert(stack[0].getDate() == 2)
    assert(stack[0].getHours() == 22)
    assert(stack[0].getMinutes() == 25)
    assert(stack[1].getFullYear() == 2020)
    assert(stack[1].getMonth() == 10)
    assert(stack[1].getDate() == 2)
    assert(stack[2].getHours() == 22)
    assert(stack[2].getMinutes() == 25)
    return true;
}

async function test_datetime_to_timestamp() {
    let interp = new Interpreter()
    await interp.run(`
    2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
    `)
    let stack = interp.stack
    assert(stack[0] == 1593642000)
    return true;
}

async function test_timestamp_to_datetime() {
    let interp = new Interpreter()
    await interp.run(`
    1593895532 TIMESTAMP>DATETIME
    `)
    let stack = interp.stack
    assert(stack[0].getFullYear() == 2020)
    assert(stack[0].getMonth() == 6)
    assert(stack[0].getDate() == 4)
    assert(stack[0].getHours() == 13)
    assert(stack[0].getMinutes() == 45)
    return true;
}

async function test_arithmetic() {
    let interp = new Interpreter()
    await interp.run(`
    2 4 +
    2 4 -
    2 4 *
    2 4 /
    5 3 MOD
    2.5 ROUND
    [1 2 3] +
    `)
    let stack = interp.stack
    assert(stack[0] == 6)
    assert(stack[1] == -2)
    assert(stack[2] == 8)
    assert(stack[3] == 0.5)
    assert(stack[4] == 2)
    assert(stack[5] == 3)
    assert(stack[6] == 6)
    return true;
}

async function test_comparison() {
    let interp = new Interpreter()
    await interp.run(`
    2 4 ==
    2 4 !=
    2 4 <
    2 4 <=
    2 4 >
    2 4 >=
    `)
    let stack = interp.stack
    assert(stack[0] == false)
    assert(stack[1] == true)
    assert(stack[2] == true)
    assert(stack[3] == true)
    assert(stack[4] == false)
    assert(stack[5] == false)
    return true;
}

async function test_logic() {
    let interp = new Interpreter()
    await interp.run(`
    FALSE FALSE OR
    [FALSE FALSE TRUE FALSE] OR
    FALSE TRUE AND
    [FALSE FALSE TRUE FALSE] AND
    FALSE NOT
    `)
    let stack = interp.stack
    assert(stack[0] == false)
    assert(stack[1] == true)
    assert(stack[2] == false)
    assert(stack[3] == false)
    assert(stack[4] == true)
    return true;
}

async function test_in() {
    let interp = new Interpreter()
    await interp.run(`
    "alpha" ["beta" "gamma"] IN
    "alpha" ["beta" "gamma" "alpha"] IN
    `)
    let stack = interp.stack
    assert(stack[0] == false)
    assert(stack[1] == true)
    return true;
}

async function test_any() {
    let interp = new Interpreter()
    await interp.run(`
    ["alpha" "beta"] ["beta" "gamma"] ANY
    ["delta" "beta"] ["gamma" "alpha"] ANY
    ["alpha" "beta"] [] ANY
    `)
    let stack = interp.stack
    assert(stack[0] == true)
    assert(stack[1] == false)
    assert(stack[2] == true)
    return true;
}

async function test_all() {
    let interp = new Interpreter()
    await interp.run(`
    ["alpha" "beta"] ["beta" "gamma"] ALL
    ["delta" "beta"] ["beta"] ALL
    ["alpha" "beta"] [] ALL
        `)
    let stack = interp.stack
    assert(stack[0] == false)
    assert(stack[1] == true)
    assert(stack[2] == true)
    return true;
}

async function test_math_converters() {
    let interp = new Interpreter()
    await interp.run(`
    NULL >BOOL
    0 >BOOL
    1 >BOOL
    "" >BOOL
    "Hi" >BOOL
    "3" >INT
    4 >INT
    4.6 >INT
    "1.2" >FLOAT
    2 >FLOAT
    `)
    let stack = interp.stack
    return true;
    assert(stack[0] == false)
    assert(stack[1] == false)
    assert(stack[2] == true)
    assert(stack[3] == false)
    assert(stack[4] == true)
    assert(stack[5] == 3)
    assert(stack[6] == 4)
    assert(stack[7] == 4)
    assert(stack[8] == 1.2)
    assert(stack[9] == 2.0)
    return true;
}

async function test_profiling() {
    let interp = new Interpreter()
    await interp.run(`
    PROFILE-START
    [0 "1 + 0 +" 6 <REPEAT]
    PROFILE-END
    PROFILE-DATA
    `)
    let stack = interp.stack
    let profile_data = stack[1]
    assert(profile_data["word_counts"][0]["word"] == "+")
    assert(profile_data["word_counts"][0]["count"] == 12)
    return true;
}



let tests = {
    "test_literal": test_literal,
    "test_variables": test_variables,
    "test_set_get_variables": test_set_get_variables,
    "test_bang_at": test_bang_at,
    "test_interpret": test_interpret,
    "test_memo": test_memo,
    "test_rec": test_rec,
    "test_rec_at": test_rec_at,
    "test_l_rec_bang": test_l_rec_bang,
    "test_load_module": test_load_module,

    // Array/Record words
    "test_append": test_append,
    "test_reverse": test_reverse,
    "test_unique": test_unique,
    "test_l_del": test_l_del,
    "test_relabel": test_relabel,
    "test_group_by_field": test_group_by_field,
    "test_group_by": test_group_by,
    "test_group_by_w_key": test_group_by_w_key,
    "test_groups_of": test_groups_of,
    "test_map": test_map,
    "test_map_w_key": test_map_w_key,
    "test_foreach": test_foreach,
    "test_foreach_w_key": test_foreach_w_key,
    "test_zip": test_zip,
    "test_zip_with": test_zip_with,
    "test_keys": test_keys,
    "test_values": test_values,
    "test_length": test_length,
    "test_slice": test_slice,
    "test_difference": test_difference,
    "test_select": test_select,
    "test_select_w_key": test_select_w_key,

    "test_take": test_take,
    "test_drop": test_drop,
    "test_rotate": test_rotate,
    "test_rotate_element": test_rotate_element,
    "test_shuffle": test_shuffle,
    "test_sort": test_sort,
    "test_sort_w_forthic": test_sort_w_forthic,
    "test_sort_w_key_func": test_sort_w_key_func,
    "test_nth": test_nth,
    "test_last": test_last,
    "test_unpack": test_unpack,
    "test_flatten": test_flatten,
    "test_key_of": test_key_of,
    "test_reduce": test_reduce,

    // Stack words
    "test_pop": test_pop,
    "test_dup": test_dup,
    "test_swap": test_swap,

    // String words
    "test_split": test_split,
    "test_join": test_join,
    "test_special_chars": test_special_chars,
    "test_pipe_lower": test_pipe_lower,
    "test_pipe_ascii": test_pipe_ascii,
    "test_strip": test_strip,
    "test_replace": test_replace,
    "test_match": test_match,
    "test_match_group": test_match_group,
    "test_match_all": test_match_all,

    // Misc words
    "test_default": test_default,
    "test_l_repeat": test_l_repeat,
    "test_to_fixed": test_to_fixed,
    "test_to_json": test_to_json,
    "test_json_to": test_json_to,
    "test_quoted": test_quoted,

    // Date/time words
    "test_now": test_now,
    "test_to_time": test_to_time,
    "test_time_to_str": test_time_to_str,
    "test_to_date": test_to_date,
    "test_today": test_today,
    "test_days_of_week": test_days_of_week,
    "test_add_days": test_add_days,
    "test_subtract_dates": test_subtract_dates,
    "test_date_to_str": test_date_to_str,
    "test_date_time_to_datetime": test_date_time_to_datetime,
    "test_datetime_to_timestamp": test_datetime_to_timestamp,
    "test_timestamp_to_datetime": test_timestamp_to_datetime,

    // Math words
    "test_arithmetic": test_arithmetic,
    "test_comparison": test_comparison,
    "test_logic": test_logic,
    "test_in": test_in,
    "test_any": test_any,
    "test_all": test_all,
    "test_math_converters": test_math_converters,

    // Profiling words
    "test_profiling": test_profiling,
}

export { tests };
