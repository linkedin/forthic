import { Interpreter } from "../interpreter";
import {
  InvalidVariableNameError,
  UnknownWordError,
  UnknownScreenError,
  UnknownModuleError,
  StackUnderflowError,
  MissingSemicolonError,
  ExtraSemicolonError,
  WordExecutionError
} from "../errors";
import { Temporal } from "temporal-polyfill";

let interp: Interpreter;
let interp_any: any;

beforeEach(async () => {
  interp = new Interpreter([], "America/Los_Angeles");
  interp_any = interp as any;
});

test("Literal values", async () => {
  await interp.run("TRUE FALSE 2  3.14 2020-06-05");
  expect(interp_any.stack[0]).toBe(true);
  expect(interp_any.stack[1]).toBe(false);
  expect(interp_any.stack[2]).toBe(2);
  expect(interp_any.stack[3]).toBe(3.14);
  expect(interp_any.stack[4]).toEqual(Temporal.PlainDate.from({
      year: 2020,
      month: 6,
      day: 5,
    }),
  );
});

test("Literal time values", async () => {
  await interp.run("9:00");
  const date = interp.stack_pop();
  expect(date).toEqual(Temporal.PlainTime.from({
    hour: 9,
    minute: 0,
  }));

  await interp.run("11:30 PM");
  const date2 = interp.stack_pop();
  expect(date2).toEqual(Temporal.PlainTime.from({
    hour: 23,
    minute: 30,
  }));

  await interp.run("22:15 AM");
  const date3 = interp.stack_pop();
  expect(date3).toEqual(Temporal.PlainTime.from({
    hour: 10,
    minute: 15,
  }));
});

test("Literal datetime values", async () => {
  // Check that Z is interpreted as UTC
  await interp.run("2025-05-24T10:15:00Z");
  const datetime = interp.stack_pop();
  const expected1 = Temporal.ZonedDateTime.from({
    year: 2025,
    month: 5,
    day: 24,
    hour: 10,
    minute: 15,
    second: 0,
    millisecond: 0,
    timeZone: "UTC",
  });
  expect(datetime.toInstant().toString()).toEqual(expected1.toInstant().toString());

  // Check that explicit offsets are interpreted as the given timezone
  await interp.run("2025-05-24T10:15:00-05:00");
  const datetime2 = interp.stack_pop();
  const expected2 = Temporal.ZonedDateTime.from({
    year: 2025,
    month: 5,
    day: 24,
    hour: 15,
    minute: 15,
    second: 0,
    millisecond: 0,
    timeZone: "UTC",
  });
  expect(datetime2.toInstant().toString()).toEqual(expected2.toInstant().toString());

  // Check that a datetime with no timezone is interpreted in the current timezone
  await interp.run("2025-05-24T10:15:00");
  const datetime3 = interp.stack_pop();
  const expected3 = Temporal.ZonedDateTime.from({
    year: 2025,
    month: 5,
    day: 24,
    hour: 17,
    minute: 15,
    second: 0,
    millisecond: 0,
    timeZone: "UTC",
  });
  expect(datetime3.toInstant().toString()).toEqual(expected3.toInstant().toString());
});

test("Variables", async () => {
  await interp.run("['x' 'y']  VARIABLES");
  const variables = (interp as any).app_module.variables;
  expect(variables["x"]).not.toBeNull();
  expect(variables["y"]).not.toBeNull();
});

test("Invalid variable name", async () => {
  try {
    await interp.run("['__test'] VARIABLES");
  } catch (e) {
    expect(e).toBeInstanceOf(WordExecutionError);
    const root_error = e.getError();
    expect(root_error).toBeInstanceOf(InvalidVariableNameError);
    expect(root_error.getVarName()).toBe("__test");
  }
});


test("Set and get variables", async () => {
  await interp.run("['x']  VARIABLES");
  await interp.run("24 x !");
  const x_var = (interp as any).app_module.variables["x"];
  expect(x_var.get_value()).toBe(24);
  await interp.run("x @");
  expect(interp.stack_pop()).toBe(24);
});

test("Bang at (!@)", async () => {
  await interp.run("['x']  VARIABLES");
  await interp.run("24 x !@");
  const x_var = (interp as any).app_module.variables["x"];
  expect(x_var.get_value()).toBe(24);
  expect(interp.stack_pop()).toBe(24);
});

test("Auto-create variables with string names", async () => {
  // Test ! with string variable name (auto-creates variable)
  await interp.run('"hello" "autovar1" !');
  await interp.run('autovar1 @');
  expect(interp.stack_pop()).toBe("hello");
  
  // Verify variable was created in app module
  const autovar1 = (interp as any).app_module.variables["autovar1"];
  expect(autovar1).toBeDefined();
  expect(autovar1.get_value()).toBe("hello");
  
  // Test @ with string variable name (auto-creates with null)
  await interp.run('"autovar2" @');
  expect(interp.stack_pop()).toBe(null);
  
  // Verify variable was created
  const autovar2 = (interp as any).app_module.variables["autovar2"];
  expect(autovar2).toBeDefined();
  expect(autovar2.get_value()).toBe(null);
  
  // Test !@ with string variable name (auto-creates and returns value)
  await interp.run('"world" "autovar3" !@');
  expect(interp.stack_pop()).toBe("world");
  
  // Verify variable was created with correct value
  const autovar3 = (interp as any).app_module.variables["autovar3"];
  expect(autovar3).toBeDefined();
  expect(autovar3.get_value()).toBe("world");
  
  // Test that existing variables still work with strings
  await interp.run('"updated" "autovar1" !');
  await interp.run('"autovar1" @');
  expect(interp.stack_pop()).toBe("updated");
});

test("Auto-create variables validation", async () => {
  // Test that __ prefix variables are rejected
  expect(async () => {
    await interp.run('"value" "__invalid" !');
  }).rejects.toThrow();
  
  // Test that validation works for @ as well
  expect(async () => {
    await interp.run('"__invalid2" @');
  }).rejects.toThrow();
  
  // Test that validation works for !@ as well
  expect(async () => {
    await interp.run('"value" "__invalid3" !@');
  }).rejects.toThrow();
});

test("Interpret", async () => {
  await interp.run("'24' INTERPRET");
  expect(interp.stack_pop()).toBe(24);

  await interp.run(`'{module-A  : MESSAGE   "Hi" ;}' INTERPRET`);
  await interp.run("{module-A MESSAGE}");
  expect(interp.stack_pop()).toBe("Hi");
});

test("module_id", async () => {
  const global_module = (interp as any).global_module;
  expect(global_module.module_id).toMatch(/<GLOBAL>-\d*/);
});

// NOTE: This is just to exercise the date functions
test("Dates", async () => {
  // await interp.run("TODAY");
  // console.log("TODAY", interp.stack_pop());
  // await interp.run("SATURDAY");
  // console.log(interp.stack_pop());
});

test("DATE>STR", async () => {
  await interp.run(`2021-01-01 DATE>STR`);
  expect(interp.stack_pop()).toEqual("2021-01-01");
});

test("REC", async () => {
  const interp = new Interpreter();
  await interp.run(`
    [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
  `);

  expect((interp as any).stack.length).toBe(1);

  const rec = interp.stack_pop();
  expect(rec["alpha"]).toBe(2);
  expect(rec["gamma"]).toBe(4);
});

test("REC@", async () => {
  const interp = new Interpreter();
  await interp.run(`
    [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
    'beta' REC@
  `);
  expect((interp as any).stack.length).toBe(1);
  expect(interp.stack_pop()).toBe(3);

  await interp.run(`
    [10 20 30 40 50] 3 REC@
  `);
  expect(interp.stack_pop()).toBe(40);
});

test("Nested REC@", async () => {
  const interp = new Interpreter();
  await interp.run(`
    [ ["alpha" [["alpha1" 20]] REC]
      ["beta" [["beta1"  30]] REC]
    ] REC
    ["beta" "beta1"] REC@
  `);
  expect(interp.stack_pop()).toBe(30);

  await interp.run(`
    [ [] [] [[3]] ]
    [2 0 0] REC@
  `);
  expect(interp.stack_pop()).toBe(3);

  await interp.run(`
    [ ["alpha" [["alpha1" 20]] REC]
      ["beta" [["beta1"  [10 20 30]]] REC]
    ] REC
    ["beta" "beta1" 1] REC@
  `);
  expect(interp.stack_pop()).toBe(20);
});

test("REC!", async () => {
  // Case: Set value on a record
  let interp = new Interpreter();
  await interp.run(`
    [["alpha" 2] ["beta" 3] ["gamma" 4]] REC
    700 'beta' <REC! 'beta' REC@
  `);
  expect((interp as any).stack.length).toBe(1);
  expect(interp.stack_pop()).toBe(700);

  // Case: Set a nested value
  interp = new Interpreter();
  await interp.run(`
    [] REC "Green" ["2021-03-22" "TEST-1234"] <REC! ["2021-03-22" "TEST-1234"] REC@
  `);
  expect((interp as any).stack.length).toBe(1);
  expect(interp.stack_pop()).toBe("Green");

  // Case: Set value on a NULL
  interp = new Interpreter();
  await interp.run(`
    NULL 700 'beta' <REC! 'beta' REC@
  `);
  expect((interp as any).stack.length).toBe(1);
  expect(interp.stack_pop()).toBe(700);
});

test("Append", async () => {
  // Test append to array
  await interp.run(`
    [ 1 2 3 ] 4 APPEND
  `);
  expect((interp as any).stack.length).toBe(1);

  const array = interp.stack_pop();
  expect(array).toEqual([1, 2, 3, 4]);

  // Test append to record
  await interp.run(`
    [["a" 1] ["b" 2]] REC  ["c" 3] APPEND
  `);
  expect((interp as any).stack.length).toBe(1);

  const rec = interp.stack_pop();
  const values = ["a", "b", "c"].map((k) => rec[k]);
  expect(values).toEqual([1, 2, 3]);
});

test("Reverse", async () => {
  await interp.run(`
    [ 1 2 3 ] REVERSE
  `);
  expect((interp as any).stack.length).toBe(1);

  expect(interp.stack_pop()).toEqual([3, 2, 1]);
});

test("Unique", async () => {
  await interp.run(`
    [ 1 2 3 3 2 ] UNIQUE
  `);
  expect(interp.stack_pop()).toEqual([1, 2, 3]);
});

test("Delete", async () => {
  await interp.run(`
    [ "a" "b" "c" ] 1 <DEL
  `);

  expect(interp.stack_pop()).toEqual(["a", "c"]);

  await interp.run(`
    [["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL
  `);
  let rec = interp.stack_pop();
  expect(Object.keys(rec).sort()).toEqual(["a", "c"]);

  await interp.run(`
    [["a" 1] ["b" 2] ["c" 3]] REC  "d" <DEL
  `);
  rec = interp.stack_pop();
  expect(Object.keys(rec).sort()).toEqual(["a", "b", "c"]);
});

test("Relabel", async () => {
  let interp = new Interpreter();
  await interp.run(`
    [ "a" "b" "c" ] [0 2] [25 23] RELABEL
  `);

  expect((interp as any).stack.length).toBe(1);
  const array = interp.stack_pop();
  expect(array).toEqual(["c", "a"]);

  interp = new Interpreter();
  await interp.run(`
    [["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL
  `);
  expect((interp as any).stack.length).toBe(1);
  const rec = interp.stack_pop();
  expect(Object.keys(rec).sort()).toEqual(["alpha", "gamma"]);
  expect(["alpha", "gamma"].map((k) => rec[k])).toEqual([1, 3]);
});

function makeRecords() {
  return [
    { key: 100, assignee: "user1", status: "OPEN" },
    { key: 101, assignee: "user1", status: "OPEN" },
    { key: 102, assignee: "user1", status: "IN PROGRESS" },
    { key: 103, assignee: "user1", status: "CLOSED" },
    { key: 104, assignee: "user2", status: "IN PROGRESS" },
    { key: 105, assignee: "user2", status: "OPEN" },
    { key: 106, assignee: "user2", status: "CLOSED" },
  ];
}

test("By field", async () => {
  interp.stack_push(makeRecords());
  await interp.run("'key' BY-FIELD");
  const grouped = interp.stack_pop();
  expect(grouped[104].status).toBe("IN PROGRESS");
});

test("By field with nulls", async () => {
  interp.stack_push([...makeRecords(), null, null]);
  await interp.run("'key' BY-FIELD");
  const grouped = interp.stack_pop();
  expect(grouped[104].status).toBe("IN PROGRESS");
});

test("Group by field", async () => {
  interp.stack_push(makeRecords());
  await interp.run("'assignee' GROUP-BY-FIELD");
  const grouped = interp.stack_pop();
  expect(Object.keys(grouped).sort()).toEqual(["user1", "user2"]);
  expect(grouped["user1"].length).toBe(4);
  expect(grouped["user2"].length).toBe(3);

  // Test grouping a record
  interp = new Interpreter();

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  // Now group a record
  await interp.run("'assignee' GROUP-BY-FIELD");
  let grouped_rec = interp.stack_pop();
  expect(Object.keys(grouped_rec).sort()).toEqual(["user1", "user2"]);
  expect(grouped_rec["user1"].length).toBe(4);
  expect(grouped_rec["user2"].length).toBe(3);
  expect(grouped).toEqual(grouped_rec);

  // Test grouping a list-valued field
  interp.stack_push([
    { id: 1, attrs: ["blue", "important"] },
    { id: 2, attrs: ["red"] },
  ]);
  await interp.run("'attrs' GROUP-BY-FIELD");
  grouped_rec = interp.stack_pop();
  expect(grouped_rec["blue"][0].id).toBe(1);
  expect(grouped_rec["important"][0].id).toBe(1);
  expect(grouped_rec["red"][0].id).toBe(2);
});

test("Group by", async () => {
  interp.stack_push(makeRecords());
  await interp.run(`
    "'assignee' REC@" GROUP-BY
  `);
  const grouped = interp.stack_pop();
  expect(Object.keys(grouped).sort()).toEqual(["user1", "user2"]);
  expect(grouped["user1"].length).toBe(4);
  expect(grouped["user2"].length).toBe(3);

  // Test grouping a record
  interp = new Interpreter();

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  // Now group a record
  await interp.run(`
    "'assignee' REC@" GROUP-BY
  `);
  const grouped_rec = interp.stack_pop();
  expect(Object.keys(grouped_rec).sort()).toEqual(["user1", "user2"]);
  expect(grouped_rec["user1"].length).toBe(4);
  expect(grouped_rec["user2"].length).toBe(3);
  expect(grouped).toEqual(grouped_rec);
});

test("Group by with key", async () => {
  interp.stack_push(makeRecords());
  await interp.run(`
    ['key' 'val'] VARIABLES
    "val ! key ! key @ 3 MOD" !WITH-KEY GROUP-BY
  `);
  const grouped = interp.stack_pop();
  expect(Object.keys(grouped).sort()).toEqual(["0", "1", "2"]);
  expect(grouped[0].length).toBe(3);
  expect(grouped[1].length).toBe(2);
  expect(grouped[2].length).toBe(2);

  // Test grouping a record
  interp = new Interpreter();

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  // Now group a record
  await interp.run(`
    ['key' 'val'] VARIABLES
    "val ! key ! key @ 2 *" !WITH-KEY GROUP-BY
  `);
  const grouped_rec = interp.stack_pop();
  expect(Object.keys(grouped_rec).sort()).toEqual([
    "200",
    "202",
    "204",
    "206",
    "208",
    "210",
    "212",
  ]);
});

test("Groups of", async () => {
  await interp.run(`
    [1 2 3 4 5 6 7 8] 3 GROUPS-OF
  `);
  const groups = interp.stack_pop();
  expect(groups[0]).toEqual([1, 2, 3]);
  expect(groups[1]).toEqual([4, 5, 6]);
  expect(groups[2]).toEqual([7, 8]);
});

test("Groups of record", async () => {
  await interp.run(`
    [
      ['a' 1]
      ['b' 2]
      ['c' 3]
      ['d' 4]
      ['e' 5]
      ['f' 6]
      ['g' 7]
      ['h' 8]
    ] REC 3 GROUPS-OF
  `);
  const groups = interp.stack_pop();
  expect(groups[0]).toEqual({ a: 1, b: 2, c: 3 });
  expect(groups[1]).toEqual({ d: 4, e: 5, f: 6 });
  expect(groups[2]).toEqual({ g: 7, h: 8 });
});


test("Groups of using record", async () => {
  // Test grouping a record
  interp = new Interpreter();

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  // Now group a record
  await interp.run(`
      3 GROUPS-OF
    `);
  const recs = interp.stack_pop();
  expect(Object.keys(recs[0]).length).toBe(3);
  expect(Object.keys(recs[1]).length).toBe(3);
  expect(Object.keys(recs[2]).length).toBe(1);
});

test("INDEX", async () => {
  await interp.run(`
    : |KEYS   "'key' REC@" MAP;
    : TICKETS [
      [['key'   101] ['Labels'  ['alpha' 'beta']]] REC
      [['key'   102] ['Labels'  ['alpha' 'gamma']]] REC
      [['key'   103] ['Labels'  ['alpha']]] REC
      [['key'   104] ['Labels'  ['beta']]] REC
    ];

    TICKETS "'Labels' REC@" INDEX  "|KEYS" MAP
  `);
  const index_record = interp.stack_pop();
  expect(index_record["alpha"]).toEqual([101, 102, 103]);
  expect(index_record["beta"]).toEqual([101, 104]);
  expect(index_record["gamma"]).toEqual([102]);
});

test("MAP", async () => {
  await interp.run(`
    [1 2 3 4 5] '2 *' MAP
  `);
  let array = interp.stack_pop();
  expect(array).toEqual([2, 4, 6, 8, 10]);

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  await interp.run(`
    "'status' REC@" MAP
  `);
  const record = interp.stack_pop();
  expect(record[100]).toBe("OPEN");
  expect(record[102]).toBe("IN PROGRESS");
  expect(record[106]).toBe("CLOSED");

  // Test map in module
  await interp.run(`
    {my-module
      : DOUBLE   2 *;
      : RUN   [1 2 3 4 5] "DOUBLE" MAP;
    }
    {my-module RUN}
  `);
  array = interp.stack_pop();
  expect(array).toEqual([2, 4, 6, 8, 10]);
});

test("MAP depth", async () => {
  const interp = new Interpreter();
  await interp.run(`
    : k1-REC   [
      ["l1"  [["m"  2]] REC]
      ["l2"  [["m"  3]] REC]
    ] REC;

    : k2-REC   [
      ["l1"  [["m"  3]] REC]
      ["l2"  [["m"  4]] REC]
    ] REC;

    : DEEP-RECORD [
      ["k1"  k1-REC]
      ["k2"  k2-REC]
    ] REC;

    DEEP-RECORD "2 *" 2 !DEPTH MAP
  `);
  const record = interp.stack_pop();
  expect(record).toEqual({
    k1: { l1: { m: 4 }, l2: { m: 6 } },
    k2: { l1: { m: 6 }, l2: { m: 8 } },
  });
});

test("MAP depth over array", async () => {
  const interp = new Interpreter();
  await interp.run(`
    : DEEP-LIST [ [ [[["m"  2]] REC [["m"  3]] REC] ] [ [[["m"  3]] REC [["m"  4]] REC] ] ];

    DEEP-LIST "2 *" 3 !DEPTH MAP
  `);
  const array = interp.stack_pop();
  expect(array).toStrictEqual([[[{ m: 4 }, { m: 6 }]], [[{ m: 6 }, { m: 8 }]]]);
});

test("MAP depth over array of maps", async () => {
  const interp = new Interpreter();
  await interp.run(`
    : DEEP-LIST [ [ [2 3] ] [ [3 4] ] ];

    DEEP-LIST "2 *" 2 !DEPTH MAP
  `);
  const array = interp.stack_pop();
  expect(array).toEqual([[[4, 6]], [[6, 8]]]);
});

test("MAP depth with error", async () => {
  const interp = new Interpreter();
  await interp.run(`
    : k1-REC   [
      ["l1"  [["m"  2]] REC]
      ["l2"  [["m"  3]] REC]
    ] REC;

    : k2-REC   [
      ["l1"  [["m"  'GARBAGE']] REC]
      ["l2"  [["m"  4]] REC]
    ] REC;

    : DEEP-RECORD [
      ["k1"  k1-REC]
      ["k2"  k2-REC]
    ] REC;

    DEEP-RECORD ">STR INTERPRET" 2 !DEPTH !PUSH-ERROR MAP
  `);
  const errors = interp.stack_pop();
  const record = interp.stack_pop();

  expect(record).toEqual({
    k1: { l1: { m: 2 }, l2: { m: 3 } },
    k2: { l1: { m: null }, l2: { m: 4 } },
  });
  expect(errors[0]).toBe(null);
  expect(errors[1]).toBe(null);
  expect(errors[2]).not.toBe(null);
  expect(errors[3]).toBe(null);
});

test("MAP with key", async () => {
  await interp.run(`
    [1 2 3 4 5] '+ 2 *' !WITH-KEY MAP
  `);
  const array = interp.stack_pop();
  expect(array).toEqual([2, 6, 10, 14, 18]);

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  await interp.run(`
    ["k" "v"] VARIABLES
    "v ! k ! k @ >STR v @ 'status' REC@ CONCAT" !WITH-KEY MAP
  `);
  const record = interp.stack_pop();
  expect(record[100]).toBe("100OPEN");
  expect(record[102]).toBe("102IN PROGRESS");
  expect(record[106]).toBe("106CLOSED");
});

test("FOREACH", async () => {
  await interp.run(`
    0 [1 2 3 4 5] '+' FOREACH
  `);
  const sum = interp.stack_pop();
  expect(sum).toBe(15);

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  await interp.run(`
    "" SWAP "'status' REC@ CONCAT" FOREACH
  `);
  const string = interp.stack_pop();
  expect(string).toBe("OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED");
});

test("FOREACH with key", async () => {
  await interp.run(`
    0 [1 2 3 4 5] '+ +' !WITH-KEY FOREACH
  `);
  const sum = interp.stack_pop();
  expect(sum).toBe(25);

  // First, set up the record
  const records = makeRecords();
  const by_key = {};
  for (const rec of records) {
    by_key[rec.key] = rec;
  }
  interp.stack_push(by_key);

  await interp.run(`
    "" SWAP "'status' REC@ CONCAT CONCAT" !WITH-KEY FOREACH
  `);
  const string = interp.stack_pop();
  expect(string).toBe(
    "100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED",
  );
});

function makeStatusToManagerToIds() {
  return {
    open: {
      manager1: [101, 102],
      manager2: [103],
    },
    blocked: {
      manager3: [104],
    },
    closed: {
      manager1: [10, 11],
      manager2: [12, 13],
    },
  };
}

test("FOREACH to errors", async () => {
  await interp.run(`
    ['2' '3' 'GARBAGE' '+'] 'INTERPRET' !PUSH-ERROR FOREACH
  `);
  const errors = interp.stack_pop();
  expect(errors[0]).toBeNull();
  expect(errors[1]).toBeNull();
  expect(errors[2]).not.toBeNull();
  expect(errors[3]).toBeNull();
  const res = interp.stack_pop();
  expect(res).toBe(5);
});

test("INVERT-KEYS", async () => {
  const statusToManagerToIds = makeStatusToManagerToIds();
  interp.stack_push(statusToManagerToIds);
  await interp.run("INVERT-KEYS");
  const res = interp.stack_pop();
  const expected = {
    manager1: {
      open: [101, 102],
      closed: [10, 11],
    },
    manager2: {
      open: [103],
      closed: [12, 13],
    },
    manager3: {
      blocked: [104],
    },
  };
  expect(res).toEqual(expected);
});

test("ZIP", async () => {
  await interp.run(`
    ['a' 'b'] [1 2] ZIP
  `);
  const array = interp.stack_pop();
  expect(array[0]).toEqual(["a", 1]);
  expect(array[1]).toEqual(["b", 2]);

  // First, set up the record
  await interp.run(`
    [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
  `);
  const record = interp.stack_pop();
  expect(Object.keys(record).sort()).toEqual(["a", "b", "z"]);
  expect(record["a"]).toEqual([100, "Hi"]);
  expect(record["b"]).toEqual([200, "Bye"]);
  expect(record["z"]).toEqual([300, undefined]);
});

test("ZIP-WITH", async () => {
  await interp.run(`
    [10 20] [1 2] "+" ZIP-WITH
  `);
  const array = interp.stack_pop();
  expect(array[0]).toBe(11);
  expect(array[1]).toBe(22);

  // First, set up the record
  await interp.run(`
    [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+" ZIP-WITH
  `);
  const record = interp.stack_pop();
  expect(Object.keys(record).sort()).toEqual(["a", "b"]);
  expect(record["a"]).toBe(11);
  expect(record["b"]).toBe(22);
});

test("KEYS", async () => {
  await interp.run(`
    ['a' 'b' 'c'] KEYS
  `);
  let array = interp.stack_pop();
  expect(array).toEqual([0, 1, 2]);

  // First, set up the record
  await interp.run(`
    [['a' 1] ['b' 2]] REC KEYS
  `);
  array = interp.stack_pop();
  expect(array.sort()).toEqual(["a", "b"]);
});

test("VALUES", async () => {
  await interp.run(`
    ['a' 'b' 'c'] VALUES
  `);
  let array = interp.stack_pop();
  expect(array).toEqual(["a", "b", "c"]);

  // First, set up the record
  await interp.run(`
    [['a' 1] ['b' 2]] REC VALUES
  `);
  array = interp.stack_pop();
  expect(array.sort()).toEqual([1, 2]);
});

test("LENGTH", async () => {
  await interp.run(`
    ['a' 'b' 'c'] LENGTH
    "Howdy" LENGTH
  `);
  expect(interp.stack_pop()).toBe(5);
  expect(interp.stack_pop()).toBe(3);

  // Test record
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2]] REC LENGTH
  `);
  const length = (interp as any).stack[0];
  expect(length).toBe(2);
});

test("RANGE", async () => {
  await interp.run(`
    : EVEN?   2 MOD  0 ==;
    : ODD?    2 MOD  1 ==;
    [1 2 3 4 5] "EVEN?" "ODD?" RANGE
  `);
  expect((interp as any).stack[0]).toEqual([1, 2]);
});

test("SLICE", async () => {
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
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual(["a", "b", "c"]);
  expect(stack[1]).toEqual(["b", "c", "d"]);
  expect(stack[2]).toEqual(["f", "e", "d"]);
  expect(stack[3]).toEqual(["g", "f"]);
  expect(stack[4]).toEqual(["e", "f"]);
  expect(stack[5]).toEqual(["f", "g", null, null]);

  // Slice records
  interp = new Interpreter();
  await interp.run(`
    ['x'] VARIABLES
    [['a' 1] ['b' 2] ['c' 3]] REC x !
    x @ 0 1 SLICE
    x @ -1 -2 SLICE
    x @ 5 7 SLICE
  `);
  const stack2 = (interp as any).stack;
  expect(Object.keys(stack2[0]).sort()).toEqual(["a", "b"]);
  expect(Object.keys(stack2[1]).sort()).toEqual(["b", "c"]);
  expect(stack2[2]).toEqual({});
});

test("DIFFERENCE", async () => {
  await interp.run(`
    ['x' 'y'] VARIABLES
    ['a' 'b' 'c'] x !
    ['a' 'c' 'd'] y !
    x @ y @ DIFFERENCE
    y @ x @ DIFFERENCE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual(["b"]);
  expect(stack[1]).toEqual(["d"]);

  // Records
  interp = new Interpreter();
  await interp.run(`
    ['x' 'y'] VARIABLES
    [['a' 1] ['b' 2] ['c' 3]] REC x !
    [['a' 20] ['c' 40] ['d' 10]] REC y !
    x @ y @ DIFFERENCE
    y @ x @ DIFFERENCE
  `);
  const stack2 = (interp as any).stack;
  expect(Object.keys(stack2[0])).toEqual(["b"]);
  expect(Object.values(stack2[0])).toEqual([2]);
  expect(Object.keys(stack2[1])).toEqual(["d"]);
  expect(Object.values(stack2[1])).toEqual([10]);
});

test("INTERSECTION", async () => {
  await interp.run(`
    ['x' 'y'] VARIABLES
    ['a' 'b' 'c'] x !
    ['a' 'c' 'd'] y !
    x @ y @ INTERSECTION
  `);
  let stack = (interp as any).stack;
  expect(stack[0].sort()).toEqual(["a", "c"]);

  // Records
  interp = new Interpreter();
  await interp.run(`
    ['x' 'y'] VARIABLES
    [['a' 1] ['b' 2] ['f' 3]] REC x !
    [['a' 20] ['c' 40] ['d' 10]] REC y !
    x @ y @ INTERSECTION
  `);
  stack = (interp as any).stack;
  expect(Object.keys(stack[0])).toEqual(["a"]);
  expect(Object.values(stack[0])).toEqual([1]);
});

test("UNION", async () => {
  await interp.run(`
    ['x' 'y'] VARIABLES
    ['a' 'b' 'c'] x !
    ['a' 'c' 'd'] y !
    x @ y @ UNION
  `);
  let stack = (interp as any).stack;
  expect(stack[0].sort()).toEqual(["a", "b", "c", "d"]);

  // Records
  interp = new Interpreter();
  await interp.run(`
    ['x' 'y'] VARIABLES
    [['a' 1] ['b' 2] ['f' 3]] REC x !
    [['a' 20] ['c' 40] ['d' 10]] REC y !
    x @ y @ UNION
  `);
  stack = (interp as any).stack;
  expect(Object.keys(stack[0]).sort()).toEqual(["a", "b", "c", "d", "f"]);
  expect(Object.values(stack[0]).sort((a: number, b: number) => a - b)).toEqual(
    [1, 2, 3, 10, 40],
  );
});

test("SELECT", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([1, 3, 5]);

  // Slice records
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  "2 MOD 0 ==" SELECT
  `);
  stack = (interp as any).stack;
  expect(Object.keys(stack[0])).toEqual(["b"]);
  expect(Object.values(stack[0])).toEqual([2]);
});

test("SELECT with key", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] "+ 3 MOD 1 ==" !WITH-KEY SELECT
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([2, 5]);

  // Slice records
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  "CONCAT 'c3' ==" !WITH-KEY SELECT
  `);
  stack = (interp as any).stack;
  expect(Object.keys(stack[0])).toEqual(["c"]);
  expect(Object.values(stack[0])).toEqual([3]);
});

test("TAKE", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] 3 TAKE
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([0, 1, 2]);

  // Take records
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 TAKE
  `);
  stack = (interp as any).stack;
  expect(stack[0].length).toBe(2);
});

test("TAKE with rest", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] 3 !PUSH-REST TAKE
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([0, 1, 2]);
  expect(stack[1]).toEqual([3, 4, 5, 6]);

  // Take records
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 !PUSH-REST TAKE
  `);
  stack = (interp as any).stack;
  expect(stack[0].length).toBe(2);
  expect(stack[1].length).toBe(1);
});

test("DROP", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] 4 DROP
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([4, 5, 6]);

  // Drop records
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 DROP
  `);
  stack = (interp as any).stack;
  expect(stack[0].length).toBe(1);
});

test("ROTATE", async () => {
  await interp.run(`
    ['a' 'b' 'c' 'd'] ROTATE
    ['b'] ROTATE
    [] ROTATE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual(["d", "a", "b", "c"]);
  expect(stack[1]).toEqual(["b"]);
  expect(stack[2]).toEqual([]);
});

test("ARRAY?", async () => {
  await interp.run(`
    ['a' 'b' 'c' 'd'] ARRAY?
    'b' ARRAY?
    0 ARRAY?
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(true);
  expect(stack[1]).toBe(false);
  expect(stack[2]).toBe(false);
});

test("SHUFFLE", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] SHUFFLE
  `);
  const stack = (interp as any).stack;
  expect(stack[0].length).toBe(7);
});

test("SORT", async () => {
  await interp.run(`
    [2 8 1 4 7 3] SORT
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual([1, 2, 3, 4, 7, 8]);
});

test("SORT with null", async () => {
  await interp.run(`
    [2 8 1 NULL 4 7 NULL 3] SORT
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual([1, 2, 3, 4, 7, 8, null, null]);
});

test("SORT with forthic", async () => {
  await interp.run(`
    [2 8 1 4 7 3] "-1 *" !COMPARATOR SORT
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual([8, 7, 4, 3, 2, 1]);
});

test("SORT with key func", async () => {
  interp.stack_push(makeRecords());
  await interp.run(`
    'status' FIELD-KEY-FUNC !COMPARATOR SORT
  `);
  const stack = (interp as any).stack;
  expect(stack[0][0]["status"]).toBe("CLOSED");
  expect(stack[0][1]["status"]).toBe("CLOSED");
  expect(stack[0][2]["status"]).toBe("IN PROGRESS");
  expect(stack[0][3]["status"]).toBe("IN PROGRESS");
  expect(stack[0][4]["status"]).toBe("OPEN");
  expect(stack[0][5]["status"]).toBe("OPEN");
  expect(stack[0][6]["status"]).toBe("OPEN");
});

test("NTH", async () => {
  await interp.run(`
    ["x"] VARIABLES
    [0 1 2 3 4 5 6] x !
    x @ 0 NTH
    x @ 5 NTH
    x @ 55 NTH
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(0);
  expect(stack[1]).toBe(5);
  expect(stack[2]).toBeNull();
});

test("LAST", async () => {
  await interp.run(`
    [0 1 2 3 4 5 6] LAST
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(6);
});

test("UNPACK", async () => {
  await interp.run(`
    [0 1 2] UNPACK
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toBe(0);
  expect(stack[1]).toBe(1);
  expect(stack[2]).toBe(2);

  // For record
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC UNPACK
  `);
  stack = (interp as any).stack;
  expect(stack[0]).toBe(1);
  expect(stack[1]).toBe(2);
  expect(stack[2]).toBe(3);
});

test("FLATTEN", async () => {
  await interp.run(`
    [0 [1 2 [3 [4]] ]] FLATTEN
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toEqual([0, 1, 2, 3, 4]);

  // For record
  interp = new Interpreter();
  await interp.run(`
    ['uno' 'alpha'] VARIABLES
    [['uno' 4] ['duo' 8]] REC uno !
    [['alpha' uno @]] REC alpha !
    [['a' 1] ['b' alpha @] ['c' 3]] REC FLATTEN
  `);
  stack = (interp as any).stack;
  const record = stack[0];
  expect(Object.keys(record).sort()).toEqual([
    "a",
    "b\talpha\tduo",
    "b\talpha\tuno",
    "c",
  ]);
});

test("FLATTEN depth", async () => {
  const interp = new Interpreter();
  await interp.run(`
    [ [ [0 1] [2 3] ]
      [ [4 5]       ] ] 1 !DEPTH FLATTEN
  `);
  let array = (interp as any).stack[(interp as any).stack.length - 1];
  expect(array).toEqual([
    [0, 1],
    [2, 3],
    [4, 5],
  ]);

  await interp.run(`
    [ [ [0 1] [2 3] ]
      [ [4 5]       ] ] 0 !DEPTH FLATTEN
  `);
  array = (interp as any).stack[(interp as any).stack.length - 1];
  expect(array).toEqual([
    [
      [0, 1],
      [2, 3],
    ],
    [[4, 5]],
  ]);

  await interp.run(`
    [ [ [0 1] [2 3] ]
      [ [4 5]       ] ] 2 !DEPTH FLATTEN
  `);
  array = (interp as any).stack[(interp as any).stack.length - 1];
  expect(array).toEqual([0, 1, 2, 3, 4, 5]);
});

test("FLATTEN one level record", async () => {
  await interp.run(`
    ['uno' 'alpha'] VARIABLES
    [['uno' 4] ['duo' 8]] REC uno !
    [['alpha' uno @]] REC alpha !
    [['a' 1] ['b' alpha @] ['c' 3]] REC 1 !DEPTH FLATTEN
  `);
  const record = (interp as any).stack[(interp as any).stack.length - 1];
  expect(Object.keys(record).sort()).toEqual(["a", "b\talpha", "c"]);
});

test("KEY-OF", async () => {
  await interp.run(`
    ['x'] VARIABLES
    ['a' 'b' 'c' 'd'] x !
    x @  'c' KEY-OF
    x @  'z' KEY-OF
  `);
  let stack = (interp as any).stack;
  expect(stack[0]).toBe(2);
  expect(stack[1]).toBeNull();

  // For record
  interp = new Interpreter();
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  2 KEY-OF
  `);
  stack = (interp as any).stack;
  expect(stack[0]).toBe("b");
});

test("REDUCE", async () => {
  await interp.run(`
    [1 2 3 4 5] 10 "+" REDUCE
  `);
  expect(interp.stack_pop()).toBe(25);

  // For record
  await interp.run(`
    [['a' 1] ['b' 2] ['c' 3]] REC  20 "+" REDUCE
  `);
  expect(interp.stack_pop()).toBe(26);
});

test("POP", async () => {
  await interp.run(`
    1 2 3 4 5 POP
  `);
  const stack = (interp as any).stack;
  expect(stack.length).toBe(4);
  expect(stack[stack.length - 1]).toBe(4);
});

test("DUP", async () => {
  await interp.run(`
    5 DUP
  `);
  const stack = (interp as any).stack;
  expect(stack.length).toBe(2);
  expect(stack[0]).toBe(5);
  expect(stack[1]).toBe(5);
});

test("SWAP", async () => {
  await interp.run(`
    6 8 SWAP
  `);
  const stack = (interp as any).stack;
  expect(stack.length).toBe(2);
  expect(stack[0]).toBe(8);
  expect(stack[1]).toBe(6);
});

test("SPLIT", async () => {
  await interp.run(`
    'Now is the time' ' ' SPLIT
  `);
  const stack = (interp as any).stack;
  expect(stack.length).toBe(1);
  expect(stack[0]).toEqual(["Now", "is", "the", "time"]);
});

test("JOIN", async () => {
  await interp.run(`
    ["Now" "is" "the" "time"] "--" JOIN
  `);
  const stack = (interp as any).stack;
  expect(stack.length).toBe(1);
  expect(stack[0]).toBe("Now--is--the--time");
});

test("SPECIAL CHARS", async () => {
  await interp.run(`
    /R /N /T
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("\r");
  expect(stack[1]).toBe("\n");
  expect(stack[2]).toBe("\t");
});

test("LOWERCASE", async () => {
  await interp.run(`
    "HOWDY, Everyone!" LOWERCASE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("howdy, everyone!");
});

test("ASCII", async () => {
  await interp.run(`
    "“HOWDY, Everyone!”" ASCII
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("HOWDY, Everyone!");
});

test("STRIP", async () => {
  await interp.run(`
    "  howdy  " STRIP
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("howdy");
});

test("REPLACE", async () => {
  await interp.run(`
    "1-40 2-20" "-" "." REPLACE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("1.40 2.20");
});

test("RE-MATCH", async () => {
  await interp.run(`
    "123message456" "\\d{3}.*\\d{3}" RE-MATCH
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).not.toBeNull();
});

test("RE-MATCH-GROUP", async () => {
  await interp.run(`
    "123message456" "\\d{3}(.*)\\d{3}" RE-MATCH 1 RE-MATCH-GROUP
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("message");
});

test("RE-MATCH-ALL", async () => {
  await interp.run(`
    "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual(["android", "ios", "android", "web", "web"]);
});

test("URL-ENCODE", async () => {
  await interp.run(`
    "now/is the time" URL-ENCODE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("now%2Fis%20the%20time");
});

test("URL-DECODE", async () => {
  await interp.run(`
    "now%2Fis%20the%20time" URL-DECODE
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("now/is the time");
});

test("DEFAULT", async () => {
  await interp.run(`
    NULL 22.4 DEFAULT
    0 22.4 DEFAULT
    "" "Howdy" DEFAULT
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(22.4);
  expect(stack[1]).toBe(0);
  expect(stack[2]).toBe("Howdy");
});

test("*DEFAULT", async () => {
  await interp.run(`
    NULL "3.1 5 +" *DEFAULT
    0 "22.4" *DEFAULT
    "" "['Howdy, ' 'Everyone!'] CONCAT" *DEFAULT
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBeCloseTo(8.1);
  expect(stack[1]).toBe(0);
  expect(stack[2]).toBe("Howdy, Everyone!");
});

test("<REPEAT", async () => {
  await interp.run(`
    [0 "1 +" 6 <REPEAT]
  `);
  const stack = (interp as any).stack;
  expect(stack[0]).toEqual([0, 1, 2, 3, 4, 5, 6]);
});

test("TO-FIXED", async () => {
  await interp.run(`
        22 7 / 2 >FIXED
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("3.14");
});

test("TO-JSON", async () => {
  await interp.run(`
        [["a" 1] ["b" 2]] REC >JSON
      `);
  expect(interp.stack_pop()).toBe('{"a":1,"b":2}');
});

test("JSON-TO", async () => {
  await interp.run(`
        '{"a": 1, "b": 2}' JSON>
      `);
  const stack = (interp as any).stack;
  expect(Object.keys(stack[0]).sort()).toEqual(["a", "b"]);
  expect(stack[0]["a"]).toBe(1);
  expect(stack[0]["b"]).toBe(2);
});

test("NOW", async () => {
  await interp.run("NOW");
  const stack = (interp as any).stack;
  const result = Temporal.PlainDateTime.from(stack[0]);
  const now = Temporal.Now.plainDateTimeISO(interp.get_timezone());
  expect(result.hour).toBe(now.hour);
  expect(result.minute).toBe(now.minute);
});

test("TO-TIME", async () => {
  await interp.run("'10:52 PM' >TIME");
  const stack = (interp as any).stack;
  const result = Temporal.PlainTime.from(stack[0]);
  expect(result.hour).toBe(22);
  expect(result.minute).toBe(52);
});

test("TO-DATE", async () => {
  await interp.run(`
        "Oct 21, 2020" >DATE
      `);
  const stack = (interp as any).stack;
  const date = Temporal.PlainDate.from(stack[0]);
  expect(date.year).toBe(2020);
  expect(date.month).toBe(10);
  expect(date.day).toBe(21);
});

test("TODAY", async () => {
  await interp.run("TODAY");
  const stack = (interp as any).stack;
  const result = Temporal.PlainDate.from(stack[0]);
  const today = Temporal.Now.plainDateISO(interp.get_timezone());
  console.log("today: ", JSON.stringify(today, undefined, 4));
  expect(result.year).toBe(today.year);
  expect(result.month).toBe(today.month);
  expect(result.day).toBe(today.day);
});

test("Date literals", async () => {
  const zonedDateTime = Temporal.Now.zonedDateTimeISO(interp?.get_timezone() ?? "UTC");

  // Case 1: Wildcard year and month and day
  await interp.run(`YYYY-MM-DD`);
  let date = interp.stack_pop();
  expect(date.year).toBe(zonedDateTime.year);
  expect(date.month).toBe(zonedDateTime.month);
  expect(date.day).toBe(zonedDateTime.day);

  // Case 2: Wildcard year and month
  await interp.run(`YYYY-MM-14`);
  date = interp.stack_pop();
  expect(date.year).toBe(zonedDateTime.year);
  expect(date.month).toBe(zonedDateTime.month);
  expect(14).toBe(date.day);

  // Case 3: Wildcard year
  await interp.run(`YYYY-02-03`);
  date = interp.stack_pop();
  expect(date.year).toBe(zonedDateTime.year);
  expect(2).toBe(date.month);
  expect(3).toBe(date.day);

  // Case 4: Explicit date
  await interp.run(`2025-02-03`);
  date = interp.stack_pop();
  expect(2025).toBe(date.year);
  expect(2).toBe(date.month);
  expect(3).toBe(date.day);

  // Case 5: Wildcard month and day
  await interp.run(`2025-MM-DD`);
  date = interp.stack_pop();
  expect(2025).toBe(date.year);
  expect(date.month).toBe(zonedDateTime.month);
  expect(date.day).toBe(zonedDateTime.day);

  // Case 6: Wildcard day
  await interp.run(`2025-03-DD`);
  date = interp.stack_pop();
  expect(2025).toBe(date.year);
  expect(3).toBe(date.month);
  expect(date.day).toBe(zonedDateTime.day);

  // Case 7: Wildcard month
  await interp.run(`2025-MM-03`);
  date = interp.stack_pop();
  expect(2025).toBe(date.year);
  expect(date.month).toBe(zonedDateTime.month);
  expect(3).toBe(date.day);

  // Case 8: Wildcard year and day
  await interp.run(`YYYY-10-DD`);
  date = interp.stack_pop();
  expect(date.year).toBe(zonedDateTime.year);
  expect(10).toBe(date.month);
  expect(date.day).toBe(zonedDateTime.day);
});

// test("DAYS-OF-WEEK", async () => {
//   await interp.run(`
//         MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY
//       `);
//   const stack = (interp as any).stack;
//   const today = new Date();
//   expect(new Date(stack[0]).getTime()).toBeLessThanOrEqual(today.getTime());
//   expect(new Date(stack[6]).getTime()).toBeGreaterThanOrEqual(today.getTime());
// });

test("ADD-DAYS to date", async () => {
  await interp.run(`
        2020-10-21 12 ADD-DAYS
      `);
  const stack = (interp as any).stack;
  const date = Temporal.PlainDate.from(stack[0]);
  expect(date.year).toBe(2020);
  expect(date.month).toBe(11); // Months are 0-based in JavaScript
  expect(date.day).toBe(2);
});

test("SUBTRACT-DATES", async () => {
  await interp.run(`
        2020-10-21 2020-11-02 SUBTRACT-DATES
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(-12);
});

test("DATE-TO-STR", async () => {
  await interp.run(`
        2020-11-02 DATE>STR
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe("2020-11-02");
});

test("DATE-TIME-TO-DATETIME", async () => {
  await interp.run(`
        2020-11-02 10:25 PM DATE-TIME>DATETIME
        2020-11-02 10:25 PM DATE-TIME>DATETIME >DATE
        2020-11-02 10:25 PM DATE-TIME>DATETIME >TIME
      `);
  const stack = (interp as any).stack;
  const datetime = Temporal.PlainDateTime.from(stack[0]);
  expect(datetime.year).toBe(2020);
  expect(datetime.month).toBe(11); // Months are 0-based in JavaScript
  expect(datetime.day).toBe(2);
  expect(datetime.hour).toBe(22);
  expect(datetime.minute).toBe(25);

  const date = Temporal.PlainDateTime.from(stack[1]);
  expect(date.year).toBe(2020);
  expect(date.month).toBe(11);
  expect(date.day).toBe(2);

  const time = Temporal.PlainTime.from(stack[2]);
  expect(time.hour).toBe(22);
  expect(time.minute).toBe(25);
});

test("DATETIME-TO-TIMESTAMP", async () => {
  await interp.run(`
        2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(1593642000);
});

test("TIMESTAMP-TO-DATETIME", async () => {
  await interp.run(`
        1593895532 TIMESTAMP>DATETIME
      `);
  const stack = (interp as any).stack;
  const datetime = Temporal.PlainDateTime.from(stack[0]);
  expect(datetime.year).toBe(2020);
  expect(datetime.month).toBe(7); // Months are 0-based in JavaScript
  expect(datetime.day).toBe(4);
  expect(datetime.hour).toBe(13);
  expect(datetime.minute).toBe(45);
});

test("arithmetic", async () => {
  await interp.run(`
            2 4 +
            2 4 -
            2 4 *
            2 4 /
            5 3 MOD
            2.51 ROUND
            [1 2 3] +
            [2 3 4] *
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(6);
  expect(stack[1]).toBe(-2);
  expect(stack[2]).toBe(8);
  expect(stack[3]).toBe(0.5);
  expect(stack[4]).toBe(2);
  expect(stack[5]).toBe(3);
  expect(stack[6]).toBe(6);
  expect(stack[7]).toBe(24);
});

test("MEAN", async () => {
  await interp.run("[1 2 3 4 5] MEAN");
  let stack = (interp as any).stack;
  expect(stack[stack.length - 1]).toBe(3);

  await interp.run("[4] MEAN");
  stack = (interp as any).stack;
  expect(stack[stack.length - 1]).toBe(4);

  await interp.run("[] MEAN");
  stack = (interp as any).stack;
  expect(stack[stack.length - 1]).toBe(0);

  await interp.run("NULL MEAN");
  stack = (interp as any).stack;
  expect(stack[stack.length - 1]).toBe(0);
});

test("comparison", async () => {
  await interp.run(`
            2 4 ==
            2 4 !=
            2 4 <
            2 4 <=
            2 4 >
            2 4 >=
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(false);
  expect(stack[1]).toBe(true);
  expect(stack[2]).toBe(true);
  expect(stack[3]).toBe(true);
  expect(stack[4]).toBe(false);
  expect(stack[5]).toBe(false);
});

test("logic", async () => {
  await interp.run(`
            FALSE FALSE OR
            [FALSE FALSE TRUE FALSE] OR
            FALSE TRUE AND
            [FALSE FALSE TRUE FALSE] AND
            FALSE NOT
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(false);
  expect(stack[1]).toBe(true);
  expect(stack[2]).toBe(false);
  expect(stack[3]).toBe(false);
  expect(stack[4]).toBe(true);
});

test("IN", async () => {
  await interp.run(`
            "alpha" ["beta" "gamma"] IN
            "alpha" ["beta" "gamma" "alpha"] IN
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(false);
  expect(stack[1]).toBe(true);
});

test("ANY", async () => {
  await interp.run(`
            ["alpha" "beta"] ["beta" "gamma"] ANY
            ["delta" "beta"] ["gamma" "alpha"] ANY
            ["alpha" "beta"] [] ANY
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(true);
  expect(stack[1]).toBe(false);
  expect(stack[2]).toBe(true);
});

test("ALL", async () => {
  await interp.run(`
            ["alpha" "beta"] ["beta" "gamma"] ALL
            ["delta" "beta"] ["beta"] ALL
            ["alpha" "beta"] [] ALL
          `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(false);
  expect(stack[1]).toBe(true);
  expect(stack[2]).toBe(true);
});

test("QUOTED", async () => {
  const DLE = "\u0010"; // Data Link Escape character
  await interp.run(`
        "howdy" QUOTED
        "sinister${DLE}INJECT-BADNESS" QUOTED
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(`${DLE}howdy${DLE}`);
  expect(stack[1]).toBe(`${DLE}sinister INJECT-BADNESS${DLE}`);
});

test("RANGE-INDEX", async () => {
  await interp.run(`
        0 [0 1 2] RANGE-INDEX
        1 [0 1 2] RANGE-INDEX
        2 [0 1 2] RANGE-INDEX
        3 [0 1 2] RANGE-INDEX
        100 [0 1 2] RANGE-INDEX
        -1 [0 1 2] RANGE-INDEX
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(0);
  expect(stack[1]).toBe(1);
  expect(stack[2]).toBe(2);
  expect(stack[3]).toBe(2);
  expect(stack[4]).toBe(2);
  expect(stack[5]).toBeNull();
});

test("MATH CONVERTERS", async () => {
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
      `);
  const stack = (interp as any).stack;
  expect(stack[0]).toBe(false);
  expect(stack[1]).toBe(false);
  expect(stack[2]).toBe(true);
  expect(stack[3]).toBe(false);
  expect(stack[4]).toBe(true);
  expect(stack[5]).toBe(3);
  expect(stack[6]).toBe(4);
  expect(stack[7]).toBe(4);
  expect(stack[8]).toBe(1.2);
  expect(stack[9]).toBe(2.0);
});

test("PROFILING", async () => {
  await interp.run(`
        PROFILE-START
        [1 "1 +" 6 <REPEAT]
        PROFILE-END POP
        PROFILE-DATA
      `);
  const stack = (interp as any).stack;
  const profile_data = stack[stack.length - 1];
  expect(profile_data["word_counts"][0]["word"]).toBe("1");
  expect(profile_data["word_counts"][0]["count"]).toBe(7);
});

test("Parallel map", async () => {
  await interp.run(`
    [ 1 2 3 4 5 ] "DUP *" 2 !INTERPS MAP
    `);
  expect(interp.stack_pop()).toEqual([1, 4, 9, 16, 25]);
});

test("Parallel map over record", async () => {
  await interp.run(`
    [
      ['a' 1]
      ['b' 2]
      ['c' 3]
      ['d' 4]
    ] REC "3 *" 2 !INTERPS MAP
    `);
  expect(interp.stack_pop()).toEqual({ a: 3, b: 6, c: 9, d: 12 });
});

test("|REC@|", async () => {
  interp.stack_push([{ a: 1 }, { a: 2 }, { a: 3 }]);
  await interp.run(`'a' |REC@`);

  expect(interp.stack_pop()).toEqual([1, 2, 3]);

  interp.stack_push([{ a: { b: 1 } }, { a: { b: 2 } }, { a: { b: 3 } }]);
  await interp.run(`['a' 'b'] |REC@`);

  expect(interp.stack_pop()).toEqual([1, 2, 3]);
});

test("MAX of two numbers", async () => {
  interp.stack_push(4);
  interp.stack_push(18);
  await interp.run("MAX");
  expect(interp.stack_pop()).toEqual(18);
});

test("MAX of an array of numbers", async () => {
  interp.stack_push([14, 8, 55, 4, 5]);
  await interp.run("MAX");
  expect(interp.stack_pop()).toEqual(55);
});

test("MIN of two numbers", async () => {
  interp.stack_push(4);
  interp.stack_push(18);
  await interp.run("MIN");
  expect(interp.stack_pop()).toEqual(4);
});

test("MIN of an array of numbers", async () => {
  interp.stack_push([14, 8, 55, 4, 5]);
  await interp.run("MIN");
  expect(interp.stack_pop()).toEqual(4);
});

test("MEAN of an array of numbers", async () => {
  interp.stack_push([1, 2, 3, 4, 5]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual(3);
});

test("MEAN of an array of letters", async () => {
  interp.stack_push(["a", "a", "b", "c"]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual({ a: 0.5, b: 0.25, c: 0.25 });
});

test("MEAN of an array of numbers with null values", async () => {
  interp.stack_push([1, 2, 3, null, 4, undefined, 5]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual(3);
});

test("MEAN of an array of letters with null values", async () => {
  // Ignore null values
  interp.stack_push(["a", "a", undefined, "b", null, "c"]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual({ a: 0.5, b: 0.25, c: 0.25 });
});

test("MEAN of an array of objects", async () => {
  interp.stack_push([
    { a: 1, b: 0 },
    { a: 2, b: 0 },
    { a: 3, b: 0 },
  ]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual({ a: 2, b: 0 });
});

test("MEAN of an array of objects with some numbers and some strings", async () => {
  interp.stack_push([
    { a: 0 },
    { a: 1, b: "To Do" },
    { a: 2, b: "To Do" },
    { a: 3, b: "In Progress" },
    { a: 4, b: "Done" },
  ]);
  await interp.run("MEAN");
  expect(interp.stack_pop()).toEqual({
    a: 2,
    b: { "To Do": 0.5, "In Progress": 0.25, Done: 0.25 },
  });
});

test("RANGE-INDEX on boundary", async () => {
  const value1 = 5;
  const start_ranges = [0, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(1);
});

test("RANGE-INDEX before first value", async () => {
  const value1 = -5;
  const start_ranges = [0, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(null);
});

test("RANGE-INDEX equals first value", async () => {
  const value1 = 0;
  const start_ranges = [0, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(0);
});

test("RANGE-INDEX in between values", async () => {
  const value1 = 3;
  const start_ranges = [0, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(0);
});

test("RANGE-INDEX after last value", async () => {
  const value1 = 25;
  const start_ranges = [0, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(3);
});

test("RANGE-INDEX with -Infinity value", async () => {
  const value1 = -15;
  const start_ranges = [-Infinity, 5, 10, 20];
  interp.stack_push(value1);
  interp.stack_push(start_ranges);
  await interp.run("RANGE-INDEX");
  const res1 = interp.stack_pop();
  expect(res1).toEqual(0);
});

test("DIVIDE", async () => {
  interp.stack_push(10);
  interp.stack_push(2);
  await interp.run("DIVIDE");
  expect(interp.stack_pop()).toEqual(5);
});

test("ADD-DAYS", async () => {
  await interp.run("MONDAY");
  interp.stack_push(0);
  await interp.run("ADD-DAYS");
  const res1 = interp.stack_pop();
  console.log("ADD-DAYS", res1);
});


test("Unknown screen", async () => {
  try {
    await interp.run("'garbage' LOAD-SCREEN");
  } catch (e) {
    expect(e).toBeInstanceOf(WordExecutionError);
    const root_error = e.getError();
    expect(root_error).toBeInstanceOf(UnknownScreenError);
    expect(root_error.getScreenName()).toEqual("garbage");
  }
})

test("Unknown word", async () => {
  try {
    await interp.run("GARBAGE");
  } catch (e) {
    expect(e).toBeInstanceOf(UnknownWordError);
    expect(e.getWord()).toEqual("GARBAGE");
  }
})

test("Unknown module", async () => {
  try {
    await interp.run("['garbage'] USE-MODULES");
  } catch (e) {
    expect(e).toBeInstanceOf(WordExecutionError);
    const root_error = e.getError();
    expect(root_error).toBeInstanceOf(UnknownModuleError);
    expect(root_error.getModuleName()).toEqual("garbage");
  }
})

test ("Stack underflow", async () => {
  try {
    await interp.run("POP");
  } catch (e) {
    expect(e).toBeInstanceOf(WordExecutionError);
    const root_error = e.getError();
    expect(root_error).toBeInstanceOf(StackUnderflowError);
  }
})

test ("Missing semicolon", async () => {
  try {
    await interp.run(": UNFINISHED   1 2 3  : NEW-WORD 'howdy' ;");
  } catch (e) {
    expect(e).toBeInstanceOf(MissingSemicolonError);
  }

  try {
    await interp.run("@: UNFINISHED   1 2 3  : NEW-WORD 'howdy' ;");
  } catch (e) {
    expect(e).toBeInstanceOf(MissingSemicolonError);
  }

})

test ("Extra semicolon error", async () => {
  try {
    await interp.run("1 2 3 ;");
  } catch (e) {
    expect(e).toBeInstanceOf(ExtraSemicolonError);
  }
})

