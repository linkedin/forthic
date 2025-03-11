import { Interpreter, Stack } from "../interpreter";
import { Module } from "../module";
import { PositionedString } from "../tokenizer";
test("Initial state", () => {
  const interp = new Interpreter();
  expect((interp as any).stack).toEqual([]);
  expect(interp.cur_module().name).toEqual("");
});

test("Push string", async () => {
  const interp = new Interpreter();
  await interp.run("'Howdy'");
  expect(interp.stack_pop()).toEqual("Howdy");
});

test("Comment", async () => {
  const interp = new Interpreter();
  await interp.run("# A comment");
  await interp.run("#A comment");
  expect((interp as any).stack.length).toBe(0);
});

test("Empty array", async () => {
  const interp = new Interpreter();
  await interp.run("[]");
  expect(interp.stack_pop()).toEqual([]);
});

test("Start module", async () => {
  let interp = new Interpreter() as any;

  // Push application module onto module stack
  await interp.run("{");
  expect(interp.module_stack.length).toBe(2);
  expect(interp.module_stack[0]).toBe(interp.module_stack[1]);

  // Push module-A onto module stack
  interp = new Interpreter();
  await interp.run("{module-A");
  expect(interp.module_stack.length).toBe(2);
  expect(interp.module_stack[1].name).toBe("module-A");
  expect(interp.app_module.modules["module-A"]).not.toBeNull();

  // Push module-A and then module-B onto module stack
  interp = new Interpreter();
  await interp.run("{module-A {module-B");
  expect(interp.module_stack.length).toBe(3);
  expect(interp.module_stack[1].name).toBe("module-A");
  expect(interp.module_stack[2].name).toBe("module-B");

  const module_A = interp.app_module.modules["module-A"];
  expect(module_A.modules["module-B"]).not.toBeNull();

  await interp.run("}}");
  expect(interp.module_stack.length).toBe(1);
  expect(interp.module_stack[0]).toBe(interp.app_module);
});


test("Definition", async () => {
  // Can define and find a word in the app module
  let interp = new Interpreter();
  await interp.run(": NOTHING   ;");
  let word = (interp as any).app_module.find_word("NOTHING");
  expect(word).not.toBeNull();

  // Words defined in other modules aren't automatically available in the app module
  interp = new Interpreter();
  await interp.run("{module-A   : NOTHING   ;}");
  word = (interp as any).app_module.find_word("NOTHING");
  expect(word).toBeNull();

  const module_A = (interp as any).app_module.modules["module-A"];
  word = module_A.find_word("NOTHING");
  expect(word).not.toBeNull();
});


test("Memo", async () => {
  const interp = new Interpreter();
  const interp_any = interp as any;

  await interp.run("@: MY-MEMO   ;");
  for (const name of ["MY-MEMO", "MY-MEMO!", "MY-MEMO!@"]) {
    const word = interp_any.app_module.find_word(name);
    expect(word).not.toBeNull();
  }

  // Test storing a value and retrieving it
  await interp.run("41 MY-MEMO");
  await interp.run("MY-MEMO");
  expect(interp.stack_pop()).toBe(41);

  // Test refreshing a value
  interp_any.stack = [];
  await interp.run("81 MY-MEMO!");
  expect(interp_any.stack.length).toBe(0);
  await interp.run("MY-MEMO");
  expect(interp.stack_pop()).toBe(81);

  // Test !@
  interp_any.stack = [];
  await interp.run("101 MY-MEMO!@");
  expect(interp_any.stack.length).toBe(1);
  expect(interp.stack_pop()).toBe(101);
  interp_any.stack = [];
  await interp.run("MY-MEMO");
  expect(interp.stack_pop()).toBe(101);
});


test("Word scope", async () => {
  const interp = new Interpreter();
  await interp.run(`
    : APP-MESSAGE   "Hello (from app)";
    {module1
        APP-MESSAGE
    }
  `);
  expect(interp.stack_pop()).toBe("Hello (from app)");
});


test("Open module - Test word", async () => {
  const interp = new Interpreter();
  await interp.run(`
    {mymodule
       : MESSAGE   "Hello (from mymodule)";
    }
    : MESSAGE   {mymodule MESSAGE };
    MESSAGE
  `);
  expect(interp.stack_pop()).toBe("Hello (from mymodule)");
});

test("Open module - Test memo", async () => {
  const interp = new Interpreter();
  await interp.run(`
    {mymodule
       @: MESSAGE-MEMO   "Hello (from mymodule memo)";
    }
    : MESSAGE   {mymodule MESSAGE-MEMO };
    MESSAGE
  `);
  expect(interp.stack_pop()).toBe("Hello (from mymodule memo)");
});


test("Word", async () => {
  let interp = new Interpreter();
  await interp.run(": MESSAGE   'Howdy' ;");
  await interp.run("MESSAGE");
  expect(interp.stack_pop()).toBe("Howdy");

  interp = new Interpreter();
  await interp.run("{module-A {module-B   : MESSAGE   'In module-B' ;}}");
  await interp.run("{module-A {module-B   MESSAGE}}");
  expect(interp.stack_pop()).toBe("In module-B");
});

test("Search global module", async () => {
  const interp = new Interpreter();
  await interp.run("'Hi'");
  expect((interp as any).stack.length).toBe(1);
  await interp.run("POP");
  expect((interp as any).stack.length).toBe(0);
});


test("Use modules", async () => {
  const interp = new Interpreter();
  interp.register_module(new TestModule());
  interp.use_modules(["test"]);
  await interp.run("test.TEST");
  expect(interp.stack_pop()).toBe("TEST called");

  // Test with prefix
  const interp2 = new Interpreter();
  interp2.register_module(new TestModule());
  interp2.use_modules([["test", ""]]);
  await interp2.run("TEST");
  expect(interp2.stack_pop()).toBe("TEST called");
});

test("Import module", async () => {
  // Test without prefix
  const interp = new Interpreter();
  const test_module = new TestModule();
  interp.import_module(test_module);
  await interp.run("TEST");
  expect(interp.stack_pop()).toBe("TEST called");

  // Test with prefix
  const interp2 = new Interpreter();
  const test_module2 = new TestModule();
  interp2.import_module(test_module2, "t");
  await interp2.run("t.TEST");
  expect(interp2.stack_pop()).toBe("TEST called");

  // Test with module name as prefix
  const interp3 = new Interpreter();
  const test_module3 = new TestModule();
  interp3.import_module(test_module3, test_module3.get_name());
  await interp3.run("test.TEST");
  expect(interp3.stack_pop()).toBe("TEST called");
});

test("Stack", async () => {
  const value = {a: 1, b: 2};
  const stack = new Stack([value]);
  expect(stack.length).toBe(1);
  expect(JSON.stringify(stack)).toBe(JSON.stringify([value]));
  expect(stack.pop()).toBe(value);
  expect(stack.length).toBe(0);
  stack.push(value);
  expect(stack.length).toBe(1);
  expect(JSON.stringify(stack)).toBe(JSON.stringify([value]));
});

test("Stack raw items", async () => {
  const value = new PositionedString("Hello", {line: 1, column: 1, screen_name: "test", start_pos: 0, end_pos: 5});
  const stack = new Stack([value]);
  expect(stack.get_raw_items()).toEqual([value]);
  expect(stack.get_items()).toEqual(["Hello"]);
});



test("Construct interpreter with modules", async () => {
  const interp = new Interpreter([new TestModule()]);
  await interp.run("TEST");
  expect(interp.stack_pop()).toBe("TEST called");
});

// ===== Sample modules =================================================================

class TestModule extends Module {
  constructor() {
    super("test");

    this.add_module_word("TEST", this.word_TEST.bind(this));
  }

  // ( -- message )
  async word_TEST(_interp: Interpreter) {
    _interp.stack_push("TEST called");
    // console.log("TEST called", _interp === this.get_interp());
  }
}