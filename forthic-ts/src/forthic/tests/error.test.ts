import { Interpreter } from "../interpreter";
import { Module } from "../module";
import { ForthicError, UnknownWordError, WordExecutionError, MissingSemicolonError, ExtraSemicolonError, get_error_description, StackUnderflowError } from "../errors";

const PRINT_ERRORS = false;

function printError(e: Error) {
  if (!PRINT_ERRORS) return;

  if (e instanceof ForthicError) {
    console.log(e.getDescription())
  }
  else {
    console.log(e.message);
  }
}

test("Unknown word error", async () => {
  const interp = new Interpreter();
  const forthic = "1 2 3 GARBAGE +"
  try {

    await interp.run(forthic);
  } catch (e) {
    if (e instanceof UnknownWordError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
});

test("Stack underflow error", async () => {
  const interp = new Interpreter();
  const forthic = "1 + 3 2 *"
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof WordExecutionError && e.getError() instanceof StackUnderflowError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
});

test("Word execution error", async () => {
  const interp = new Interpreter();
  const forthic = `: ADD   +;
  1 ADD 2 *`
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof WordExecutionError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
});

test("Missing semicolon error", async () => {
  const interp = new Interpreter();
  const forthic = `: ADD   +
  : MUL   *;`
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof MissingSemicolonError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
});

test("Extra semicolon error", async () => {
  const interp = new Interpreter();
  const forthic = `: ADD   +;
[1 2 3];`
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof ExtraSemicolonError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
});


test("Errors in MAP", async () => {
  const interp = new Interpreter();
  const forthic = `: ADD   +;
  [1 2 3] "ADD" MAP`
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof ForthicError) {
      printError(e);
    }
    else {
      throw e;
    }
  }
})

// TODO: See if we can wrap the execution of a word in a ForthicError

test("Module error", async () => {
  const interp = new Interpreter();
  interp.register_module(new TestModule(interp));
  await interp.run(`[ "test" ] USE-MODULES`)
  const forthic = `
  [1] "test.TEST" MAP 2 *`
  try {
    await interp.run(forthic);
  } catch (e) {
    if (e instanceof ForthicError) {
      printError(e);
    }
    else {
      const description = get_error_description(forthic, e);
      console.log(description);
    }
  }
});



// Module that throws an error
class TestModule extends Module {
  constructor(interp: Interpreter) {
    super("test", interp);

    this.add_module_word("TEST", this.word_TEST.bind(this));
  }

  // ( -- )
  async word_TEST(_interp: Interpreter) {
    throw new Error("Something horrible happened");
  }
}