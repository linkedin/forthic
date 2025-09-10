import { Interpreter } from "../interpreter";

test("Dot symbols work as strings in interpreter", async () => {
  const interp = new Interpreter();

  // Test that dot symbols are pushed as strings onto the stack
  await interp.run(".symbol .test-123");

  const stack = interp.get_stack().get_items();
  expect(stack).toEqual(["symbol", "test-123"]);
});

test("Short dot symbols (.s, .S) still work as existing words", async () => {
  const interp = new Interpreter();

  // These should trigger the debugging words, not be treated as symbols
  let error1: Error | null = null;
  let error2: Error | null = null;

  try {
    await interp.run("42 .s");
  } catch (e) {
    error1 = e as Error;
  }

  try {
    await interp.run("1 2 .S");
  } catch (e) {
    error2 = e as Error;
  }

  // Both should throw IntentionalStopError, indicating they executed as words
  expect(error1?.name).toEqual("IntentionalStopError");
  expect(error2?.name).toEqual("IntentionalStopError");
});

test("Dot symbols mixed with regular tokens", async () => {
  const interp = new Interpreter();

  await interp.run('[ .key1 "value1" .key2 "value2" ]');

  const stack = interp.get_stack().get_items();
  expect(stack).toEqual([["key1", "value1", "key2", "value2"]]);
});

test("Minimum length boundary cases", async () => {
  const interp = new Interpreter();

  // .ab should be a dot symbol (minimum length)
  await interp.run(".ab");
  expect(interp.get_stack().get_items()).toEqual(["ab"]);

  // Clear stack
  interp.get_stack().get_raw_items().length = 0;

  // .a should be treated as a word (will cause UnknownWordError since it doesn't exist)
  let error: Error | null = null;
  try {
    await interp.run(".a");
  } catch (e) {
    error = e as Error;
  }

  expect(error?.name).toEqual("UnknownWordError");

  error = null
  try {
    await interp.run(".");
  } catch (e) {
    error = e as Error;
  }

  expect(error?.name).toEqual("UnknownWordError");
});
