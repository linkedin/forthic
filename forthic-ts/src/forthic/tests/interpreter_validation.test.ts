import { Interpreter } from "../interpreter";
import { UnknownWordError, MissingSemicolonError } from "../errors";
import { UnterminatedStringError } from "../tokenizer";

test("Initial state", () => {
  const interp = new Interpreter();
  expect((interp as any).stack).toEqual([]);
  expect(interp.cur_module().name).toEqual("");
  expect(interp.get_validation_mode()).toBe(false);
});

test("Unknown word", async () => {
  const interp = new Interpreter();
  interp.set_validation_mode(true);
  await expect(interp.run("UNKNOWN")).rejects.toThrow(UnknownWordError);
});

test("Missing semicolon", async () => {
  const interp = new Interpreter();
  interp.set_validation_mode(true);
  await expect(interp.run(": UNKNOWN  : HOWDY  ;")).rejects.toThrow(MissingSemicolonError);
});


test("Unterminated string", async () => {
  const interp = new Interpreter();
  interp.set_validation_mode(true);
  await expect(interp.run("'Hello")).rejects.toThrow(UnterminatedStringError);
});
