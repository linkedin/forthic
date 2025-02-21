import { Interpreter } from "../interpreter";

describe("Interpreter.streamingRun", () => {
  let interp: Interpreter;

  beforeEach(() => {
    interp = new Interpreter();
  });

  test("general streaming test", async () => {
    const items: (string | { stringDelta: string })[] = [];

    // First chunk with START_LOG and beginning of string
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown`,
      false,
    )) {
      items.push(delta);
    }

    // Add more to the string
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps`,
      false,
    )) {
      items.push(delta);
    }

    // Complete the string and END_LOG
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG`,
      false,
    )) {
      items.push(delta);
    }

    // Add UPPER (split across chunks)
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG UPPER`,
      false,
    )) {
      items.push(delta);
    }

    // Complete with CASE
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG UPPERCASE`,
      true,
    )) {
      items.push(delta);
    }

    expect(items).toEqual([
      { stringDelta: "The quick brown" },
      { stringDelta: " fox jumps" },
      "The quick brown fox jumps over",
    ]);

    expect(interp.get_stack()).toEqual(["THE QUICK BROWN FOX JUMPS OVER"]);
  });

  test("executes complete tokens and skips the last incomplete token (done=false)", async () => {
    // Imagine the Forthic code "1 2 +"
    // When the code is not final (done=false), streamingRun should execute only "1" and "2"
    const gen = interp.streamingRun("1 2 +", true);
    await gen.next();

    // The literal handlers should push numeric values.
    // In this case only tokens for "1" and "2" were executed.
    expect(interp.get_stack()).toEqual([3]);
  });

  test("executes the final token when done flag is true", async () => {
    // First call: pass the incomplete code. Only tokens "1" and "2" will execute.
    const gen1 = interp.streamingRun("1 2 +", false);
    await gen1.next();
    expect(interp.get_stack()).toEqual([1, 2]);

    // Second call: pass the full code, and now indicate that the stream is done.
    // The final plus token is now executed – which pops 1 and 2 and pushes 3.
    const gen2 = interp.streamingRun("1 2 +", true);
    await gen2.next();

    expect(interp.get_stack()).toEqual([3]);
  });

  test("executes only new tokens between calls", async () => {
    // Simulate streaming where each call gets the full code so far.
    // First, the interpreter sees only "1".
    const gen1 = interp.streamingRun("1", false);
    await gen1.next();
    expect(interp.get_stack()).toEqual([]);

    // Then, the code grows to "1 2". The new token "2" is executed.
    const gen2 = interp.streamingRun("1 2", false);
    await gen2.next();
    expect(interp.get_stack()).toEqual([1]);

    // Finally, the full code "1 2 +" is provided, and done=true executes the final token (plus).
    const gen3 = interp.streamingRun("1 2 +", true);
    await gen3.next();
    // With the plus executed, 1 and 2 are replaced by 3.
    expect(interp.get_stack()).toEqual([3]);
  });

  test("streaming complex arithmetic", async () => {
    const gen1 = interp.streamingRun("1 2 + 3", false);
    await gen1.next();
    expect(interp.get_stack()).toEqual([3]);

    const gen2 = interp.streamingRun("1 2 + 3 +", false);
    await gen2.next();
    expect(interp.get_stack()).toEqual([3, 3]);

    const gen3 = interp.streamingRun("1 2 + 3 + 4 + 5 + 2 * 4 -", true);
    await gen3.next();
    expect(interp.get_stack()).toEqual([26]);
  });

  test("yields string deltas between START_LOG and END_LOG", async () => {
    const items: ({ stringDelta: string } | string)[] = [];

    for await (const fullValue of interp.streamingRun(
      `123 START_LOG "hello world" END_LOG`,
      true,
    )) {
      items.push(fullValue);
    }

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world`,
    )) {
      items.push(delta);
    }

    //123 STR

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world how`,
    )) {
      items.push(delta);
    }

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world how are you`,
    )) {
      items.push(delta);
    }

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world how are you doing today"`,
      true,
    )) {
      items.push(delta);
    }

    expect(items).toEqual([
      "hello world",
      { stringDelta: "hello world" },
      { stringDelta: " how" },
      { stringDelta: " are you" },
      "hello world how are you doing today",
    ]);
  });

  test("multiple START_LOG/END_LOG blocks", async () => {
    const deltas: ({ stringDelta: string } | string)[] = [];

    for await (const delta of interp.streamingRun(
      `START_LOG "first" END_LOG 123 START_LOG "second" END_LOG`,
      true,
    )) {
      deltas.push(delta);
    }
    expect(deltas).toEqual(["first", "second"]);
  });

  test("handles split words across streaming chunks", async () => {
    const items: ({ stringDelta: string } | string)[] = [];

    // First chunk with START_LOG and beginning of string
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown`,
      false,
    )) {
      items.push(delta);
    }

    // Add more to the string
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps`,
      false,
    )) {
      items.push(delta);
    }

    // Complete the string and END_LOG
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG`,
      false,
    )) {
      items.push(delta);
    }

    // Add UPPER (split across chunks)
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG UPPER`,
      false,
    )) {
      items.push(delta);
    }

    // Complete with CASE
    for await (const delta of interp.streamingRun(
      `START_LOG "The quick brown fox jumps over" END_LOG UPPERCASE`,
      true,
    )) {
      items.push(delta);
    }

    expect(items).toEqual([
      { stringDelta: "The quick brown" },
      { stringDelta: " fox jumps" },
      "The quick brown fox jumps over",
    ]);

    expect(interp.get_stack()).toEqual(["THE QUICK BROWN FOX JUMPS OVER"]);
  });
});
