import { UnterminatedStringError } from "../tokenizer";
import { Interpreter } from "../interpreter";
import { Module } from "../module";
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

    expect(interp.get_stack().get_items()).toEqual([
      "THE QUICK BROWN FOX JUMPS OVER",
    ]);
  });

  test("multi-line stream ending with word execution using incremental streaming", async () => {
    const interp = new Interpreter();

    // Define the EMAIL word
    await interp.run(': EMAIL   "email called";');

    // Create multi-line input ending with EMAIL
    const input = `"""Create a new email to test@test.com with subject 'A Haiku About Life' and body:

Morning sun rises
Life flows like gentle river
Night brings peaceful rest""" EMAIL`;

    // Simulate streaming by calling streamingRun with increasing substrings
    for (let i = 1; i <= input.length; i++) {
      const substring = input.substring(0, i);
      const isDone = i === input.length; // Only mark as done on the final iteration

      const streamGenerator = interp.streamingRun(substring, isDone);

      // Exhaust the generator for this chunk
      for await (const _ of streamGenerator) {
        // We don't need to do anything with the yield values in this test
      }

      // If we're done, verify the EMAIL word was executed
      if (isDone) {
        const stackItems = interp.get_stack().get_items();
        expect(stackItems.length).toBe(2);
        const top_item = interp.stack_pop();
        expect(top_item).toEqual("email called");
      }
    }
  });

  test("executes complete tokens and skips the last incomplete token (done=false)", async () => {
    // Imagine the Forthic code "1 2 +"
    // When the code is not final (done=false), streamingRun should execute only "1" and "2"
    const gen = interp.streamingRun("1 2 +", true);
    await gen.next();

    // The literal handlers should push numeric values.
    // In this case only tokens for "1" and "2" were executed.
    expect(interp.get_stack().get_items()).toEqual([3]);
  });

  test("executes the final token when done flag is true", async () => {
    // First call: pass the incomplete code. Only tokens "1" and "2" will execute.
    const gen1 = interp.streamingRun("1 2 +", false);
    await gen1.next();
    expect(interp.get_stack().get_items()).toEqual([1, 2]);

    // Second call: pass the full code, and now indicate that the stream is done.
    // The final plus token is now executed – which pops 1 and 2 and pushes 3.
    const gen2 = interp.streamingRun("1 2 +", true);
    await gen2.next();

    expect(interp.get_stack().get_items()).toEqual([3]);
  });

  test("executes only new tokens between calls", async () => {
    // Simulate streaming where each call gets the full code so far.
    // First, the interpreter sees only "1".
    const gen1 = interp.streamingRun("1", false);
    await gen1.next();
    expect(interp.get_stack().get_items()).toEqual([]);

    // Then, the code grows to "1 2". The new token "2" is executed.
    const gen2 = interp.streamingRun("1 2", false);
    await gen2.next();
    expect(interp.get_stack().get_items()).toEqual([1]);

    // Finally, the full code "1 2 +" is provided, and done=true executes the final token (plus).
    const gen3 = interp.streamingRun("1 2 +", true);
    await gen3.next();
    // With the plus executed, 1 and 2 are replaced by 3.
    expect(interp.get_stack().get_items()).toEqual([3]);
  });

  test("streaming complex arithmetic", async () => {
    const gen1 = interp.streamingRun("1 2 + 3", false);
    await gen1.next();
    expect(interp.get_stack().get_items()).toEqual([3]);

    const gen2 = interp.streamingRun("1 2 + 3 +", false);
    await gen2.next();
    expect(interp.get_stack().get_items()).toEqual([3, 3]);

    const gen3 = interp.streamingRun("1 2 + 3 + 4 + 5 + 2 * 4 -", true);
    await gen3.next();
    expect(interp.get_stack().get_items()).toEqual([26]);
  });

  test("streaming MAP", async () => {
    const gen = interp.streamingRun(`[1 2 3] `, false);
    await gen.next();
    expect(interp.stack_peek()).toEqual(3);

    const gen2 = interp.streamingRun(`[1 2 3] "2 *"`, false);
    await gen2.next();
    expect(interp.stack_peek()).toEqual([1, 2, 3]);

    const gen3 = interp.streamingRun(`[1 2 3] "2 *" MAP`, false);
    await gen3.next();
    expect(interp.stack_peek()).toEqual("2 *");

    const gen4 = interp.streamingRun(`[1 2 3] "2 *" MAP`, true);
    await gen4.next();
    expect(interp.stack_peek()).toEqual([2, 4, 6]);
  });

  test("streaming MAP with module", async () => {
    const myInterp = new Interpreter([new SampleModule()]);
    const gen = myInterp.streamingRun(`[1 2 3] "SEND-EMAIL"`, false);
    await gen.next();
    expect(myInterp.stack_peek()).toEqual([1, 2, 3]);

    const gen2 = myInterp.streamingRun(`[1 2 3] "SEND-EMAIL" MAP`, true);
    await gen2.next();
    expect(myInterp.stack_peek()).toEqual(["sent 1", "sent 2", "sent 3"]);
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
      false,
    )) {
      items.push(delta);
    }

    //123 STR

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world how`,
      false,
    )) {
      items.push(delta);
    }

    for await (const delta of interp.streamingRun(
      `123 START_LOG "hello world how are you`,
      false,
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

    expect(interp.get_stack().get_items()).toEqual([
      "THE QUICK BROWN FOX JUMPS OVER",
    ]);
  });

  test("START_LOG/END_LOG with numbers", async () => {
    const deltas: ({ stringDelta: string } | string)[] = [];

    for await (const delta of interp.streamingRun(
      "START_LOG 1 2 3 END_LOG",
      true,
    )) {
      deltas.push(delta);
    }
    expect(deltas).toEqual(["1", "2", "3"]);
    const val = interp.stack_pop();
    expect(val).toEqual(3);
    expect(interp.get_stack().get_items()).toEqual([1, 2]);
  });

  test("START_LOG/END_LOG with objects", async () => {
    const deltas: ({ stringDelta: string } | string)[] = [];

    for await (const delta of interp.streamingRun(
      `START_LOG [["key with space" 1] ["other key with space" 2] ] REC END_LOG`,
      true,
    )) {
      deltas.push(delta);
    }

    expect(deltas).toEqual([
      "[",
      "[",
      "key with space",
      "1",
      "]",
      "[",
      "other key with space",
      "2",
      "]",
      "]",
      "REC",
    ]);
    const val = interp.stack_pop();
    expect(val).toEqual({ "key with space": 1, "other key with space": 2 });
  });
});

test("Unterminated string", async () => {
  const interp = new Interpreter();
  const gen = interp.streamingRun(`''''`, false);
  await gen.next();

  const gen2 = interp.streamingRun(`''''`, true);
  await expect(gen2.next()).rejects.toThrow(UnterminatedStringError);
});


test("Nested string issue", async () => {
  // Won't throw an error because we're not done yet
  let interp = new Interpreter();
  const gen = interp.streamingRun(`"""Reply saying "Thanks`, false);
  await gen.next();

  // Should now handle nested quotes correctly (no longer throws error)
  interp = new Interpreter();
  const gen2 = interp.streamingRun(`"""Reply saying "Thanks""""`, true);
  const result = await gen2.next();
  expect(result.done).toBe(true);
});

class SampleModule extends Module {
  constructor() {
    super("sample");
    this.add_module_word("SEND-EMAIL", this.word_SEND_EMAIL.bind(this));
  }

  // ( email -- )
  async word_SEND_EMAIL(interp: Interpreter) {
    const email = interp.stack_pop();
    interp.stack_push(`sent ${email}`);
  }
}
