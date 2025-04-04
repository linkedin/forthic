import { Tokenizer, CodeLocation, InvalidWordNameError, UnterminatedStringError } from "../tokenizer";

test("Knows token positions", () => {
  const main_forthic = `
    : ADD-ONE   1 23 +;
    {module
        # 2 ADD-ONE
    }
    @: MY-MEMO   [ "hello" '''triple-single-quoted-string'''];
    `;

  const reference_location = new CodeLocation({
    screen_name: "main",
    line: 1,
    column: 1,
    start_pos: 0,
  });

  const tokenizer = new Tokenizer(main_forthic, reference_location);

  // TOK_START_DEF
  const begin_def = tokenizer.next_token();
  expect(begin_def.location).toEqual({
    line: 2,
    column: 7,
    screen_name: "main",
    start_pos: 7,
    end_pos: 14,
  });

  // TOK_WORD: 1
  const one_token = tokenizer.next_token();
  expect(one_token.location).toEqual({
    line: 2,
    column: 17,
    screen_name: "main",
    start_pos: 17,
    end_pos: 18,
  });

  // TOK_WORD: 23
  const token_23 = tokenizer.next_token();
  expect(token_23.location).toEqual({
    line: 2,
    column: 19,
    screen_name: "main",
    start_pos: 19,
    end_pos: 21,
  });

  // TOK_WORD: +
  const plus_token = tokenizer.next_token();
  expect(plus_token.location).toEqual({
    line: 2,
    column: 22,
    screen_name: "main",
    start_pos: 22,
    end_pos: 23,
  });

  // TOK_END_DEF
  const end_def_token = tokenizer.next_token();
  expect(end_def_token.location).toEqual({
    line: 2,
    column: 23,
    screen_name: "main",
    start_pos: 23,
    end_pos: 24,
  });

  // TOK_START_MODULE
  const module_start_token = tokenizer.next_token();
  expect(module_start_token.location).toEqual({
    line: 3,
    column: 6,
    screen_name: "main",
    start_pos: 30,
    end_pos: 36,
  });

  // TOK_COMMENT
  const comment_token = tokenizer.next_token();
  expect(comment_token.location).toEqual({
    line: 4,
    column: 10,
    screen_name: "main",
    start_pos: 46,
    end_pos: 57,
  });

  // TOK_END_MODULE
  const end_module_token = tokenizer.next_token();
  expect(end_module_token.location).toEqual({
    line: 5,
    column: 5,
    screen_name: "main",
    start_pos: 61,
    end_pos: 62,
  });

  // TOK_START_MEMO
  const start_memo_token = tokenizer.next_token();
  expect(start_memo_token.location).toEqual({
    line: 6,
    column: 8,
    screen_name: "main",
    start_pos: 70,
    end_pos: 77,
  });

  // TOK_START_ARRAY
  const start_array_token = tokenizer.next_token();
  expect(start_array_token.location).toEqual({
    line: 6,
    column: 18,
    screen_name: "main",
    start_pos: 80,
    end_pos: 81,
  });

  // TOK_STRING
  const start_string_token = tokenizer.next_token();
  expect(start_string_token.location).toEqual({
    line: 6,
    column: 21,
    screen_name: "main",
    start_pos: 83,
    end_pos: 88,
  });

  // TOK_STRING
  const start_triple_string_token = tokenizer.next_token();
  expect(start_triple_string_token.location).toEqual({
    line: 6,
    column: 31,
    screen_name: "main",
    start_pos: 93,
    end_pos: 120,
  });

  // TOK_END_ARRAY,
  const end_array_token = tokenizer.next_token();
  expect(end_array_token.location).toEqual({
    line: 6,
    column: 61,
    screen_name: "main",
    start_pos: 123,
    end_pos: 124,
  });
});

test("Knows token location in ad hoc string given reference", () => {
  const reference_location = new CodeLocation({
    screen_name: "main",
    line: 21,
    column: 15,
    start_pos: 67,
  });

  const main_forthic = "'key' REC@ LOWERCASE";

  const tokenizer = new Tokenizer(main_forthic, reference_location);
  let token;

  // 'key'
  token = tokenizer.next_token();
  expect(token.string).toEqual("key");
  expect(token.location).toEqual({
    line: 21,
    column: 16,
    screen_name: "main",
    start_pos: 68,
    end_pos: 71,
  });

  // REC@
  token = tokenizer.next_token();
  expect(token.string).toEqual("REC@");
  expect(token.location).toEqual({
    line: 21,
    column: 21,
    screen_name: "main",
    start_pos: 73,
    end_pos: 77,
  });

  // LOWERCASE
  token = tokenizer.next_token();
  expect(token.string).toEqual("LOWERCASE");
  expect(token.location).toEqual({
    line: 21,
    column: 26,
    screen_name: "main",
    start_pos: 78,
    end_pos: 87,
  });
});


test("Invalid word name", () => {
  const reference_location = new CodeLocation({
    screen_name: "main",
    line: 1,
    column: 1,
    start_pos: 0,
  });

  const main_forthic = ": John's-Word   1 23 +;";

  try {
    const tokenizer = new Tokenizer(main_forthic, reference_location);
    tokenizer.next_token();
  }
  catch (e) {
    expect(e).toBeInstanceOf(InvalidWordNameError);
  }
});


test("Unterminated string", () => {
  const reference_location = new CodeLocation({
    screen_name: "main",
    line: 1,
    column: 1,
    start_pos: 0,
  });

  const main_forthic = "'key";

  try {
    const tokenizer = new Tokenizer(main_forthic, reference_location);
    tokenizer.next_token();
  }
  catch (e) {
    expect(e).toBeInstanceOf(UnterminatedStringError);
    console.log(e.message);
  }
})