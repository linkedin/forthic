import { Tokenizer, CodeLocation, InvalidWordNameError, UnterminatedStringError, TokenType } from "../tokenizer";

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

describe("Triple quote string with nested quotes", () => {
  const reference_location = new CodeLocation({
    screen_name: "test",
    line: 1,
    column: 1,
    start_pos: 0,
  });

  test("Basic case: '''I said 'Hello''''", () => {
    const input = "'''I said 'Hello''''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual("I said 'Hello'");
  });

  test("Normal triple quote behavior (no 4+ consecutive quotes)", () => {
    const input = "'''Hello'''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual("Hello");
  });

  test("Double quotes with greedy mode", () => {
    const input = '"""I said "Hello""""';
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual('I said "Hello"');
  });

  test("Six consecutive quotes (empty string case)", () => {
    const input = "''''''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual("");
  });

  test("Eight consecutive quotes (two quote content)", () => {
    const input = "''''''''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual("''");
  });

  test("Multiple nested quotes", () => {
    const input = `"""He said "I said 'Hello' to you""""`;
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual(`He said "I said 'Hello' to you"`);
  });

  test("No greedy mode when triple quote not followed by quote", () => {
    const input = "'''Hello''' world'''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    // Should close at first ''' since it's not followed by another quote
    expect(token.string).toEqual("Hello");
  });

  test("Content with apostrophes (contractions)", () => {
    const input = "'''It's a beautiful day, isn't it?''''";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.string).toEqual("It's a beautiful day, isn't it?'");
  });

  test("Mixed quote types don't trigger greedy mode", () => {
    const input = "'''Hello\"\"\"";
    const tokenizer = new Tokenizer(input, reference_location);
    
    try {
      tokenizer.next_token();
    } catch (e) {
      expect(e).toBeInstanceOf(UnterminatedStringError);
    }
  });

  test("Backward compatibility: normal strings unchanged", () => {
    const inputs = [
      "'''simple'''",
      "'''multi\nline\nstring'''",
      "'''string with \"double quotes\"'''",
      "'''string with 'single quotes'''''"
    ];
    
    const expected = [
      "simple",
      "multi\nline\nstring", 
      'string with "double quotes"',
      "string with 'single quotes''"
    ];
    
    inputs.forEach((input, i) => {
      const tokenizer = new Tokenizer(input, reference_location);
      const token = tokenizer.next_token();
      expect(token.string).toEqual(expected[i]);
    });
  });
});

describe("Dot symbol tokenization", () => {
  const reference_location = new CodeLocation({
    screen_name: "test",
    line: 1,
    column: 1,
    start_pos: 0,
  });

  test("Basic dot symbol: .symbol", () => {
    const input = ".symbol";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token.string).toEqual("symbol");
  });

  test("Dot symbol with numbers and hyphens: .symbol-123", () => {
    const input = ".symbol-123";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token.string).toEqual("symbol-123");
  });

  test("Dot symbol with underscores: .my_symbol_123", () => {
    const input = ".my_symbol_123";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token.string).toEqual("my_symbol_123");
  });

  test("Dot symbol terminated by whitespace", () => {
    const input = ".symbol NEXT";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token1.string).toEqual("symbol");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.WORD);
    expect(token2.string).toEqual("NEXT");
  });

  test("Dot symbol terminated by array bracket", () => {
    const input = ".symbol]";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token1.string).toEqual("symbol");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.END_ARRAY);
    expect(token2.string).toEqual("]");
  });

  test("Dot symbol terminated by semicolon", () => {
    const input = ".symbol;";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token1.string).toEqual("symbol");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.END_DEF);
    expect(token2.string).toEqual(";");
  });

  test("Dot symbol in array: [.symbol1 .symbol2]", () => {
    const input = "[.symbol1 .symbol2]";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const tokens = [];
    let token = tokenizer.next_token();
    while (token.type !== TokenType.EOS) {
      tokens.push(token);
      token = tokenizer.next_token();
    }
    
    expect(tokens.length).toEqual(4);
    expect(tokens[0].type).toEqual(TokenType.START_ARRAY);
    expect(tokens[1].type).toEqual(TokenType.DOT_SYMBOL);
    expect(tokens[1].string).toEqual("symbol1");
    expect(tokens[2].type).toEqual(TokenType.DOT_SYMBOL);
    expect(tokens[2].string).toEqual("symbol2");
    expect(tokens[3].type).toEqual(TokenType.END_ARRAY);
  });

  test("Dot symbol with complex characters: .test@domain.com", () => {
    const input = ".test@domain.com";
    const tokenizer = new Tokenizer(input, reference_location);
    const token = tokenizer.next_token();
    
    expect(token.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token.string).toEqual("test@domain.com");
  });

  test("Just a dot by itself should be treated as a word", () => {
    const input = ". NEXT";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.WORD);
    expect(token1.string).toEqual(".");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.WORD);
    expect(token2.string).toEqual("NEXT");
  });

  test("Short dot symbols (.s, .S) should be treated as words", () => {
    const input = ".s .S .x";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.WORD);
    expect(token1.string).toEqual(".s");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.WORD);
    expect(token2.string).toEqual(".S");
    
    const token3 = tokenizer.next_token();
    expect(token3.type).toEqual(TokenType.WORD);
    expect(token3.string).toEqual(".x");
  });

  test("Minimum length dot symbol (.ab) should be treated as DOT_SYMBOL", () => {
    const input = ".ab NEXT";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token1.string).toEqual("ab");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.WORD);
    expect(token2.string).toEqual("NEXT");
  });

  test("Multiple dot symbols in sequence", () => {
    const input = ".first .second .third";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const token1 = tokenizer.next_token();
    expect(token1.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token1.string).toEqual("first");
    
    const token2 = tokenizer.next_token();
    expect(token2.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token2.string).toEqual("second");
    
    const token3 = tokenizer.next_token();
    expect(token3.type).toEqual(TokenType.DOT_SYMBOL);
    expect(token3.string).toEqual("third");
  });

  test("Dot symbol mixed with other tokens", () => {
    const input = ": TEST-DEF   .symbol 42 + ;";
    const tokenizer = new Tokenizer(input, reference_location);
    
    const tokens = [];
    let token = tokenizer.next_token();
    while (token.type !== TokenType.EOS) {
      tokens.push(token);
      token = tokenizer.next_token();
    }
    
    expect(tokens.length).toEqual(5);
    expect(tokens[0].type).toEqual(TokenType.START_DEF);
    expect(tokens[0].string).toEqual("TEST-DEF");
    expect(tokens[1].type).toEqual(TokenType.DOT_SYMBOL);
    expect(tokens[1].string).toEqual("symbol");
    expect(tokens[2].type).toEqual(TokenType.WORD);
    expect(tokens[2].string).toEqual("42");
    expect(tokens[3].type).toEqual(TokenType.WORD);
    expect(tokens[3].string).toEqual("+");
    expect(tokens[4].type).toEqual(TokenType.END_DEF);
    expect(tokens[4].string).toEqual(";");
  });
});