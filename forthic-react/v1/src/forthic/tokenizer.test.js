import {Token, Tokenizer, TOK_END_ARRAY, TOK_END_DEF, TOK_END_MODULE, TOK_EOS,
    TOK_START_ARRAY, TOK_START_DEF, TOK_START_MEMO, TOK_START_MODULE, TOK_STRING,
    TOK_WORD, DLE} from "./tokenizer"

it('Recognizes basic tokens', () => {
    let tokenizer = new Tokenizer("[ ] : DEFINITION ; { } '' WORD  @: MEMO");
    let tokens = get_tokens(tokenizer)
    let token_types = tokens.map(t => t.type)

    expect(token_types).toEqual([TOK_START_ARRAY, TOK_END_ARRAY, TOK_START_DEF, TOK_END_DEF,
                                 TOK_START_MODULE, TOK_END_MODULE, TOK_STRING, TOK_WORD, TOK_START_MEMO, TOK_EOS])
});


it('Checks that end definition (;) is recognized even at end of word', () => {
    let tokenizer = new Tokenizer("WORD; WORD2")
    let tokens = get_tokens(tokenizer)

    expect(is_word_token(tokens[0], "WORD")).toBeTruthy()
    expect(tokens[1].type).toEqual(TOK_END_DEF)
    expect(is_word_token(tokens[2], "WORD2")).toBeTruthy()
})


it('Can find a start module', () => {
    let tokenizer = new Tokenizer("{ {my-mod")
    let tokens = get_tokens(tokenizer)

    expect(is_start_module_token(tokens[0], "")).toBeTruthy()
    expect(is_start_module_token(tokens[1], "my-mod")).toBeTruthy()
})


it('Can parse strings', () => {
    let tokenizer = new Tokenizer(`'Single' ^Caret^ '''Triple Single''' ^^^Triple Caret^^^ ${DLE}Single DLE${DLE}`)
    let tokens = get_tokens(tokenizer)

    expect(is_string_token(tokens[0], "Single")).toBeTruthy()
    expect(is_string_token(tokens[1], "Caret")).toBeTruthy()
    expect(is_string_token(tokens[2], "Triple Single")).toBeTruthy()
    expect(is_string_token(tokens[3], "Triple Caret")).toBeTruthy()
    expect(is_string_token(tokens[4], "Single DLE")).toBeTruthy()

    tokenizer = new Tokenizer('"Double" """Triple Double"""')
    tokens = get_tokens(tokenizer)

    expect(is_string_token(tokens[0], "Double")).toBeTruthy()
    expect(is_string_token(tokens[1], "Triple Double")).toBeTruthy()
})

it('Can parse arrays', () => {
    let tokenizer = new Tokenizer("[1 2] [3[4]]")
    let tokens = get_tokens(tokenizer)

    expect(tokens[0].type === TOK_START_ARRAY).toBeTruthy()
    expect(is_word_token(tokens[1], "1")).toBeTruthy()
    expect(is_word_token(tokens[2], "2")).toBeTruthy()
    expect(tokens[3].type === TOK_END_ARRAY).toBeTruthy()
    expect(tokens[4].type === TOK_START_ARRAY).toBeTruthy()
    expect(is_word_token(tokens[5], "3")).toBeTruthy()
    expect(tokens[6].type === TOK_START_ARRAY).toBeTruthy()
    expect(is_word_token(tokens[7], "4")).toBeTruthy()
    expect(tokens[8].type === TOK_END_ARRAY).toBeTruthy()
    expect(tokens[9].type === TOK_END_ARRAY).toBeTruthy()
})

it('Can recognize an end module', () => {
    let tokenizer = new Tokenizer("WORD1}WORD2")
    let tokens = get_tokens(tokenizer)

    expect(is_word_token(tokens[0], "WORD1")).toBeTruthy()
    expect(tokens[1].type === TOK_END_MODULE).toBeTruthy()
    expect(is_word_token(tokens[2], "WORD2")).toBeTruthy()
})


// ----- Utilities --------------------------------------------------------------------------------
function get_tokens(tokenizer) {
    let result = []
    let token = new Token(-1, "")
    while (token.type !== TOK_EOS) {
        token = tokenizer.next_token()
        result.push(token)
    }
    return result
}

function is_word_token(token, string) {
    if (token.type !== TOK_WORD)   return false
    if (token.string !== string)       return false
    return true
}

function is_string_token(token, string) {
    if (token.type !== TOK_STRING)   return false
    if (token.string !== string)     return false
    return true
}

function is_start_module_token(token, string) {
    if (token.type !== TOK_START_MODULE)   return false
    if (token.string !== string)               return false
    return true
}
