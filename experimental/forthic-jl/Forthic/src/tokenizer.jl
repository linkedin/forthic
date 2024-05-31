@enum TokenType begin
    TOK_STRING
    TOK_EVAL_STRING
    TOK_COMMENT
    TOK_START_ARRAY
    TOK_END_ARRAY
    TOK_START_MODULE
    TOK_END_MODULE
    TOK_START_DEF
    TOK_END_DEF
    TOK_WORD
    TOK_EOS
end

struct Token
    type::TokenType
    string::String
end

DLE = Char(16)
whitespace = Set([' ' '\t' '\n' '\r' '(' ')'])
quote_chars = Set(['"' '\'' DLE])
non_word_chars = Set([';' '[' ']' '}'])

mutable struct State
    input_string::String
    position::Integer
    token_string::String
end

new_tokenizer(string::String) = State(string, 1, "")

is_whitespace(char::Char) = char in whitespace
is_quote(char::Char) = char in quote_chars
is_nonword_char(char::Char) = char in non_word_chars

function is_triple_quote(tokenizer::State, index::Integer, char::Char)
    if !is_quote(char)                              return false end
    if index + 2 > length(tokenizer.input_string)   return false end
    return tokenizer.input_string[index + 1] == char && tokenizer.input_string[index + 2 ] == char
end

function next_token(tokenizer::State)
    tokenizer.token_string = ""
    return transition_from_START(tokenizer)
end

function transition_from_COMMENT(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.token_string *= char
        tokenizer.position += 1
        if char == '\n'   break end
    end
    return Token(TOK_COMMENT, tokenizer.token_string)
end


function transition_from_GATHER_WORD(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1
        if is_whitespace(char)   break end
        if is_nonword_char(char)
            tokenizer.position -= 1
            break
        else
            tokenizer.token_string *= char
        end
    end
    return Token(TOK_WORD, tokenizer.token_string)
end

function transition_from_START_DEFINITION(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1

        if is_whitespace(char)   continue
        elseif is_quote(char)    throw(error("Definitions shouldn't have quotes in them"))
        else
            tokenizer.position -= 1
            return transition_from_GATHER_DEFINITION_NAME(tokenizer)
        end
    end

    throw(error("Got EOS in START_DEFINITION"))
end

function transition_from_GATHER_DEFINITION_NAME(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1

        if is_whitespace(char)     break end
        if is_quote(char)          throw(error("Definitions can't have quotes in them")) end
        if is_nonword_char(char)   throw(error("Definitions can't have '$char' in them")) end

        tokenizer.token_string *= char
    end
    return Token(TOK_START_DEF, tokenizer.token_string)
end

function transition_from_GATHER_MODULE(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1

        if is_whitespace(char)
            break
        elseif char == '}'
            tokenizer.position -= 1
            break
        else
            tokenizer.token_string *= char
        end
    end
    return Token(TOK_START_MODULE, tokenizer.token_string)
end

function transition_from_GATHER_TRIPLE_QUOTE_STRING(tokenizer::State, delim::Char)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        if char == delim && is_triple_quote(tokenizer, tokenizer.position, char)
            tokenizer.position += 3
            return Token(TOK_STRING, tokenizer.token_string)
        else
            tokenizer.position += 1
            tokenizer.token_string *= char
        end
    end
    throw(error("Unterminated triple quote string"))
end


function transition_from_GATHER_STRING(tokenizer::State, delim::Char)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1

        if char == delim
            return Token(TOK_STRING, tokenizer.token_string)
        else
            tokenizer.token_string *= char
        end
    end
    throw(error("Unterminated string"))
end

function transition_from_GATHER_EVAL_STRING(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1

        if char == '\`'
            return Token(TOK_EVAL_STRING, tokenizer.token_string)
        else
            tokenizer.token_string *= char
        end
    end
    throw(error("Unterminated eval string"))
end

function transition_from_START(tokenizer::State)
    while tokenizer.position <= length(tokenizer.input_string)
        char = tokenizer.input_string[tokenizer.position]
        tokenizer.position += 1
        if is_whitespace(char)   continue
        elseif char == '#'       return transition_from_COMMENT(tokenizer)
        elseif char == ':'       return transition_from_START_DEFINITION(tokenizer)
        elseif char == ';'       return Token(TOK_END_DEF, string(char))
        elseif char == '['       return Token(TOK_START_ARRAY, string(char))
        elseif char == ']'       return Token(TOK_END_ARRAY, string(char))
        elseif char == '{'       return transition_from_GATHER_MODULE(tokenizer)
        elseif char == '}'       return Token(TOK_END_MODULE, string(char))
        elseif is_triple_quote(tokenizer, tokenizer.position-1, char)
            tokenizer.position += 2
            return transition_from_GATHER_TRIPLE_QUOTE_STRING(tokenizer, char)
        elseif is_quote(char)    return transition_from_GATHER_STRING(tokenizer, char)
        elseif char == '\`'       return transition_from_GATHER_EVAL_STRING(tokenizer)
        else
            tokenizer.position -= 1
            return transition_from_GATHER_WORD(tokenizer)
        end
    end

    return Token(TOK_EOS, "")
end
