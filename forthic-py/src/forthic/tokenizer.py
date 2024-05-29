from .tokens import StartArrayToken, EndArrayToken, StartDefinitionToken, EndDefinitionToken, \
    StartMemoToken, CommentToken, StartModuleToken, EndModuleToken, StringToken, WordToken, EOSToken, Token
from typing import List


# 'Data Link Escape'
DLE = chr(16)


class TokenizerError(RuntimeError):
    pass


class InvalidDefinitionError(TokenizerError):
    def __init__(self, msg: str):
        super().__init__(msg)


class UnterminatedStringError(TokenizerError):
    def __init__(self, msg: str):
        super().__init__(msg)


class Tokenizer:
    """A Tokenizer is constructed with an input string and returns the next available
       token on request.
    """
    def __init__(self, string: str):
        self.input_string: str = string
        self.position: int = 0
        self.whitespace: List[str] = [' ', '\t', '\n', '\r', '(', ')']
        self.quote_chars: List[str] = ['"', "'", '^', DLE]
        self.token_string: str = ''   # Token string currently gathered from the input string

    def next_token(self):
        self.clear_token_string()
        return self.transition_from_START()

    # =======
    # Internal functions

    def clear_token_string(self):
        self.token_string = ''

    def is_whitespace(self, char: str) -> bool:
        return char in self.whitespace

    def is_quote(self, char: str) -> bool:
        return char in self.quote_chars

    def is_triple_quote(self, index: int, char: str) -> bool:
        if not self.is_quote(char):
            return False
        if index + 2 >= len(self.input_string):
            return False
        return self.input_string[index + 1] == char and self.input_string[index + 2] == char

    def is_start_memo(self, index: int) -> bool:
        if index + 1 >= len(self.input_string):
            return False
        result = self.input_string[index] == "@" and self.input_string[index + 1] == ":"
        return result

    def transition_from_START(self) -> Token:
        """Tokenization is implemented as a state machine. This is the entry point.
        """
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1
            if self.is_whitespace(char):
                pass
            elif char == '#':
                return self.transition_from_COMMENT()
            elif char == ':':
                return self.transition_from_START_DEFINITION()
            elif self.is_start_memo(self.position - 1):
                self.position += 1   # Skip over ":" in "@:"
                return self.transition_from_START_MEMO()
            elif char == ';':
                return EndDefinitionToken()
            elif char == '[':
                return StartArrayToken()
            elif char == ']':
                return EndArrayToken()
            elif char == '{':
                return self.transition_from_GATHER_MODULE()
            elif char == '}':
                return EndModuleToken()
            elif self.is_triple_quote(self.position - 1, char):
                self.position += 2  # Skip over 2nd and 3rd quote chars
                return self.transition_from_GATHER_TRIPLE_QUOTE_STRING(char)
            elif self.is_quote(char):
                return self.transition_from_GATHER_STRING(char)
            else:
                self.position -= 1  # Back up to beginning of word
                return self.transition_from_GATHER_WORD()
        return EOSToken()

    def transition_from_COMMENT(self) -> CommentToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.token_string += char
            self.position += 1
            if char == '\n':
                break
        return CommentToken(self.token_string)

    def transition_from_START_DEFINITION(self) -> StartDefinitionToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1

            if self.is_whitespace(char):
                continue
            else:
                self.position -= 1
                return self.transition_from_GATHER_DEFINITION_NAME()

        raise InvalidDefinitionError("Got EOS in START_DEFINITION")

    def transition_from_START_MEMO(self) -> StartMemoToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1

            if self.is_whitespace(char):
                continue
            else:
                self.position -= 1
                return self.transition_from_GATHER_MEMO_NAME()

        raise InvalidDefinitionError("Got EOS in START_MEMO")

    def gather_definition_name(self) -> None:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1
            if self.is_whitespace(char):
                break
            elif self.is_quote(char):
                raise InvalidDefinitionError("Definitions can't have quotes in them")
            elif char in ['[', ']', '{', '}']:
                raise InvalidDefinitionError(f"Definitions can't have '{char}' in them")
            else:
                self.token_string += char
        return

    def transition_from_GATHER_DEFINITION_NAME(self) -> StartDefinitionToken:
        self.gather_definition_name()
        return StartDefinitionToken(self.token_string)

    def transition_from_GATHER_MEMO_NAME(self) -> StartMemoToken:
        self.gather_definition_name()
        return StartMemoToken(self.token_string)

    def transition_from_GATHER_MODULE(self) -> StartModuleToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1
            if self.is_whitespace(char):
                break
            elif char == '}':
                self.position -= 1
                break
            else:
                self.token_string += char
        return StartModuleToken(self.token_string)

    def transition_from_GATHER_TRIPLE_QUOTE_STRING(self, string_delimiter: str) -> StringToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            if char == string_delimiter and self.is_triple_quote(self.position, char):
                self.position += 3
                return StringToken(self.token_string)
            else:
                self.position += 1
                self.token_string += char
        raise UnterminatedStringError(f"Unterminated triple quoted string ({string_delimiter * 3})")

    def transition_from_GATHER_STRING(self, string_delimiter: str) -> StringToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1
            if char == string_delimiter:
                return StringToken(self.token_string)
            else:
                self.token_string += char
        raise UnterminatedStringError(f"Unterminated string ({string_delimiter}), {self.token_string}")

    def transition_from_GATHER_WORD(self) -> WordToken:
        while self.position < len(self.input_string):
            char = self.input_string[self.position]
            self.position += 1
            if self.is_whitespace(char):
                break
            if char in [';', '[', ']', '}']:
                self.position -= 1
                break
            else:
                self.token_string += char
        return WordToken(self.token_string)
