import unittest
from forthic.tokenizer import Tokenizer, DLE
from forthic.tokens import StringToken, StartArrayToken, EndArrayToken, StartModuleToken,\
    EndModuleToken, StartDefinitionToken, EndDefinitionToken, WordToken, EOSToken

def get_tokens(tokenizer):
    result = []
    token = None
    while not isinstance(token, EOSToken):
        token = tokenizer.next_token()
        result.append(token)
    return result


def is_word_token(token, name):
    if not isinstance(token, WordToken):
        return False
    if token.name != name:
        return False
    return True


def is_string_token(token, string):
    if not isinstance(token, StringToken):
        return False
    if token.string != string:
        return False
    return True


def is_start_module_token(token, name):
    if not isinstance(token, StartModuleToken):
        return False
    if token.name != name:
        return False
    return True


class TestTokenizer(unittest.TestCase):
    def test_basic(self):
        """Checks to see that all basic tokens are recognized
        """
        tokenizer = Tokenizer("[ ] : DEFINITION ; { } '' WORD")
        tokens = get_tokens(tokenizer)
        expected = [StartArrayToken, EndArrayToken, StartDefinitionToken, EndDefinitionToken,
                    StartModuleToken, EndModuleToken, StringToken, WordToken, EOSToken]

        self.assertEqual(len(expected), len(tokens))

        for i in range(len(tokens)):
            self.assertIsInstance(tokens[i], expected[i])

    def test_end_definition(self):
        """Checks that end definition (;) is recognized even at end of word
        """
        tokenizer = Tokenizer("WORD; WORD2")
        tokens = get_tokens(tokenizer)

        self.assertTrue(is_word_token(tokens[0], "WORD"))
        self.assertIsInstance(tokens[1], EndDefinitionToken)
        self.assertTrue(is_word_token(tokens[2], "WORD2"))


    def test_start_module(self):
        tokenizer = Tokenizer("{ {my-mod")
        tokens = get_tokens(tokenizer)

        self.assertTrue(is_start_module_token(tokens[0], ""))
        self.assertTrue(is_start_module_token(tokens[1], "my-mod"))

    def test_strings(self):

        tokenizer = Tokenizer(f"'Single' ^Caret^ '''Triple Single''' ^^^Triple Caret^^^ {DLE}Single DLE{DLE}")
        tokens = get_tokens(tokenizer)

        self.assertTrue(is_string_token(tokens[0], "Single"))
        self.assertTrue(is_string_token(tokens[1], "Caret"))
        self.assertTrue(is_string_token(tokens[2], "Triple Single"))
        self.assertTrue(is_string_token(tokens[3], "Triple Caret"))
        self.assertTrue(is_string_token(tokens[4], "Single DLE"))

        tokenizer = Tokenizer('"Double" """Triple Double"""')
        tokens = get_tokens(tokenizer)

        self.assertTrue(is_string_token(tokens[0], "Double"))
        self.assertTrue(is_string_token(tokens[1], "Triple Double"))

    def test_arrays(self):
        tokenizer = Tokenizer("[1 2] [3[4]]")

        tokens = get_tokens(tokenizer)

        self.assertTrue(isinstance(tokens[0], StartArrayToken))
        self.assertTrue(is_word_token(tokens[1], "1"))
        self.assertTrue(is_word_token(tokens[2], "2"))
        self.assertTrue(isinstance(tokens[3], EndArrayToken))
        self.assertTrue(isinstance(tokens[4], StartArrayToken))
        self.assertTrue(is_word_token(tokens[5], "3"))
        self.assertTrue(isinstance(tokens[6], StartArrayToken))
        self.assertTrue(is_word_token(tokens[7], "4"))
        self.assertTrue(isinstance(tokens[8], EndArrayToken))
        self.assertTrue(isinstance(tokens[9], EndArrayToken))

    def test_end_module(self):
        tokenizer = Tokenizer("WORD1}WORD2")
        tokens = get_tokens(tokenizer)

        self.assertTrue(is_word_token(tokens[0], "WORD1"))
        self.assertTrue(isinstance(tokens[1], EndModuleToken))
        self.assertTrue(is_word_token(tokens[2], "WORD2"))


if __name__ == '__main__':
    unittest.main()
