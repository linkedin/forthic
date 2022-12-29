import unittest
from forthic.v2.tokenizer import Tokenizer, UnterminatedStringError, InvalidDefinitionError


class TestTokenizerErrors(unittest.TestCase):
    def test_unterminated_string(self):
        """Raise exception if strings are unterminated
        """
        tokenizer = Tokenizer("'Unterminated")
        self.assertRaises(UnterminatedStringError, tokenizer.next_token)

    def test_start_definition_eos(self):
        """Can't have an empty definition
        """
        tokenizer = Tokenizer(":")
        self.assertRaises(InvalidDefinitionError, tokenizer.next_token)

    def test_start_definition_special_chars(self):
        """Can't have definition with Forthic special chars
        """
        def check_for_exception(input):
            tokenizer = Tokenizer(input)
            self.assertRaises(InvalidDefinitionError, tokenizer.next_token)

        invalid_start_defs = [": 'HOWDY", ": HOW'DY", ": HOW[DY", ": HOW]DY", ": HOW{DY", ": HOW}DY"]

        for forthic in invalid_start_defs:
            check_for_exception(forthic)


if __name__ == '__main__':
    unittest.main()
