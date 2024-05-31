#include "pch.h"
#include "Tokenizer.h"

Tokenizer::Tokenizer(string& s) : input(s) 
{
	whitespace = " \r\t\n()";
	token_string = "";
	position = 0;
}

Tokenizer::~Tokenizer()
{
}

bool Tokenizer::is_whitespace(char c)
{
	for (unsigned int i = 0; i < whitespace.length(); i++) {
		if (c == whitespace[i]) return true;
	}
	return false;
}

bool Tokenizer::is_quote(char c)
{
	return (c == '"' || c == '\'');
}

bool Tokenizer::IsTripleQuote(int index, char c)
{
	if (!is_quote(c)) return false;
	if (index + 2 >= input.length()) return false;
	return (input[index + 1] == c && input[index + 2] == c);
}

Token Tokenizer::NextToken()
{
	token_string = "";
	return transition_from_START();
}

Token Tokenizer::transition_from_START()
{
	while (position < input.length())
	{
		char c = input[position++];
		if (is_whitespace(c)) continue;
		else if (c == '#') return transition_from_COMMENT();
		else if (c == ':') return transition_from_START_DEFINITION();
		else if (c == ';') return Token(TokenType::END_DEFINITION);
		else if (c == '[') return Token(TokenType::START_ARRAY);
		else if (c == ']') return Token(TokenType::END_ARRAY);
		else if (c == '{') return transition_from_GATHER_MODULE();
		else if (c == '}') return Token(TokenType::END_MODULE);
		else if (IsTripleQuote(position - 1, c))
		{
			position += 2;  // Skip 2nd and 3rd quote chars
			return transition_from_GATHER_TRIPLE_QUOTE_STRING(c);
		}
		else if (is_quote(c))
		{
			return transition_from_GATHER_STRING(c);
		}
		else return transition_from_GATHER_WORD(c);
	}
	return Token(TokenType::EOS);
}

Token Tokenizer::transition_from_COMMENT()
{
	while (position < input.length())
	{
		char c = input[position++];
		if (c == '\n') break;
		token_string += c;
	}
	return Token(TokenType::COMMENT, token_string);
}


Token Tokenizer::transition_from_START_DEFINITION()
{

	while (position < input.length())
	{

		char c = input[position++];
		if (is_whitespace(c)) continue;
		else if (c == '"' || c == '\'') throw "Definition cannot start with a quote";
		else
		{
			position--;
			return transition_from_GATHER_DEFINITION_NAME();
		}
	}

	throw "Got EOS in START_DEFINITION";
}

Token Tokenizer::transition_from_GATHER_DEFINITION_NAME()
{
	while (position < input.length())
	{
		char c = input[position++];
		if (is_whitespace(c)) break;
		else token_string += c;
	}

	return Token(TokenType::START_DEFINITION, token_string);
}

Token Tokenizer::transition_from_GATHER_MODULE()
{
	while (position < input.length())
	{
		char c = input[position++];
		if (is_whitespace(c)) break;
		else if (c == '}')
		{
			position--;
			break;
		}
		else token_string += c;
	}
	return Token(TokenType::START_MODULE, token_string);
}


Token Tokenizer::transition_from_GATHER_TRIPLE_QUOTE_STRING(char delim)
{
	while (position < input.length())
	{
		char c = input[position];
		if (c == delim && IsTripleQuote(position, c))
		{
			position += 3;
			return Token(TokenType::STRING, token_string);
		}
		else
		{
			token_string += c;
			position++;
		}
	}
	throw "Unterminated triple quote string";
}


Token Tokenizer::transition_from_GATHER_STRING(char delim)
{
	while (position < input.length())
	{
		char c = input[position++];
		if (c == delim)  return Token(TokenType::STRING, token_string);
		else             token_string += c;
	}
	throw "Unterminated string";
}


Token Tokenizer::transition_from_GATHER_WORD(char first_char)
{
	token_string += first_char;
	while (position < input.length())
	{
		char c = input[position++];
		if (is_whitespace(c)) break;
		else token_string += c;
	}
	return Token(TokenType::WORD, token_string);
}