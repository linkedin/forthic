#pragma once

#include <string>
#include "Token.h"
#include "Defines.h"

using namespace std;

class FORTHICLIB_API Tokenizer
{
public:
	Tokenizer(string& s);
	~Tokenizer();
	Token NextToken();
	bool IsTripleQuote(int index, char c);

protected:
	bool is_whitespace(char c);
	bool is_quote(char c);

	// Transition functions
	Token transition_from_START();
	Token transition_from_COMMENT();
	Token transition_from_START_DEFINITION();
	Token transition_from_GATHER_DEFINITION_NAME();
	Token transition_from_GATHER_MODULE();
	Token transition_from_GATHER_TRIPLE_QUOTE_STRING(char delim);
	Token transition_from_GATHER_STRING(char delim);
	Token transition_from_GATHER_WORD(char first_char);


	unsigned int position;
	string input;
	string whitespace;
	string token_string;
};

