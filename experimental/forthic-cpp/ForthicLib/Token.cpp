#include "pch.h"
#include "Token.h"


Token::Token(enum class TokenType t, string& s)
{
	type = t;
	text = s;
}

Token::Token(enum class TokenType t)
{
	type = t;
	text = "";
}

Token::~Token()
{
}


enum class TokenType Token::GetType()
{
	return type;
}

string Token::GetText()
{
	return text;
}
