#include "Token.h"


Token::Token(TokenType t, string& s)
{
    type = t;
    text = s;
}

Token::Token(TokenType t)
{
    type = t;
    text = "";
}

Token::~Token()
{
}


TokenType Token::GetType()
{
    return type;
}

string Token::GetText()
{
    return text;
}
