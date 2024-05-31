#pragma once

#include <string>
using namespace std;


enum class TokenType { 
    COMMENT, 
    START_DEFINITION, 
    END_DEFINITION, 
    START_ARRAY,
    END_ARRAY,
    START_MODULE,
    END_MODULE,
    STRING,
    WORD,
    EOS };

class Token
{
public:
    Token(TokenType t, string& s);
    Token(TokenType t);
    ~Token();

    TokenType GetType();
    string GetText();

protected:
    TokenType type;
    string text;
};

