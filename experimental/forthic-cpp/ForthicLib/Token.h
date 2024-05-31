#pragma once

#include <string>
using namespace std;

#define FORTHICLIB_API  __declspec(dllexport)

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

class FORTHICLIB_API Token
{
public:
	Token(enum class TokenType t, string& s);
	Token(enum class TokenType t);
	~Token();

	enum class TokenType GetType();
	string GetText();

protected:
	enum class TokenType type;
	string text;
};

