#include "pch.h"

#include "Word.h"


Word::Word(string word_name) : name(word_name)
{
}


Word::~Word()
{
}

string Word::GetName()
{
	return name;
}