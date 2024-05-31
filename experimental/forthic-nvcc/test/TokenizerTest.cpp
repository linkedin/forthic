#include <string>
#include <iostream>

#include "TokenizerTest.h"

#include "../Tokenizer.h"

using namespace std;


TokenizerTest::TokenizerTest() {
    name = "TokenizerTest";
}


void TokenizerTest::run() {
    testWhitespace();
    testComment();
    testStartEndDefinition();
    testStartEndArray();
    testStartEndNamedModule();
    testAnonymousModule();
    testTripleQuote();
    testTripleQuoteString();
    testString();
    testWord();
}


void TokenizerTest::printFailure(bool failed, const char* testName) {
    if (failed)   printf("=> FAIL  %s::%s\n", name.c_str(), testName);
}


void TokenizerTest::testWhitespace() {
    string input = "     ()   \t\r\n   ";
    Tokenizer tokenizer(input);
    Token tok = tokenizer.NextToken();
    printFailure(TokenType::EOS != tok.GetType(), "testWhitespace");
}


void TokenizerTest::testComment() {
    string input = "  # This is a comment";
    Tokenizer tokenizer(input);
    Token tok = tokenizer.NextToken();
    printFailure(TokenType::COMMENT != tok.GetType(), "testComment 1");

    tok = tokenizer.NextToken();
    printFailure(TokenType::EOS != tok.GetType(), "testComment 2");
}


void TokenizerTest::testStartEndDefinition() {
    string input = ": DEF1 ;";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::START_DEFINITION != tok.GetType(), "testStartEndDefinition 1");
    printFailure(tok.GetText() != "DEF1", "testStartEndDefinition 2");

    tok = tokenizer.NextToken();
    printFailure(TokenType::END_DEFINITION != tok.GetType(), "testStartEndDefinition 3");
}


void TokenizerTest::testStartEndArray() {
    string input = "[ ]";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::START_ARRAY != tok.GetType(), "testStartEndArray 1");

    tok = tokenizer.NextToken();
    printFailure(TokenType::END_ARRAY != tok.GetType(), "testStartEndArray 2");
}


void TokenizerTest::testStartEndNamedModule() {
    string input = "{html }";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::START_MODULE != tok.GetType(), "testStartEndNamedModule 1");
    printFailure(tok.GetText() != "html", "testStartEndNamedModule 2");

    tok = tokenizer.NextToken();
    printFailure(TokenType::END_MODULE != tok.GetType(), "testStartEndNamedModule 3");
}


void TokenizerTest::testAnonymousModule() {
    string input = "{ }";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::START_MODULE != tok.GetType(), "testAnonymousModule 1");
    printFailure(tok.GetText() != "", "testAnonymousModule 2");

    tok = tokenizer.NextToken();
    printFailure(TokenType::END_MODULE != tok.GetType(), "testAnonymousModule 3");
}


void TokenizerTest::testTripleQuote() {
    string input1 = "'''Now'''";
    Tokenizer t1(input1);
    printFailure(t1.IsTripleQuote(0, '\'') == false, "testTripleQuote 1");
    printFailure(t1.IsTripleQuote(6, '\'') == false, "testTripleQuote 2");

    string input2 = "\"\"\"Now\"\"\"";
    Tokenizer t2(input2);
    printFailure(t2.IsTripleQuote(0, '"') == false, "testTripleQuote 3");
    printFailure(t2.IsTripleQuote(6, '"') == false, "testTripleQuote 4");
}


void TokenizerTest::testTripleQuoteString() {
    string input = "'''This is a ""triple - quoted"" string!'''";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::STRING != tok.GetType(), "testTripleQuoteString 1");
    printFailure(tok.GetText() != "This is a ""triple - quoted"" string!", "testTripleQuoteString 2");
}


void TokenizerTest::testString() {
    string input = "'Single quote' \"Double quote\"";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::STRING != tok.GetType(), "testString 1");
    printFailure(tok.GetText() != "Single quote", "testString 2");

    tok = tokenizer.NextToken();
    printFailure(TokenType::STRING != tok.GetType(), "testString 3");
    printFailure(tok.GetText() != "Double quote", "testString 4");
}


void TokenizerTest::testWord() {
    string input = "WORD1 WORD2";
    Tokenizer tokenizer(input);

    Token tok = tokenizer.NextToken();
    printFailure(TokenType::WORD != tok.GetType(), "testWord 1");
    printFailure(tok.GetText() != "WORD1", "testWord 2");

    tok = tokenizer.NextToken();
    printFailure(TokenType::WORD != tok.GetType(), "testWord 3");
    printFailure(tok.GetText() != "WORD2", "testWord 4");
}

