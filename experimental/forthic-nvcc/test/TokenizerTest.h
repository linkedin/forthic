#pragma once
#include <string>

class TokenizerTest {
public:
    TokenizerTest();
    void run();

protected:
    void printFailure(bool pass, const char* testName);

private:
    void testWhitespace();
    void testComment();
    void testStartEndDefinition();
    void testStartEndArray();
    void testStartEndNamedModule();
    void testAnonymousModule();
    void testTripleQuote();
    void testTripleQuoteString();
    void testString();
    void testWord();

private:
   std::string name; 
};
