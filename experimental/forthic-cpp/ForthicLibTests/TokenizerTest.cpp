#include "pch.h"
#include "CppUnitTest.h"
#include "../ForthicLib/Tokenizer.h"
#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;
using namespace std;

namespace ForthicLibTests
{
    TEST_CLASS(TokenizerTest)
    {
    public:
        TEST_METHOD(TestConstruction)
        {
			string input = "";
			Tokenizer tokenizer(input);
			Assert::IsNotNull(&tokenizer);
        }

		TEST_METHOD(TestWhitespace)
		{
			string input = "      ()   \t\r\n   ";
			Tokenizer tokenizer(input);
			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::EOS == tok.GetType());
		}

		TEST_METHOD(TestComment)
		{
			string input = "  # This is a comment";
			Tokenizer tokenizer(input);
			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::COMMENT == tok.GetType());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::EOS == tok.GetType());
		}

		TEST_METHOD(TestStartEndDefinition)
		{
			string input = ": DEF1 ;";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::START_DEFINITION == tok.GetType());
			Assert::AreEqual(string("DEF1"), tok.GetText());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::END_DEFINITION == tok.GetType());
		}

		TEST_METHOD(TestStartEndArray)
		{
			string input = "[ ]";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::START_ARRAY == tok.GetType());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::END_ARRAY == tok.GetType());
		}

		TEST_METHOD(TestStartEndNamedModule)
		{
			string input = "{html }";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::START_MODULE == tok.GetType());
			Assert::AreEqual(string("html"), tok.GetText());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::END_MODULE == tok.GetType());
		}

		TEST_METHOD(TestAnonymousModule)
		{
			string input = "{ }";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::START_MODULE == tok.GetType());
			Assert::AreEqual(string(""), tok.GetText());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::END_MODULE == tok.GetType());
		}

		TEST_METHOD(TestIsTripleQuote)
		{
			Tokenizer t1(string("'''Now'''"));
			Assert::IsTrue(t1.IsTripleQuote(0, '\''));
			Assert::IsTrue(t1.IsTripleQuote(6, '\''));

			Tokenizer t2(string("\"\"\"Now\"\"\""));
			Assert::IsTrue(t2.IsTripleQuote(0, '"'));
			Assert::IsTrue(t2.IsTripleQuote(6, '"'));
		}

		TEST_METHOD(TestTripleQuoteString)
		{
			string input = "'''This is a ""triple - quoted"" string!'''";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::STRING == tok.GetType());
			Assert::AreEqual(string("This is a ""triple - quoted"" string!"), tok.GetText());
		}

		TEST_METHOD(TestString)
		{
			string input = "'Single quote' \"Double quote\"";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::STRING == tok.GetType());
			Assert::AreEqual(string("Single quote"), tok.GetText());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::STRING == tok.GetType());
			Assert::AreEqual(string("Double quote"), tok.GetText());
		}

		TEST_METHOD(TestWord)
		{
			string input = "WORD1 WORD2";
			Tokenizer tokenizer(input);

			Token tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::WORD == tok.GetType());
			Assert::AreEqual(string("WORD1"), tok.GetText());

			tok = tokenizer.NextToken();
			Assert::IsTrue(TokenType::WORD == tok.GetType());
			Assert::AreEqual(string("WORD2"), tok.GetText());
		}
	};
}