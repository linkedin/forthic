import XCTest
@testable import Forthic

func get_tokens(tokenizer: Tokenizer) throws -> [Token] {
    var result: [Token] = []
    var token: Token = Token();
    while (!(token is EOSToken)) {
        token = try tokenizer.next_token()
        result.append(token)
    }
    return result
}


final class TokenizerTests: XCTestCase {
    func testComment() throws {
        let tokenizer = Tokenizer(string: "# Howdy, Comment!")
        let token = try tokenizer.next_token()
        
        XCTAssert(token is CommentToken)
        let comment_token: CommentToken = token as! CommentToken
        XCTAssertEqual(comment_token.str, " Howdy, Comment!")
    }

    func testStartDefinition() throws {
        let tokenizer = Tokenizer(string: ": MY-DEF   ;")
        let token = try tokenizer.next_token()
        
        XCTAssert(token is StartDefinitionToken)
        let start_def_token: StartDefinitionToken = token as! StartDefinitionToken
        XCTAssertEqual(start_def_token.name, "MY-DEF")
    }

    func testEndDefinition() throws {
        let tokenizer = Tokenizer(string: "WORD; WORD2")
        let token1 = try tokenizer.next_token()
        let token2 = try tokenizer.next_token()
        let token3 = try tokenizer.next_token()

        XCTAssert(token1 is WordToken)
        XCTAssert(token2 is EndDefinitionToken)
        XCTAssert(token3 is WordToken)
    }
    
    func testStartModule() throws {
        let tokenizer = Tokenizer(string: "{ {my-mod")
        let token1 = try tokenizer.next_token()
        let token2 = try tokenizer.next_token()
        
        XCTAssert(token1 is StartModuleToken)
        XCTAssert(token2 is StartModuleToken)
        let token2_start_module: StartModuleToken = token2 as! StartModuleToken
        XCTAssertEqual(token2_start_module.name, "my-mod")
    }

    func testEndModule() throws {
        let tokenizer = Tokenizer(string: "WORD}WORD2")
        let token1 = try tokenizer.next_token()
        let token2 = try tokenizer.next_token()
        let token3 = try tokenizer.next_token()

        XCTAssert(token1 is WordToken)
        XCTAssert(token2 is EndModuleToken)
        XCTAssert(token3 is WordToken)
    }


    func testStrings() throws {
        let tokenizer = Tokenizer(string: "'Single' ^Caret^ '''Triple Single''' ^^^Triple Caret^^^ \(DLE)Single DLE\(DLE)")
        let tokens = try get_tokens(tokenizer: tokenizer).dropLast()  // Drop EOSToken
        XCTAssertEqual(tokens.count, 5)
        for t in tokens {
            XCTAssert(t is StringToken)
        }
        XCTAssertEqual((tokens[0] as! StringToken).str, "Single")
        XCTAssertEqual((tokens[1] as! StringToken).str, "Caret")
        XCTAssertEqual((tokens[2] as! StringToken).str, "Triple Single")
        XCTAssertEqual((tokens[3] as! StringToken).str, "Triple Caret")
        XCTAssertEqual((tokens[4] as! StringToken).str, "Single DLE")
    }
}

