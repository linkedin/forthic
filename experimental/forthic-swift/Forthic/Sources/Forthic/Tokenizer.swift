import Darwin

let DLE: Character = Character(UnicodeScalar(16))

enum TokenizerError: Error {
    case EOS_IN_START_DEFINITION
    case QUOTE_IN_DEFINITION_NAME(string: String)
    case INVALID_CHAR_IN_DEFINITION_NAME(char: Character, string: String)
    case UNTERMINATED_TRIPLE_QUOTED_STRING(delimiter: Character, string: String)
    case UNTERMINATED_STRING(delimiter: Character, string: String)
}


class Tokenizer {
    var input_string: String
    var token_string: String = ""
    var position: Int = 0
    var whitespace: [Character] = [" ", "\t", "\n", "\r", "(", ")"]
    var quote_chars: [Character] = ["\"", "'", "^", DLE]

    init(string: String) {
        self.input_string = string
    }
    
    func next_token() throws -> Token {
        self.token_string = ""
        return try self.transition_from_START()
    }
    
    // ----- Internal functions ---------------------------------------------------------------------------------------
    private func is_whitespace(char: Character) -> Bool {
        return self.whitespace.contains(char)
    }
    
    private func is_quote(char: Character) -> Bool {
        return self.quote_chars.contains(char)
    }
    
    private func input_string_char(index: Int) -> Character {
        let index = self.input_string.index(self.input_string.startIndex, offsetBy: index)
        return self.input_string[index]
    }
    
    private func is_triple_quote(index: Int, char: Character) -> Bool {
        if (!self.is_quote(char: char)) {
            return false
        }
        
        if (index + 2 >= self.input_string.count) {
            return false
        }

        return self.input_string_char(index: index+1) == char && self.input_string_char(index: index+2) == char
    }

    private func transition_from_START() throws -> Token {
        while (self.position < self.input_string.count) {
            let char: Character = self.input_string_char(index: self.position)
            self.position += 1
            if (self.is_whitespace(char: char)) {
                continue
            }
            else if (char == "#") {
                return self.transition_from_COMMENT()
            }
            else if (char == ":") {
                return try self.transition_from_START_DEFINITION()
            }
            else if (char == ";") {
                return EndDefinitionToken()
            }
            else if (char == "[") {
                return StartArrayToken()
            }
            else if (char == "]") {
                return EndArrayToken()
            }
            else if (char == "{") {
                return self.transition_from_GATHER_MODULE()
            }
            else if (char == "}") {
                return EndModuleToken()
            }
            else if (self.is_triple_quote(index: self.position - 1, char: char)) {
                self.position += 2  // Skip over 2nd and 3rd quote chars
                return try self.transition_from_GATHER_TRIPLE_QUOTE_STRING(string_delimiter: char)
            }
            else if (self.is_quote(char: char)) {
                return try self.transition_from_GATHER_STRING(string_delimiter: char)
            }
            else {
                self.position -= 1  // Back up to beginning of word
                return self.transition_from_GATHER_WORD()
            }
        }
        return EOSToken()
    }
    
    private func transition_from_COMMENT() -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.token_string.append(char)
            self.position += 1
            if (char == "\n") {
                break
            }
        }
        return CommentToken(string: self.token_string)
    }
    
    private func transition_from_START_DEFINITION() throws -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.position += 1
            
            if (self.is_whitespace(char: char)) {
                continue
            }
            else {
                self.position -= 1
                return try self.transition_from_GATHER_DEFINITION_NAME()
            }
        }
        throw TokenizerError.EOS_IN_START_DEFINITION
    }

    private func transition_from_GATHER_DEFINITION_NAME() throws -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.position += 1
            
            if (self.is_whitespace(char: char)) {
                break
            }
            else if (self.is_quote(char: char)) {
                throw TokenizerError.QUOTE_IN_DEFINITION_NAME(string: self.token_string)
            }
            else if (["[", "]", "{", "}"].contains(char)) {
                throw TokenizerError.INVALID_CHAR_IN_DEFINITION_NAME(char: char, string: self.token_string)
            }
            else {
                self.token_string.append(char)
            }
        }
        return StartDefinitionToken(name: self.token_string)
    }
    
    private func transition_from_GATHER_MODULE() -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.position += 1
            if (self.is_whitespace(char: char)) {
                break
            }
            else if char == "}" {
                self.position -= 1
                break
            }
            else {
                self.token_string.append(char)
            }
        }
        return StartModuleToken(name: self.token_string)
    }
    
    private func transition_from_GATHER_TRIPLE_QUOTE_STRING(string_delimiter: Character) throws -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            if (char == string_delimiter && self.is_triple_quote(index: self.position, char: char)) {
                self.position += 3
                return StringToken(string: self.token_string)
            }
            else {
                self.position += 1
                self.token_string.append(char)
            }
        }
        throw TokenizerError.UNTERMINATED_TRIPLE_QUOTED_STRING(delimiter: string_delimiter, string: self.token_string)
    }
    
    private func transition_from_GATHER_STRING(string_delimiter: Character) throws -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.position += 1
            if (char == string_delimiter) {
                return StringToken(string: self.token_string)
            }
            else {
                self.token_string.append(char)
            }
        }
        throw TokenizerError.UNTERMINATED_STRING(delimiter: string_delimiter, string: self.token_string)
    }
    
    private func transition_from_GATHER_WORD() -> Token {
        while (self.position < self.input_string.count) {
            let char = self.input_string_char(index: self.position)
            self.position += 1
            if (self.is_whitespace(char: char)) {
                break
            }
            
            if ([";", "[", "]", "}"].contains(char)) {
                self.position -= 1
                break
            }
            else {
                self.token_string.append(char)
            }
        }
        return WordToken(name: self.token_string)
        
    }
}
