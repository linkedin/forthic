class Token {
}


class StringToken : Token {
    var str: String

    init(string: String) {
        self.str = string
    }
}


class CommentToken : Token {
    var str: String
    
    init(string: String) {
        self.str = string
    }
}


class StartArrayToken : Token {
}


class EndArrayToken : Token {
}


class StartModuleToken : Token {
    var name: String
    
    init(name: String) {
        self.name = name
    }
}


class EndModuleToken : Token {
}


class StartDefinitionToken : Token {
    var name: String
    
    init(name: String) {
        self.name = name
    }
}


class EndDefinitionToken : Token {
}

class WordToken : Token {
    var name: String
    
    init(name: String) {
        self.name = name
    }
}


class EOSToken: Token {
}
