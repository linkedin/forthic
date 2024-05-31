import Foundation
enum InterpreterError: Error {
    case UNKNOWN_MODULE_ERROR(module_name: String)
    case UNKNOWN_TOKEN_ERROR(token: Token)
    case NESTED_DEFINITION_ERROR
    case UNMATCHED_END_DEFINITION_ERROR
    case UNKNOWN_WORD_ERROR(word_name: String)
    case STACK_UNDERFLOW
    case MODULE_STACK_UNDERFLOW
    case COMPILING_WITHOUT_CUR_DEFINITION
    case UNKNOWN_MODULE(module_name: String)
    case UNKNOWN_SCREEN(name: String)
}


/// Represents the end of an array
class EndArrayWord : Word {
    init() {
        super.init(name: "]")
    }
    
    override func execute(interp: Interpreter) throws {
        var items: [Any?] = []
        var item: Any? = try interp.stack_pop()
        while (!(item is StartArrayToken)) {
            items.append(item)
            item = try interp.stack_pop()
        }
        items.reverse()
        interp.stack_push(items)
    }
}


/// This represents a word that is defined in terms of other words
///
/// A definition looks like this:
///
/// : WORD-NAME   WORD1 WORD2 WORD3;
///
/// The name of the defined word is `WORD-NAME`. When it is executed, `WORD1`, `WORD2`, and `WORD3` are
/// executed in that order.
class DefinitionWord : Word {
    var words: [Word] = []

    func add_word(word: Word) {
        self.words.append(word)
    }
    
    override func execute(interp: Interpreter) throws {
        for w in self.words {
            try w.execute(interp: interp)
        }
    }
}

/// Pushes module onto interpreter's module stack
class StartModuleWord : Word {
    override init(name: String) {
        super.init(name: name)
    }
    
    override func execute(interp: Interpreter) throws {
        // The app module is the only module with a blank name
        if (self.name == "") {
            interp.module_stack_push(module: interp.app_module())
            return
        }
        
        // If the module is used by the current module, push it onto the module stack
        var module = interp.cur_module().find_module(name: self.name)
        
        // ..if not in the current module, check the app_module
        if (module == nil) {
            module = interp.app_module().find_module(name: self.name)
        }
        
        // ..otherwise, create a new module and push that onto the module stack
        if (module == nil) {
            let new_module = Module(name: self.name, interp: interp, forthic: "")
            interp.cur_module().register_module(module_name: new_module.name, module: new_module)
            module = new_module
        }
        
        interp.module_stack_push(module: module!)
    }
}

class EndModuleWord : Word {
    init() {
        super.init(name: "}")
    }
    
    override func execute(interp: Interpreter) throws {
        try interp.module_stack_pop()
    }
}


/// The application module is a special module that contains the words and variables for a Forthic application. There is only one
/// application module in an app. It's the first module on the module stack. It's the only module where `USE-MODULE` can be called.
/// It's the only module with no name.
///
/// The Forthic code for an application module may be split across several "screens" of code.
class AppModule: Module {
    var screens : [String: String] = [:]
    
    init(interp: Interpreter) {
        super.init(name: "", interp: interp, forthic: "")
    }
    
    func set_screen(name: String, forthic: String) {
        self.screens[name] = forthic
    }
    
    func get_screen(name: String) throws -> String {
        let result: String? = self.screens[name]
        if (result == nil) {
            throw InterpreterError.UNKNOWN_SCREEN(name: name)
        }
        return result!
    }
}

class Interpreter {
    var _app_module: AppModule? = nil
    var stack : [Any?] = []
    var _global_module : GlobalModule? = nil
    var module_stack: [Module] = []
    var registered_modules: [String: Module] = [:]
    var is_compiling: Bool = false
    var cur_definition: DefinitionWord? = nil
    var dev_mode: Bool = false
    var timezone: TimeZone
    
    init(timezone: TimeZone? = nil) {
        if (timezone == nil) {
            self.timezone = TimeZone.init(identifier: "America/Los_Angeles")!
        }
        else {
            self.timezone = timezone!
        }
        
        self._global_module = GlobalModule(interp: self, timezone: self.timezone)
        self._app_module = AppModule(interp: self)
        self.module_stack.append(self._app_module!)
    }
    
    func app_module() -> AppModule {
        return self._app_module!
    }

    func global_module() -> GlobalModule {
        return self._global_module!
    }

    /// Interprets a Forthic string, executing words one at a time until the end of the string
    func run(forthic: String) throws {
        let tokenizer = Tokenizer(string: forthic)
        var token = try tokenizer.next_token()
        while (!(token is EOSToken)) {
            try self.handle_token(token)
            token = try tokenizer.next_token()
        }
    }

    func run_in_module(module: Module, forthic: String) throws {
        self.module_stack_push(module: module)
        try self.run(forthic: forthic)
        try self.module_stack_pop()
    }

    func stack_push(_ value: Any?) {
        self.stack.append(value)
    }
    
    func stack_pop() throws -> Any? {
        if (self.stack.count == 0) {
            throw InterpreterError.STACK_UNDERFLOW
        }
        let result = self.stack.popLast()!
        return result
    }
    
    func cur_module() -> Module {
        let result = self.module_stack.last!
        return result
    }
    
    func find_module(name: String) throws -> Module {
        if (!self.registered_modules.keys.contains(name)) {
            throw InterpreterError.UNKNOWN_MODULE(module_name: name)
        }
        let result: Module = self.registered_modules[name]!
        return result
    }

    func module_stack_push(module: Module) {
        self.module_stack.append(module)
    }

    func module_stack_pop() throws {
        let module = self.module_stack.popLast()
        if (module == nil) {
            throw InterpreterError.MODULE_STACK_UNDERFLOW
        }
    }
    
    /// Registers a Module with the Interpreter so that it can be used from an application
    func register_module(module: Module) {
        self.registered_modules[module.name] = module
    }
    
    
    // ----- Private functions ----------------------------------------------------------------------------------------
    
    /// Searches the interpreter for a word
    ///
    /// The module stack is searched top down. If the words cannot be found, the global module is searched.
    /// Note that the bottom of the module stack is always the application module
    private func find_word(name: String) -> Word? {
        let modules = self.module_stack.reversed()
        var result: Word? = nil
        
        for m in modules {
            result = m.find_word(name: name)
            if (result != nil) {
                break
            }
        }
        
        if (result == nil) {
            result = self.global_module().find_word(name: name)
        }
        return result
    }
    
    /// Called to handle each token from the Tokenizer
    private func handle_token(_ token: Token) throws {
        if (token is StringToken) {
            try self.handle_string_token(token as! StringToken)
        }
        else if (token is CommentToken) {
            self.handle_comment_token(token as! CommentToken)
        }
        else if (token is StartArrayToken) {
            try self.handle_start_array_token(token as! StartArrayToken)
        }
        else if (token is EndArrayToken) {
            try self.handle_end_array_token(token as! EndArrayToken)
        }
        else if (token is StartModuleToken) {
            try self.handle_start_module_token(token as! StartModuleToken)
        }
        else if (token is EndModuleToken) {
            try self.handle_end_module_token(token as! EndModuleToken)
        }
        else if (token is StartDefinitionToken) {
            try self.handle_start_definition_token(token as! StartDefinitionToken)
        }
        else if (token is EndDefinitionToken) {
            try self.handle_end_definition_token(token as! EndDefinitionToken)
        }
        else if (token is WordToken) {
            try self.handle_word_token(token as! WordToken)
        }
        else {
            throw InterpreterError.UNKNOWN_TOKEN_ERROR(token: token)
        }
    }
    
    private func handle_string_token(_ token: StringToken) throws {
        try self.handle_word(PushValueWord(name: "<string>", value: token.str))
    }
    
    private func handle_comment_token(_ token: CommentToken) {
    }
    
    private func handle_start_array_token(_ token: StartArrayToken) throws {
        try self.handle_word(PushValueWord(name: "[", value: token))
    }
    
    private func handle_end_array_token(_ token: EndArrayToken) throws {
        try self.handle_word(EndArrayWord())
    }
    
    /// NOTE: This is treated as an IMMEDIATE word and is executed even if we're compiling
    private func handle_start_module_token(_ token: StartModuleToken) throws {
        let word = StartModuleWord(name: token.name)
        if (self.is_compiling) {
            if (self.cur_definition == nil) {
                throw InterpreterError.COMPILING_WITHOUT_CUR_DEFINITION
            }
            self.cur_definition?.add_word(word: word)
        }
        try word.execute(interp: self)
    }
    
    private func handle_end_module_token(_ token: EndModuleToken) throws {
        let word = EndModuleWord()
        if (self.is_compiling) {
            if (self.cur_definition == nil) {
                throw InterpreterError.COMPILING_WITHOUT_CUR_DEFINITION
            }
            self.cur_definition?.add_word(word: word)
        }
        try word.execute(interp: self)
    }
    
    private func handle_start_definition_token(_ token: StartDefinitionToken) throws {
        if (self.is_compiling) {
            throw InterpreterError.NESTED_DEFINITION_ERROR
        }
        self.cur_definition = DefinitionWord(name: token.name)
        self.is_compiling = true
    }
    
    private func handle_end_definition_token(_ token: EndDefinitionToken) throws {
        if (!self.is_compiling) {
            throw InterpreterError.UNMATCHED_END_DEFINITION_ERROR
        }
        if (self.cur_definition == nil) {
            throw InterpreterError.COMPILING_WITHOUT_CUR_DEFINITION
        }
        self.cur_module().add_word(word: self.cur_definition!)
        self.is_compiling = false
    }
    
    private func handle_word_token(_ token: WordToken) throws {
        let word = self.find_word(name: token.name)
        if (word == nil) {
            throw InterpreterError.UNKNOWN_WORD_ERROR(word_name: token.name)
        }
        try self.handle_word(word!)
    }

    private func handle_word(_ word: Word) throws {
        if (self.is_compiling) {
            if (self.cur_definition == nil) {
                throw InterpreterError.COMPILING_WITHOUT_CUR_DEFINITION
            }
            self.cur_definition?.add_word(word: word)
        }
        else {
            try word.execute(interp: self)
        }
    }
}
