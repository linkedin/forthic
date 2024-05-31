/// Module.swift
///
/// Implements Module and classes that are part of a Module: Variable, Word, PushValueWord, ModuleWord, ImportedWord
///

typealias WordHandler = (Interpreter) throws -> Void

enum ModuleError: Error {
    case EXECUTE_MUST_BE_OVERRIDDEN(word: Word)
}


/// Represents a Forthic variable
class Variable {
    var value: Any?
    
    init(value: Any?) {
        self.value = value
    }
    
    func set_value(value: Any?) {
        self.value = value
    }
    
    func get_value() -> Any? {
        return self.value
    }
}

/// Base class for all Forthic words
class Word {
    var name: String

    init(name: String) {
        self.name = name
    }
    
    func execute(interp: Interpreter) throws {
        throw ModuleError.EXECUTE_MUST_BE_OVERRIDDEN(word: self)
    }
}


/// This word pushes a value onto the stack
class PushValueWord : Word {
    var value: Any

    init(name: String, value: Any) {
        self.value = value
        super.init(name: name)
    }
    
    override func execute(interp: Interpreter) throws {
        interp.stack_push(self.value)
    }
}


/// This is used when defining Forthic words in Swift
///
/// - Parameter name: Name of word
/// - Parameter handler: Swift function that's called when the word is executed. All handlers take an interpreter as their only argument and return Void.
///     All argument passing and results are h andled via the interpreter stack
class ModuleWord : Word {
    var handler: WordHandler
    
    init(name: String, handler: @escaping WordHandler) {
        self.handler = handler
        super.init(name: name)
    }
    
    override func execute(interp: Interpreter) throws {
        try self.handler(interp)
    }
}


/// This represents a word imported from another module
///
/// Words imported from other modules usually have their module name as a prefix (e.g., `jira.SEARCH`), but it's also possible to use a different prefix, or none at all
class ImportedWord : Word {
    var word: Word
    var imported_module: Module
    
    init(word: Word, prefix: String, module: Module) {
        self.word = word
        self.imported_module = module

        let word_prefix: String
        if (prefix != "") {
            word_prefix = "\(prefix)."
        }
        else {
            word_prefix = prefix
        }
        super.init(name: "\(word_prefix)\(word.name)")
    }
    
    override func execute(interp: Interpreter) throws {
        interp.module_stack_push(module: self.imported_module)
        try self.word.execute(interp: interp)
        try interp.module_stack_pop()
    }
}


/// A Module is essentially a collection of variables and words and potentially other Modules
class Module {
    var interp: Interpreter
    var words: [Word] = []
    var exportable: [String] = []
    var variables: [String: Variable] = [:]
    var modules: [String: Module] = [:]
    var name: String
    var forthic: String
    
    init(name: String, interp: Interpreter, forthic: String) {
        self.name = name
        self.interp = interp
        self.forthic = forthic
    }
    
    func find_module(name: String) -> Module? {
        return self.modules[name]
    }
    
    /// Adds word to module
    func add_word(word: Word) {
        self.words.append(word)
    }
    
    /// Convenience function to add an exportable module word
    func add_module_word(word_name: String, word_handler: @escaping WordHandler) {
        self.add_exportable_word(word: ModuleWord(name: word_name, handler: word_handler))
    }
    
    /// Marks a word as being exportable by the module
    func add_exportable_word(word: ModuleWord) {
        self.words.append(word)
        self.exportable.append(word.name)
    }
    
    /// Convenience to add a set of exportable words. This is used when marking words as exportable from Forthic
    func add_exportable(names: [String]) {
        self.exportable += names
    }
    
    func exportable_words() -> [Word] {
        var result: [Word] = []
        for w in self.words {
            if (self.exportable.contains(w.name)) {
                result.append(w)
            }
        }
        return result
    }
    
    /// Adds variable to module; nop if variable exists
    func add_variable(name: String, value: Any? = nil) {
        if (self.variables.keys.contains(name)) {
            return
        }
        self.variables[name] = Variable(value: value)
    }
    
    /// When a module is imported, its `forthic` must be executed in order to fully define its words
    func initialize(interp: Interpreter) throws {
        try interp.run_in_module(module: self, forthic: self.forthic)
    }
    
    func register_module(module_name: String, module: Module) {
        self.modules[module_name] = module
    }
    
    
    /// This is used to import a module for use by another module.
    ///
    /// Typically, modules are independent. But in some cases, a module may depend on other modules. When this is the case,
    /// `import_module`is used to import modules at code time
    func import_module(module_name: String, module: Module, interp: Interpreter) throws {
        let new_module: Module
        
        // If module is already registered, use it
        if (self.modules.keys.contains(module_name)) {
            new_module = self.modules[module_name]!
        }
        else {
            new_module = module
            try new_module.initialize(interp: interp)
        }
        
        let words = new_module.exportable_words()
        for word in words {
            self.add_word(word: ImportedWord(word: word, prefix: module_name, module: self))
        }
        self.register_module(module_name: module_name, module: new_module)
    }
    
    /// Finds word/variable in module
    func find_word(name: String) -> Word? {
        var result = self.find_dictionary_word(word_name: name)
        if (result == nil) {
            result = self.find_variable(varname: name)
        }
        return result
    }
    
    /// Looks up word in module
    func find_dictionary_word(word_name: String) -> Word? {
        for w in self.words.reversed() {
            if (w.name == word_name) {
                return w
            }
        }
        return nil
    }
    
    /// Looks up Variable in module, wrapping it in a `PushValueWord`
    func find_variable(varname: String) -> PushValueWord? {
        let variable = self.variables[varname]
        let result: PushValueWord?
        if (variable != nil) {
            result = PushValueWord(name: varname, value: variable as Any)
        }
        else {
            result = nil
        }
        return result
    }
}
