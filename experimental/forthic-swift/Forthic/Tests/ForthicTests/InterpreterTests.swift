import XCTest
@testable import Forthic

final class InterpreterTests: XCTestCase {
    func test_initial_state() throws {
        let interp = Interpreter()
        XCTAssertEqual(interp.stack.count, 0)
        XCTAssertEqual(interp.module_stack[0].name, "")
    }
    
    func test_push_string() throws {
        let interp = Interpreter()
        try interp.run(forthic: "'Howdy'")
        XCTAssertEqual(interp.stack[0] as! String, "Howdy")
    }
    
    func test_comment() throws {
        let interp = Interpreter()
        try interp.run(forthic: "# A comment")
        try interp.run(forthic: "#Also a comment")
        XCTAssertEqual(interp.stack.count, 0)
    }
    
    func test_empty_array() throws {
        let interp = Interpreter()
        try interp.run(forthic: "[]")
        let result: [Any] = (try interp.stack_pop()) as! [Any]
        XCTAssertEqual(result.count, 0)
    }
    
    func test_start_module() throws {
        var interp = Interpreter()
        
        // Push application module onto module stack
        try interp.run(forthic: "{")
        XCTAssertEqual(interp.module_stack.count, 2)
        XCTAssertEqual(interp.module_stack[0].name, interp.module_stack[1].name)

        // Push module-A onto module stack
        interp = Interpreter()
        try interp.run(forthic: "{module-A")
        XCTAssertEqual(interp.module_stack.count, 2)
        XCTAssertEqual(interp.module_stack[1].name, "module-A")
        XCTAssertNotNil(interp.app_module().modules["module-A"])
        
        // Push module-A and then module-B onto module stack
        interp = Interpreter()
        try interp.run(forthic: "{module-A {module-B")
        XCTAssertEqual(interp.module_stack.count, 3)
        XCTAssertEqual(interp.module_stack[0].name, "")
        XCTAssertEqual(interp.module_stack[1].name, "module-A")
        XCTAssertEqual(interp.module_stack[2].name, "module-B")
        
        let module_A = interp.app_module().modules["module-A"]!
        XCTAssertNotNil(module_A.modules["module-B"])
        
        try interp.run(forthic: "}}")
        XCTAssertEqual(interp.module_stack.count, 1)
        XCTAssertEqual(interp.module_stack[0].name, "")
    }
    
    func test_definition() throws {
        // Define and find a word in the app module
        var interp = Interpreter()
        try interp.run(forthic: ": NOTHING   ;")
        var word = interp.app_module().find_word(name: "NOTHING")
        XCTAssertNotNil(word)
        
        // Check that words defined in other modules aren't automatically available in the app module
        interp = Interpreter()
        try interp.run(forthic: "{module-A  : NOTHING   ;}")
        word = interp.app_module().find_word(name: "NOTHING")
        XCTAssertNil(word)
        
        // But words defined in other modules are actually present
        let module_A: Module = interp.app_module().modules["module-A"]!
        word = module_A.find_word(name: "NOTHING")
        XCTAssertNotNil(word)
    }
    
    func test_word_scope() throws {
        // Words defined in the application module are available from other modules
        let interp = Interpreter()
        try interp.run(forthic: """
        : APP-MESSAGE   "Hello (from app)";
        {module1
            APP-MESSAGE
        }
        """)
        XCTAssertEqual(interp.stack[0] as! String, "Hello (from app)")
    }
    
    func test_open_module() throws {
        // Define a word in a module and then refer to it from the app module by "opening up" the module
        var interp = Interpreter()
        try interp.run(forthic: """
        {mymodule
            : MESSAGE   "Hello (from mymodule)";
        }
        : MESSAGE   {mymodule MESSAGE };
        MESSAGE
        """)
        XCTAssertEqual(interp.stack[0] as! String, "Hello (from mymodule)")

        // Try same test but with MEMO
        interp = Interpreter()
        // TODO: Uncomment this when MEMO has been implemented
//        try interp.run(forthic: """
//        {mymodule
//            'MESSAGE-MEMO'   '"Hello (from mymodule memo)"'   MEMO
//        }
//        : MESSAGE   {mymodule MESSAGE-MEMO };
//        MESSAGE
//        """)
//        XCTAssertEqual(interp.stack[0] as! String, "Hello (from mymodule memo)")
    }
    
    func test_word() throws {
        var interp = Interpreter()
        try interp.run(forthic: ": MESSAGE   'Howdy';")
        try interp.run(forthic: "MESSAGE")
        XCTAssertEqual(interp.stack[0] as! String, "Howdy")

        interp = Interpreter()
        try interp.run(forthic: "{module-A {module-B   : MESSAGE   'In module-B' ;}}")
        try interp.run(forthic: "{module-A {module-B   MESSAGE}}")
        XCTAssertEqual(interp.stack[0] as! String, "In module-B")
    }
    
    func test_search_global_module() throws {
        let interp = Interpreter()
        try interp.run(forthic: "'Hi'")
        XCTAssertEqual(interp.stack.count, 1)

        try interp.run(forthic: "POP")
        XCTAssertEqual(interp.stack.count, 0)
    }
    
    func test_use_module() throws {
        let interp = Interpreter()
        interp.register_module(module: SampleDateModule(interp: interp))
        
        // TODO: Add USE-MODULES
        try interp.run(forthic: "['sample-date'] USE-MODULES")
        
        // Execute imported word with prefix
        try interp.run(forthic: "sample-date.MY-TODAY")
        let today = Date()
        XCTAssertEqual((interp.stack[0] as! Date).description, today.description)
        
        // Execute word by opening up the sample-date module
        try interp.run(forthic: "{sample-date MY-TODAY}")
        XCTAssertEqual((interp.stack[1] as! Date).description, today.description)

        // Use module with an empty prefix
        try interp.run(forthic: "[['sample-date' '']] USE-MODULES")
        try interp.run(forthic: "MY-TODAY")
        XCTAssertEqual((interp.stack[2] as! Date).description, today.description)
    }
}


/// Used as a sample module for testing
class SampleDateModule : Module {
    init(interp: Interpreter) {
        super.init(name: "sample-date", interp: interp, forthic: "")
        self.add_module_word(word_name: "MY-TODAY", word_handler: self.word_MY_TODAY)
    }
    
    // ( -- today )
    func word_MY_TODAY(interp: Interpreter) throws {
        let result = Date()
        interp.stack_push(result)
    }
}
