import {Interpreter} from "./interpreter"

it('Has a clean initial state', () => {
    let interp = new Interpreter()
    expect(interp.stack).toEqual([])
    expect(interp.module_stack[0].name).toEqual("")
});

it('Can push a string onto the stack', async () => {
    let interp = new Interpreter()
    await interp.run("'Howdy'")
    expect(interp.stack[0]).toEqual("Howdy")
})

it('Can skip comments', async () => {
    let interp = new Interpreter()
    await interp.run("# A comment")
    await interp.run("#Another comment")
    expect(interp.stack).toEqual([])
})

it ('Can create empty arrays', async () => {
    let interp = new Interpreter()
    await interp.run("[]")
    expect(interp.stack[0]).toEqual([])
})

it('Can have nested modules', async () => {
    let interp = new Interpreter()

    // Push application module onto the stack
    await interp.run("{")
    expect(interp.module_stack.length).toEqual(2)
    expect(interp.module_stack[0]).toEqual(interp.module_stack[1])

    // Push module-A onto module stack
    interp = new Interpreter()
    await interp.run("{module-A")
    expect(interp.module_stack.length).toEqual(2)
    expect(interp.module_stack[1].name).toEqual("module-A")
    expect(interp.app_module.modules['module-A']).toBeTruthy()

    // Push module-A and then module-B onto module stack
    interp = new Interpreter()
    await interp.run("{module-A {module-B")
    expect(interp.module_stack.length).toEqual(3)
    expect(interp.module_stack[1].name).toEqual("module-A")
    expect(interp.module_stack[2].name).toEqual("module-B")
    expect(interp.module_stack[1].modules["module-B"]).toBeTruthy()

    await interp.run("}}")
    expect(interp.module_stack.length).toEqual(1)
    expect(interp.module_stack[0]).toEqual(interp.app_module)
})

it('Can define new words', async () => {
    // Can define and find a word in the app module
    let interp = new Interpreter()
    await interp.run(": NOTHING   ;")
    expect(interp.app_module.find_word("NOTHING")).toBeTruthy()

    // Words defined in other modules aren't automatically available in the app module
    interp = new Interpreter()
    await interp.run("{module-A   : NOTHING   ;}")
    expect(interp.app_module.find_word("NOTHING")).toBeUndefined()

    let module_A = interp.app_module.modules["module-A"]
    expect(module_A.find_word("NOTHING")).toBeTruthy()
})

it('Can memoize definitions', async () => {
    let interp = new Interpreter()

    // Memoization creates 3 words
    await interp.run("@: MY-MEMO   ;")
    let defined_words = ["MY-MEMO", "MY-MEMO!", "MY-MEMO!@"]
    for (let name of defined_words) {
        expect(interp.app_module.find_word(name)).toBeTruthy()
    }

    // Test storing a value and retrieving it
    await interp.run("41 MY-MEMO!")
    await interp.run("MY-MEMO")
    expect(interp.stack[0]).toEqual(41)

    // Test refreshing a value
    interp.stack = []
    await interp.run("81 MY-MEMO!")
    await interp.run("MY-MEMO")
    expect(interp.stack[0]).toEqual(81)

    // Test refreshing a value of a memo while leaving the value on the stack
    interp.stack = []
    await interp.run("101 MY-MEMO!@")
    expect(interp.stack.length).toEqual(1)
    expect(interp.stack[0]).toEqual(101)
    interp.stack = []
    interp.run("MY-MEMO")
    expect(interp.stack[0]).toEqual(101)
})

it('Can defined in enclosing modules', async () => {
    let interp = new Interpreter()
    await interp.run(`
        : APP-MESSAGE   "Hello (from app)";
        {module1
            APP-MESSAGE
        }
    `)
    expect(interp.stack[0]).toEqual("Hello (from app)")
})

it('Can open up an existing module and do things in it', async () => {
    // Can execute an internal module word from a definition in another module
    let interp = new Interpreter()
    await interp.run(`
        {mymodule
            : MESSAGE   "Hello (from mymodule)";
        }
        : MESSAGE   {mymodule MESSAGE };
        MESSAGE
    `)
    expect(interp.stack[0]).toEqual("Hello (from mymodule)")

    // Can access an internal memo-ized word from a definition in another module
    interp = new Interpreter()
    await interp.run(`
        {mymodule
        @: MESSAGE-MEMO   "Hello (from mymodule memo)";
        }
        : MESSAGE   {mymodule MESSAGE-MEMO };
        MESSAGE
    `)
    expect(interp.stack[0]).toEqual("Hello (from mymodule memo)")
})

it('Searches for words and literals in the global module', async () => {
    let interp = new Interpreter()
    await interp.run("'Hi'")
    expect(interp.stack[0]).toEqual("Hi")
    await interp.run("POP")
    expect(interp.stack.length).toEqual(0)
})
