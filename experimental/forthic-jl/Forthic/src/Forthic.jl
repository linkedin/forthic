module Forthic

include("./tokenizer.jl")
include("./module.jl")
include("./global_module.jl")
include("./interpreter.jl")

function test()
    println("Starting test!")
    forthic = read(open("./test.forthic", "r"), String)

    # tokenizer = Tokenizer.new_tokenizer(raw"{module : GREETING   HOWDY;} GREETING  ")
    tokenizer = new_tokenizer(forthic)
    while true
        token = next_token(tokenizer)
        println(token)
        if token.type == TOK_EOS   break end
    end


    # my_module = new_Module("my-module", "")
    # println(my_module)
end

function test_interp()
    forthic = read(open("./test_interp.forthic", "r"), String)
    interp = new_Interpreter()
    run(interp, forthic)
    for item in interp.stack
        println(item)
    end
end

end