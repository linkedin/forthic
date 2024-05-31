include("./module.jl")
include("./tokenizer.jl")
include("./interpreter.jl")

import .Forthic

tokenizer = Forthic.new_tokenizer("# Howdy")
# forthic = read(open("./test.forthic", "r"), String)

# # tokenizer = Tokenizer.new_tokenizer(raw"{module : GREETING   HOWDY;} GREETING  ")
# tokenizer = Forthic.new_tokenizer(forthic)
# while true
#     token = Forthic.next_token(tokenizer)
#     println(token)
#     if token.type == Forthic.TOK_EOS   break end
# end


# my_module = new_Module("my-module", "")
# println(my_module)