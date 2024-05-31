# ----- DefinitionWord -----------------------------------------------------------------------------
mutable struct DefinitionWord
    name::String
    words
end

function new_DefinitionWord(name::String)
    return DefinitionWord(name, [])
end

function add_word(self::DefinitionWord, word)
    push!(self.words, word)
end

function execute(interp, self::DefinitionWord)
    for w in self.words
        execute(interp, w)
    end
end

# ----- EvalStringWord -----------------------------------------------------------------------------
mutable struct EvalStringWord
    name::String
    eval_string::String
end

function execute(interp, self::EvalStringWord)
    # NOTE: This essentially opens *everything* up. Remove this if you are at all concerned
    #       about injection attacks
    result = eval(Meta.parse(self.eval_string))
    push!(interp.stack, result)
end

# ----- StartModuleWord ----------------------------------------------------------------------------
mutable struct StartModuleWord
    name::String
end

function execute(interp, self::StartModuleWord)
    # The app module is the only module with a blank name
    if self.name == ""
        module_stack_push(interp, interp.app_module)
        return
    end

    # If the module is used by the current module, push it onto the stack, otherwise
    # create a new module.
    mod = find_module(cur_module(interp), self.name)
    if ismissing(mod)
        mod = new_Module(self.name, "")
        register_module(cur_module(interp), mod.name, mod)
    end
    push!(interp.module_stack, mod)
end

mutable struct EndModuleWord
end

function execute(intepr, self::EndModuleWord)
    pop!(interp.module_stack)
end


mutable struct EndArrayWord
end


# ----- Interpreter --------------------------------------------------------------------------------
mutable struct Interpreter
    stack
    global_module
    app_module
    module_stack
    registered_modules
    is_compiling
    cur_definition
end

function new_Interpreter()
    app_module = new_Module("", "")
    return Interpreter([], new_GlobalModule(), app_module, [app_module], Dict(), false, missing)
end

function run(self::Interpreter, forthic::String)
    tokenizer = new_tokenizer(forthic)
    token = next_token(tokenizer)
    while token.type != TOK_EOS
        handle_token(self, token)
        token = next_token(tokenizer)
    end
end

function handle_token(self::Interpreter, token::Token)
    if token.type == TOK_STRING             handle_string_token(self, token)
    elseif token.type == TOK_COMMENT        handle_comment_token(self, token)
    elseif token.type == TOK_START_ARRAY    handle_start_array_token(self, token)
    elseif token.type == TOK_END_ARRAY      handle_end_array_token(self, token)
    elseif token.type == TOK_START_MODULE   handle_start_module_token(self, token)
    elseif token.type == TOK_END_MODULE     handle_end_module_token(self, token)
    elseif token.type == TOK_START_DEF      handle_start_definition_token(self, token)
    elseif token.type == TOK_END_DEF        handle_end_definition_token(self, token)
    elseif token.type == TOK_WORD           handle_word_token(self, token)
    elseif token.type == TOK_EVAL_STRING    handle_eval_string_token(self, token)
    else throw(error("Unknown token: $token"))
    end
end

function handle_string_token(self::Interpreter, token::Token)
    handle_word(self, PushValueWord("<string>", token.string))
end

function handle_eval_string_token(self::Interpreter, token::Token)
    handle_word(self, EvalStringWord("<eval_string>", token.string))
end

function handle_comment_token(self::Interpreter, token::Token)
    println("Comment: $(token.string)")
end

function handle_start_array_token(self::Interpreter, token::Token)
    handle_word(self, PushValueWord("<start_array_token>", token))
end

function handle_end_array_token(self::Interpreter, token::Token)
    handle_word(self, EndArrayWord())
end

function handle_start_module_token(self::Interpreter, token::Token)
    word = StartModuleWord(token.string)
    if self.is_compiling   add_word(self.cur_definition, word) end
    execute(self, word)
end

function handle_end_module_token(self::Interpreter, token::Token)
    word = EndModuleWord(token.string)
    if self.is_compiling   add_word(self.cur_definition, word) end
    execute(self, word)
end

function handle_start_definition_token(self::Interpreter, token::Token)
    if self.is_compiling   throw(error("Can't have nested definitions")) end
    self.cur_definition = new_DefinitionWord(token.string)
    self.is_compiling = true
end

function handle_end_definition_token(self::Interpreter, token::Token)
    if !self.is_compiling   throw(error("Unmatched end definition")) end
    add_word(cur_module(self), self.cur_definition)
    self.is_compiling = false
end

function handle_word_token(self::Interpreter, token::Token)
    word = find_word(self, token.string)
    if ismissing(word)   throw(error("Unknown word: $(token.string)")) end
    handle_word(self, word)
end

function handle_word(self::Interpreter, word)
    if self.is_compiling
        add_word(self.cur_definition, word)
    else
        execute(self, word)
    end
end


function cur_module(self::Interpreter)
    result = last(self.module_stack)
end

function find_word(self::Interpreter, name::String)
    result = missing
    for m::Mod in reverse(self.module_stack)
        result = find_word(m, name)
        if !ismissing(result)   break end
    end
    if ismissing(result)
        result = find_word(self.global_module, name)
    end
    return result
end

function find_module(self::Interpreter, name::String)
    if name in keys(self.registered_modules)
        return self.registered_modules[name]
    else
        throw(error("Can't find module: $name"))
    end
end


# export class Interpreter implements IInterpreter {


#     async run_in_module(module: Module, forthic: string): Promise<void> {
#         self.module_stack_push(module);
#         await self.run(forthic);
#         self.module_stack_pop();
#     }



#     register_module(module: Module): void {
#         self.registered_modules[module.name] = module;
#     }

#     async run_module_code(module: Module): Promise<void> {
#         self.module_stack_push(module);
#         await self.run(module.forthic_code);
#         self.module_stack_pop();
#     }



function execute(interp::Interpreter, word::EndArrayWord)
    items = []

    while true
        item = pop!(interp.stack)
        if typeof(item) == Token && item.type == TOK_START_ARRAY break end
        push!(items, item)
    end
    result = reverse(items)
    push!(interp.stack, result)
end
