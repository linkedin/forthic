mutable struct Variable
    value
end

mutable struct Mod
    words
    exportable
    variables
    modules
    required_modules
    name
    forthic_code
end

function new_Module(name::String, forthic::String)
    return Mod([], Set([]), Dict(), Dict(), [], name, forthic)
end

mutable struct PushValueWord
    name::String
    value
end


mutable struct ModuleWord
    name::String
    handler
end



mutable struct ImportedWord
    name::String
    module_word::ModuleWord
    imported_module::Mod
end

function new_ImportedWord(module_word::ModuleWord, prefix::String, mod::Mod)
    if prefix != ""   prefix = prefix * "." end
    result = ImportedWord("$prefix" * module_word.name, module_word, mod)
    return result
end


function execute(interp, word::PushValueWord)
    push!(interp.stack, word.value)
end

function execute(interp, word::ModuleWord)
    word.handler(interp)
end

function execute(interp, word::ImportedWord)
    push!(interp.module_stack, word.imported_module)
    execute(interp, word.module_word)
    pop!(interp.module_stack)
end

# ----- Mod functions ---------------------------------------------------------------------------

function require_module(self::Mod, prefix::String, mod::Mod)
    push!(self.required_modules, Dict([("prefix", prefix), ("module", mod)]))
end

function find_module(self::Mod, name::String)
    if name in keys(self.modules)
        return self.modules[name]
    else
        return missing
    end
end

function add_exportable_word(self::Mod, word::ModuleWord)
    push!(self.words, word)
    push!(self.exportable, word.name)
end

function add_module_word(self::Mod, word_name::String, word_handler)
    add_exportable_word(self, ModuleWord(word_name, word_handler))
end

function add_word(self::Mod, word)
    push!(self.words, word)
end

function add_exportable(self::Mod, names::Array{String})
    self.exportable =  vcat(self.exportable, names)
end

function exportable_words(self::Mod)
    result = filter(w -> w.name in self.exportable, self.words)
    return result
end

function add_variable(self::Mod, name::String, value)
    if !(name in keys(self.variables))
        self.variables[name] = Variable(value)
    end
end

function import_module(self::Mod, module_name::String, mod::Mod, interp)
    if module_name in keys(self.modules)
        new_module = self.modules[module_name]
    else
        new_module = mod
        initialize(new_module, interp)
    end

    for word in exportable_words(new_module)
        add_word(self, new_ImportedWord(word, module_name, new_module))
    end
    register_module(self, module_name, new_module)
end

function register_module(self::Mod, module_name::String, mod::Mod)
    self.modules[module_name] = mod
end


function initialize(self::Mod, interp)
    for rec in self.required_modules
        import_module(self, rec.prefix, rec.module, interp)
    end
    interp.run_module_code(self, interp)
end

function find_word(self::Mod, name::String)
    result = find_dictionary_word(self, name)
    if ismissing(result)   result = find_variable(self, name) end
    return result
end

function find_dictionary_word(self::Mod, word_name::String)
    index = length(self.words)
    while index > 0
        w = self.words[index]
        if w.name == word_name   return w end
        index -= 1
    end
    return missing
end

function find_variable(self::Mod, varname::String)
    if varname in keys(self.variables)
        result = PushValueWord(varname, self.variables[varname])
    else
        result = missing
    end
    return result
end

function set_variable(self::Mod, varname::String, value)
    self.variables[varname] = Variable(value)
end
