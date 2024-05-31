using Dates

mutable struct MemoWord
    name::String
    varname::String
end

function execute(interp, self::MemoWord)
    run(interp, "$(self.varname) @")
    var_value = pop!(interp.stack)
    if ismissing(var_value)   run(interp, "$(self.name)!") end

    # Return value of variable (on stack)
    run(interp, "$(self.varname) @")
end

function to_int(string::String)
    try
        result = parse(Int, string)
        return result
    catch
        return missing
    end
end


function to_float(string::String)
    try
        result = parse(Float64, string)
        return result
    catch
        return missing
    end
end

function to_date(string::String)
    m = match(r"(\d{4})-(\d{2})-(\d{2})", string)
    if m === nothing   return missing end
    year = parse(Int, m[1])
    month = parse(Int, m[2])
    day = parse(Int, m[3])
    result = Date(year, month, day)
    return result
end

mutable struct GlobalModule
    mod
    literal_handlers
end

function new_GlobalModule()

    mod = new_Module("<global>", "")

    add_module_word(mod, "VARIABLES", word_VARIABLES)
    add_module_word(mod, "!", word_bang)
    add_module_word(mod, "@", word_at)
    add_module_word(mod, "MEMO", word_MEMO)
    add_module_word(mod, "NULL", word_NULL)
    add_module_word(mod, "INTERPRET", word_INTERPRET)
    add_module_word(mod, "EXPORT", word_EXPORT)
    add_module_word(mod, "USE-MODULES", word_USE_MODULES)
    add_module_word(mod, "REC", word_REC)
    add_module_word(mod, "REC@", word_REC_at)
    add_module_word(mod, "<REC!", word_l_REC_bang)
    add_module_word(mod, "EVAL", word_EVAL)

    add_module_word(mod, "TRUE", word_TRUE)
    add_module_word(mod, "FALSE", word_FALSE)

    literal_handlers = [
        to_int,
        to_float,
        to_date
    ]
    result = GlobalModule(mod, literal_handlers)
    return result
end


function find_word(self::GlobalModule, name::String)
    result = find_word(self.mod, name)
    if ismissing(result)   result = find_literal_word(self, name) end
    return result
end


function find_literal_word(self::GlobalModule, string::String)
    for h in self.literal_handlers
        value = h(string)
        if !ismissing(value)   return PushValueWord(string, value) end;
    end
    return missing
end


# ( varnames -- )
function word_VARIABLES(interp)
    varnames = pop!(interp.stack)
    mod = cur_module(interp)
    for v in varnames
        add_variable(mod, v, missing)
    end
end

# ( value variable -- )
function word_bang(interp)
    variable = pop!(interp.stack)
    value = pop!(interp.stack)
    variable.value = value
end

# ( variable -- value )
function word_at(interp)
    variable = pop!(interp.stack)
    push!(interp.stack, variable.value)
end

# (  -- missing )
function word_NULL(interp)
    push!(interp.stack, missing)
end

# ( name forthic -- )
function word_MEMO(interp)
    forthic = pop!(interp.stack)
    name = pop!(interp.stack)
    name_bang = "$(name)!"
    name_bang_at = "$(name)!@"
    var_name = "<memo_var_$(name)>"

    # Create variable
    run(interp, "['$(var_name)'] VARIABLES  NULL $(var_name) !")

    # name! word
    run(interp, ": $(name_bang)   $(forthic) $(var_name) !;")

    # name!@ word
    run(interp, ": $(name_bang_at)   $(name_bang) $(var_name) @;")

    # name word
    add_word(cur_module(interp), MemoWord(name, var_name))
end

function word_INTERPRET(interp)
    run(interp, pop!(interp.stack))
end

# ( names -- )
function word_EXPORT(interp)
    names = pop!(interp.stack)
    add_exportable(cur_module(interp), names)
end

# ( names -- )
function word_USE_MODULES(interp)
    names = pop!(interp.stack)
    cur_mod = cur_module(interp)
    if cur_mod != interp.app_module   throw(error("USE-MODULES can only be called within the app module")) end

    module_name = ""
    prefix = ""

    for name in names
        if typeof(name) == Vector{String}
            module_name = name[0]
            prefix = name[1]
        else
            module_name = name
            prefix = name
        end
        mod = find_module(interp, module_name)
        import_module(cur_mod, prefix, mod, interp)
    end
end

# ( key_vals -- rec )
function word_REC(interp)
    key_vals = pop!(interp.stack)

    result = Dict()
    for pair in key_vals
        key = missing
        val = missing
        if !ismissing(pair)
            if length(pair) >= 1   key = pair[1] end
            if length(pair) >= 2   val = pair[2] end
        end
        result[key] = val
    end
    push!(interp.stack, result)
end

# ( rec field -- value )
# ( rec fields -- value )
function word_REC_at(interp)
    field = pop!(interp.stack)
    rec = pop!(interp.stack)
    if ismissing(rec)
        push!(interp.stack, missing)
        return
    end

    fields = []
    if typeof(field) <: Vector
        fields = field
    else
        fields = [field]
    end
    result = drill_for_value(rec, fields)
    push!(interp.stack, result)
end



# ( rec value field -- rec )
function word_l_REC_bang(interp)
    field = pop!(interp.stack)
    value = pop!(interp.stack)
    rec = pop!(interp.stack)

    if ismissing(rec)   rec = Dict() end
    fields = []
    if typeof(field) <: Vector
        fields = field
    else
        fields = [field]
    end

    function ensure_field(rec, field)
        res = get_rec_val(rec, field)
        if ismissing(res)
            res = Dict()
            rec[field] = res
        end
        return res
    end

    cur_rec = rec
    # Drill down up until the last value
    for f in fields[1:length(fields)-1]
        cur_rec = ensure_field(cur_rec, f)
    end

    # Set the value at the right depth within rec
    cur_rec[fields[length(fields)]] = value

    push!(interp.stack, rec)
end

# ( eval_string -- result )
function word_EVAL(interp)
    eval_string = pop!(interp.stack)
    result = eval(Meta.parse(eval_string))
    push!(interp.stack, result)
end


function word_TRUE(interp)
    push!(interp.stack, true)
end

function word_FALSE(interp)
    push!(interp.stack, false)
end

# Descends into record using an array of fields, returning final value or null
function drill_for_value(record, fields)
    result = record
    for f in fields
        if ismissing(result)   return missing end
        result = get_rec_val(result, f)
    end
    return result
end

function get_rec_val(record, field)
    try
        return record[field]
    catch e
        if typeof(e) == KeyError
            return missing
        else
            throw(e)
        end
    end
end