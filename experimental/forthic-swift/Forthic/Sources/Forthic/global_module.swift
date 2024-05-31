import Foundation
import OrderedCollections

/// This implements the standard `global` module words
///
/// The `GlobalModule` is a special module because it always the last one searched for Forthic words. Because
/// of this, it is also responsible for handling "literal words" that push themselves onto the stack. These
/// are words like "1", "2.5", "06-05-2021", etc.

/// The `GlobalModule` also implements base words that might usually be built into a language, like
/// `MEMO`, `VARIABLES`, `!`, `@`, etc.

enum GlobalModuleError: Error {
    case INVALID_TIME(value: Any?)
    case USE_MODULES_ONLY_FROM_APP_MODULE(module: Module)
    case STRING_LIST_EXPECTED(value: Any?)
    case VARIABLE_EXPECTED(value: Any?)
    case STRING_EXPECTED(value: Any?)
    case NUMBER_EXPECTED(value: Any?)
    case INTEGER_EXPECTED(value: Any?)
    case DATE_EXPECTED(value: Any?)
    case BOOL_EXPECTED(value: Any?)
    case KEY_VALS_EXPECTED(value: Any?)
    case PAIR_EXPECTED(value: Any?)
    case RECORD_EXPECTED(value: Any?)
    case LOAD_SCREEN_CYCLE(value: Any?)
    case LIST_EXPECTED(value: Any?)
    case SAME_LENGTH_ARRAYS_EXPECTED(array1: Any?, array2: Any?)
    case POSITIVE_INTEGER_EXPECTED(value: Any?)
    case MATCHING_CONTAINERS_EXPECTED(container1: Any?, container2: Any?)
    case COMPARABLE_ITEMS_EXPECTED(item1: Any?, item2: Any?)
    case COMPARABLE_LIST_EXPECTED(item: Any?)
    case HASHABLE_EXPECTED(item: Any?)
    case TEXT_CHECKING_RESULT_EXPECTED(item: Any?)
    case CANNOT_DUMP_AS_JSON(item: Any?)
    case STACK_DUMP(item: [Any?])
}

typealias List = [Any?]
typealias Record = OrderedDictionary<AnyHashable, Any>

class MemoWord : Word {
    var varname: String

    init(name: String, varname: String) {
        self.varname = varname
        super.init(name: name)
    }

    override func execute(interp: Interpreter) throws {
        try interp.run(forthic: "\(self.varname) @")
        let var_value: Any? = try interp.stack_pop()
        if (var_value == nil) {
            try interp.run(forthic: "\(self.name)!")
        }

        // Return value of variable
        try interp.run(forthic: "\(self.varname) @")
    }

}

class GlobalModule : Module {
    var timezone: TimeZone
    var date_formatter = DateFormatter()
    var time_formatter = DateFormatter()

    // "Screens" of Forthic code can be loaded from disk/memory. Since screens can load other screens,
    // we need to be careful not to get into a loop. The `active_screens` keeps track of this.
    var active_screens = Set<String>()
    var literal_handlers: [(String) -> Any?] = []

    init(interp: Interpreter, timezone: TimeZone) {
        self.timezone = timezone

        super.init(name: "<GLOBAL>", interp: interp, forthic: "")

        // Set up date and time formatters
        self.date_formatter.dateFormat = "yyyy-MM-dd"
        self.date_formatter.timeZone = self.timezone

        self.time_formatter.dateFormat = "HH:mm"
        self.time_formatter.timeZone = self.timezone

        self.literal_handlers = [self.to_bool, self.to_int, self.to_float, self.to_date, self.to_time]

        // -------------------
        // Base Words
        self.add_module_word(word_name: "VARIABLES", word_handler: self.word_VARIABLES)
        self.add_module_word(word_name: "!", word_handler: self.word_bang)
        self.add_module_word(word_name: "@", word_handler: self.word_at)
        self.add_module_word(word_name: "!@", word_handler: self.word_bang_at)
        self.add_module_word(word_name: "INTERPRET", word_handler: self.word_INTERPRET)
        self.add_module_word(word_name: "MEMO", word_handler: self.word_MEMO)
        self.add_module_word(word_name: "EXPORT", word_handler: self.word_EXPORT)
        self.add_module_word(word_name: "USE-MODULES", word_handler: self.word_USE_MODULES)
        self.add_module_word(word_name: "REC", word_handler: self.word_REC)
        self.add_module_word(word_name: "REC@", word_handler: self.word_REC_at)
        self.add_module_word(word_name: "<REC!", word_handler: self.word_l_REC_bang)
        self.add_module_word(word_name: "SCREEN!", word_handler: self.word_SCREEN_bang)
        self.add_module_word(word_name: "SCREEN", word_handler: self.word_SCREEN)
        self.add_module_word(word_name: "LOAD-SCREEN", word_handler: self.word_LOAD_SCREEN)

        // ----------------
        // Array/Record words
        self.add_module_word(word_name: "APPEND", word_handler: self.word_APPEND)
        self.add_module_word(word_name: "REVERSE", word_handler: self.word_REVERSE)
        self.add_module_word(word_name: "UNIQUE", word_handler: self.word_UNIQUE)
        self.add_module_word(word_name: "<DEL", word_handler: self.word_l_DEL)
        self.add_module_word(word_name: "RELABEL", word_handler: self.word_RELABEL)
        self.add_module_word(word_name: "BY-FIELD", word_handler: self.word_BY_FIELD)
        self.add_module_word(word_name: "GROUP-BY-FIELD", word_handler: self.word_GROUP_BY_FIELD)
        self.add_module_word(word_name: "GROUP-BY", word_handler: self.word_GROUP_BY)
        self.add_module_word(word_name: "GROUP-BY-w/KEY", word_handler: self.word_GROUP_BY_w_KEY)
        self.add_module_word(word_name: "GROUPS-OF", word_handler: self.word_GROUPS_OF)
        self.add_module_word(word_name: "MAP", word_handler: self.word_MAP)
        self.add_module_word(word_name: "MAP-w/KEY", word_handler: self.word_MAP_w_KEY)
        self.add_module_word(word_name: "FOREACH", word_handler: self.word_FOREACH)
        self.add_module_word(word_name: "FOREACH-w/KEY", word_handler: self.word_FOREACH_w_KEY)
        self.add_module_word(word_name: "FOREACH>ERRORS", word_handler: self.word_FOREACH_to_ERRORS)
        self.add_module_word(word_name: "FOREACH-w/KEY>ERRORS", word_handler: self.word_FOREACH_w_KEY_to_ERRORS)
        self.add_module_word(word_name: "ZIP", word_handler: self.word_ZIP)
        self.add_module_word(word_name: "ZIP-WITH", word_handler: self.word_ZIP_WITH)
        self.add_module_word(word_name: "KEYS", word_handler: self.word_KEYS)
        self.add_module_word(word_name: "VALUES", word_handler: self.word_VALUES)
        self.add_module_word(word_name: "LENGTH", word_handler: self.word_LENGTH)
        self.add_module_word(word_name: "SLICE", word_handler: self.word_SLICE)
        self.add_module_word(word_name: "DIFFERENCE", word_handler: self.word_DIFFERENCE)
        self.add_module_word(word_name: "INTERSECTION", word_handler: self.word_INTERSECTION)
        self.add_module_word(word_name: "UNION", word_handler: self.word_UNION)
        self.add_module_word(word_name: "SELECT", word_handler: self.word_SELECT)
        self.add_module_word(word_name: "SELECT-w/KEY", word_handler: self.word_SELECT_w_KEY)
        self.add_module_word(word_name: "TAKE", word_handler: self.word_TAKE)
        self.add_module_word(word_name: "DROP", word_handler: self.word_DROP)
        self.add_module_word(word_name: "ROTATE", word_handler: self.word_ROTATE)
        self.add_module_word(word_name: "ROTATE-ELEMENT", word_handler: self.word_ROTATE_ELEMENT)
        self.add_module_word(word_name: "SHUFFLE", word_handler: self.word_SHUFFLE)
        self.add_module_word(word_name: "SORT", word_handler: self.word_SORT)
        self.add_module_word(word_name: "SORT-w/FORTHIC", word_handler: self.word_SORT_w_FORTHIC)
        self.add_module_word(word_name: "NTH", word_handler: self.word_NTH)
        self.add_module_word(word_name: "LAST", word_handler: self.word_LAST)
        self.add_module_word(word_name: "UNPACK", word_handler: self.word_UNPACK)
        self.add_module_word(word_name: "FLATTEN", word_handler: self.word_FLATTEN)
        self.add_module_word(word_name: "KEY-OF", word_handler: self.word_KEY_OF)
        self.add_module_word(word_name: "REDUCE", word_handler: self.word_REDUCE)

        // -------------------
        // Stack Words
        self.add_module_word(word_name: "POP", word_handler: self.word_POP)
        self.add_module_word(word_name: "DUP", word_handler: self.word_DUP)
        self.add_module_word(word_name: "SWAP", word_handler: self.word_SWAP)

        // -------------------
        // Date/time Words
        self.add_module_word(word_name: "AM", word_handler: self.word_AM)
        self.add_module_word(word_name: "PM", word_handler: self.word_PM)
        self.add_module_word(word_name: "NOW", word_handler: self.word_NOW)
        self.add_module_word(word_name: ">TIME", word_handler: self.word_to_TIME)
        self.add_module_word(word_name: "TIME>STR", word_handler: self.word_TIME_to_STR)
        self.add_module_word(word_name: ">DATE", word_handler: self.word_to_DATE)
        self.add_module_word(word_name: "TODAY", word_handler: self.word_TODAY)
        self.add_module_word(word_name: "MONDAY", word_handler: self.word_MONDAY)
        self.add_module_word(word_name: "TUESDAY", word_handler: self.word_TUESDAY)
        self.add_module_word(word_name: "WEDNESDAY", word_handler: self.word_WEDNESDAY)
        self.add_module_word(word_name: "THURSDAY", word_handler: self.word_THURSDAY)
        self.add_module_word(word_name: "FRIDAY", word_handler: self.word_FRIDAY)
        self.add_module_word(word_name: "SATURDAY", word_handler: self.word_SATURDAY)
        self.add_module_word(word_name: "SUNDAY", word_handler: self.word_SUNDAY)
        self.add_module_word(word_name: "NEXT", word_handler: self.word_NEXT)
        self.add_module_word(word_name: "ADD-DAYS", word_handler: self.word_ADD_DAYS)
        self.add_module_word(word_name: "SUBTRACT-DATES", word_handler: self.word_SUBTRACT_DATES)
        self.add_module_word(word_name: "SUBTRACT-TIMES", word_handler: self.word_SUBTRACT_TIMES)
        self.add_module_word(word_name: "DATE>STR", word_handler: self.word_DATE_to_STR)
        self.add_module_word(word_name: "DATE-TIME>DATETIME", word_handler: self.word_DATE_TIME_to_DATETIME)
        self.add_module_word(word_name: "DATETIME>TIMESTAMP", word_handler: self.word_DATETIME_to_TIMESTAMP)
        self.add_module_word(word_name: "TIMESTAMP>DATETIME", word_handler: self.word_TIMESTAMP_to_DATETIME)


        // -------------------
        // Math Words
        self.add_module_word(word_name: "+", word_handler: self.word_plus)
        self.add_module_word(word_name: "-", word_handler: self.word_minus)
        self.add_module_word(word_name: "*", word_handler: self.word_times)
        self.add_module_word(word_name: "/", word_handler: self.word_divide_by)
        self.add_module_word(word_name: "MOD", word_handler: self.word_MOD)
        self.add_module_word(word_name: "MAX", word_handler: self.word_MAX)
        self.add_module_word(word_name: "MIN", word_handler: self.word_MIN)
        self.add_module_word(word_name: "==", word_handler: self.word_equal_equal)
        self.add_module_word(word_name: "!=", word_handler: self.word_not_equal)
        self.add_module_word(word_name: ">", word_handler: self.word_greater_than)
        self.add_module_word(word_name: ">=", word_handler: self.word_greater_than_or_equal)
        self.add_module_word(word_name: "<", word_handler: self.word_less_than)
        self.add_module_word(word_name: "<=", word_handler: self.word_less_than_or_equal)
        self.add_module_word(word_name: ">INT", word_handler: self.word_to_INT)
        self.add_module_word(word_name: ">BOOL", word_handler: self.word_to_BOOL)
        self.add_module_word(word_name: ">DOUBLE", word_handler: self.word_to_DOUBLE)
        self.add_module_word(word_name: "OR", word_handler: self.word_OR)
        self.add_module_word(word_name: "AND", word_handler: self.word_AND)
        self.add_module_word(word_name: "NOT", word_handler: self.word_NOT)
        self.add_module_word(word_name: "IN", word_handler: self.word_IN)
        self.add_module_word(word_name: "ANY", word_handler: self.word_ANY)
        self.add_module_word(word_name: "ALL", word_handler: self.word_ALL)

        // ----------------
        // String words
        self.add_module_word(word_name: "CONCAT", word_handler: self.word_CONCAT)
        self.add_module_word(word_name: "SPLIT", word_handler: self.word_SPLIT)
        self.add_module_word(word_name: "JOIN", word_handler: self.word_JOIN)
        self.add_module_word(word_name: "/N", word_handler: self.word_slash_N)
        self.add_module_word(word_name: "/R", word_handler: self.word_slash_R)
        self.add_module_word(word_name: "/T", word_handler: self.word_slash_T)
        self.add_module_word(word_name: "LOWER", word_handler: self.word_LOWER)
        self.add_module_word(word_name: "STRIP", word_handler: self.word_STRIP)
        self.add_module_word(word_name: "REPLACE", word_handler: self.word_REPLACE)
        self.add_module_word(word_name: "RE-MATCH", word_handler: self.word_RE_MATCH)
        self.add_module_word(word_name: "RE-MATCH-ALL", word_handler: self.word_RE_MATCH_ALL)
        self.add_module_word(word_name: ">STR", word_handler: self.word_to_STR)
        self.add_module_word(word_name: "URL-ENCODE", word_handler: self.word_URL_ENCODE)
        self.add_module_word(word_name: "URL-DECODE", word_handler: self.word_URL_DECODE)

        // ----------------
        // Misc words
        self.add_module_word(word_name: "NULL", word_handler: self.word_NULL)
        self.add_module_word(word_name: "QUOTE-CHAR", word_handler: self.word_QUOTE_CHAR)
        self.add_module_word(word_name: "QUOTED", word_handler: self.word_QUOTED)
        self.add_module_word(word_name: "DEFAULT", word_handler: self.word_DEFAULT)
        self.add_module_word(word_name: "*DEFAULT", word_handler: self.word_star_DEFAULT)
        self.add_module_word(word_name: "<REPEAT", word_handler: self.word_l_REPEAT)
        self.add_module_word(word_name: "IDENTITY", word_handler: self.word_IDENTITY)
        self.add_module_word(word_name: ">FIXED", word_handler: self.word_to_FIXED)
        self.add_module_word(word_name: ">JSON", word_handler: self.word_to_JSON)
        self.add_module_word(word_name: "JSON>", word_handler: self.word_JSON_to)
        self.add_module_word(word_name: ".s", word_handler: self.word_dot_s)

    }

    override func find_word(name: String) -> Word? {
        var result = super.find_word(name: name)
        if (result == nil) {
            result = self.find_literal_word(name)
        }
        return result
    }

    // ----- Literal Handlers -----------------------------------------------------------------------------------------
    private func to_bool(str_val: String) -> Bool? {
        var result: Bool?
        switch str_val {
        case "TRUE":
            result = true
        case "FALSE":
            result = false
        default:
            result = nil
        }
        return result
    }

    private func to_int(str_val: String) -> Int? {
        return Int(str_val)
    }

    private func to_float(str_val: String) -> Float? {
        return Float(str_val)
    }

    private func to_date(str_val: String) -> Date? {
        return self.date_formatter.date(from: str_val)
    }

    private func to_time(str_val: String) -> Date? {
        return self.time_formatter.date(from: str_val)
    }

    // ----- Word Handlers --------------------------------------------------------------------------------------------

    // ( varnames -- )
    // Creates new variables in the current module
    func word_VARIABLES(interp: Interpreter) throws {
        let varnames = try interp.stack_pop()
        if (!(varnames is Array<String>)) {
            throw GlobalModuleError.STRING_LIST_EXPECTED(value: varnames)
        }
        let module = interp.cur_module()
        for v in (varnames as! Array<String>) {
            module.add_variable(name: v)
        }
    }

    //( value variable -- )
    // Sets value of a variable
    func word_bang(interp: Interpreter) throws {
        let _variable = try interp.stack_pop()
        let value = try interp.stack_pop()

        if (!(_variable is Variable)) {
            throw GlobalModuleError.VARIABLE_EXPECTED(value: _variable)
        }
        let variable = _variable as! Variable
        variable.set_value(value: value)
    }

    //( variable -- value )
    // Gets value of a variable
    func word_at(interp: Interpreter) throws {
        let _variable = try interp.stack_pop()
        if (!(_variable is Variable)) {
            throw GlobalModuleError.VARIABLE_EXPECTED(value: _variable)
        }
        let variable = _variable as! Variable
        interp.stack_push(variable.get_value())
    }

    //( value variable -- value )
    // Set the value of a variable and then pushes variable's value onto the stack
    func word_bang_at(interp: Interpreter) throws {
        let _variable = try interp.stack_pop()
        let value = try interp.stack_pop()

        if (!(_variable is Variable)) {
            throw GlobalModuleError.VARIABLE_EXPECTED(value: _variable)
        }
        let variable = _variable as! Variable
        variable.set_value(value: value)
        interp.stack_push(value)
    }

    //( forthic -- ? )
    // Interprets a Forthic string
    func word_INTERPRET(interp: Interpreter) throws {
        let forthic = try pop_string(interp: interp)
        if (forthic == "") {
            return
        }
        try interp.run(forthic: forthic)
    }

    // ( name forthic -- )
    func word_MEMO(interp: Interpreter) throws {
        let forthic = try pop_string(interp: interp)
        let name = try pop_string(interp: interp)

        let var_name = "<memo_var_\(name)>"

        // Create variable
        try interp.run(forthic: "['\(var_name)'] VARIABLES")

        // Define name! word
        let name_bang = "\(name)!"
        try interp.run(forthic: ": \(name_bang)   \(forthic) \(var_name) !;")

        // Define name!@ word
        let name_bang_at = "\(name)!@"
        try interp.run(forthic: ": \(name_bang_at)   \(name_bang) \(var_name) @;")

        // Create name word
        let word = MemoWord(name: name, varname: var_name)
        interp.cur_module().add_word(word: word)
    }

    // (names -- )
    func word_EXPORT(interp: Interpreter) throws {
        let _names = try interp.stack_pop()
        if (!(_names is [String])) {
            throw GlobalModuleError.STRING_LIST_EXPECTED(value: _names)
        }
        let names = _names as! [String]
        interp.cur_module().add_exportable(names: names)
    }


    // ( names -- )
    func word_USE_MODULES(interp: Interpreter) throws {
        let names = try interp.stack_pop() as! [Any]

        // Check that we're in the app module
        if (interp.cur_module().name != "") {
            throw GlobalModuleError.USE_MODULES_ONLY_FROM_APP_MODULE(module: interp.cur_module())
        }

        for name in names {
            let module_name: String
            let prefix: String

            if (name is Array<String>) {
                let pieces: [String] = name as! Array<String>
                module_name = pieces[0]
                prefix = pieces[1]
            }
            else {
                module_name = name as! String
                prefix = name as! String
            }
            let module = try interp.find_module(name: module_name)
            try interp.app_module().import_module(module_name: prefix, module: module, interp: interp)
        }
    }

    // ( key_vals -- rec )
    func word_REC(interp: Interpreter) throws {
        let _key_vals = try interp.stack_pop()

        var key_vals: Array<Array<Any>> = []

        if (_key_vals == nil) {
            key_vals = []
        }
        else if (!(_key_vals is Array<Array<Any>>)) {
            throw GlobalModuleError.KEY_VALS_EXPECTED(value: _key_vals)
        }
        else {
            key_vals = _key_vals as! Array<Array<Any>>
        }

        var result: Record = [:]
        for kv in key_vals {
            if (kv.count != 2) {
                throw GlobalModuleError.PAIR_EXPECTED(value: kv)
            }
            if (!(kv[0] is String)) {
                throw GlobalModuleError.STRING_EXPECTED(value: kv[0])
            }
            let key: String = kv[0] as! String
            result[key] = kv[1]
        }
        interp.stack_push(result)
    }

    // ( rec field -- value )
    // ( rec fields -- value )
    func word_REC_at(interp: Interpreter) throws {
        let _field = try interp.stack_pop()
        let _rec = try interp.stack_pop()

        var rec: Record = [:]

        // If rec is nil, return nil
        if (_rec == nil) {
            interp.stack_push(nil)
            return
        }
        else if (_rec is Record) {
            rec = _rec as! Record
        }
        else {
            throw GlobalModuleError.RECORD_EXPECTED(value: _rec)
        }

        // Condition requested fields
        var fields: [String] = []
        if (_field is String) {
            fields = [_field as! String]
        }
        else if (_field is [String]) {
            fields = _field as! [String]
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: _field)
        }

        let result: Any? = try self.drill_for_value(rec: rec, fields: fields)
        interp.stack_push(result)
    }


    // ( rec value field -- rec )
    // ( rec value fields -- rec )
    func word_l_REC_bang(interp: Interpreter) throws {
        let _field = try interp.stack_pop()
        let value = try interp.stack_pop()
        let _rec = try interp.stack_pop()

        // Condition rec
        var rec: Record
        if (_rec == nil) {
            rec = [:]
        }
        else if (_rec is Record) {
            rec = _rec as! Record
        }
        else {
            throw GlobalModuleError.RECORD_EXPECTED(value: _rec)
        }

        // Condition field
        var fields: [String]
        if (_field is [String]) {
            fields = _field as! [String]
        }
        else if (_field is String) {
            fields = [_field as! String]
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: _field)
        }

        // Set value
        func drill_set(rec: Record, fields: Array<String>, value: Any?) throws -> Record {
            var result: Record = rec
            if (fields.count == 1) {
                result[fields[0]] = value
                return result
            }

            result[fields[0]] = try make_record(rec: result[fields[0]], fields: Array<String>(fields.dropFirst()), value: value)
            return result
        }

        func make_record(rec: Any?, fields: Array<String>, value: Any?) throws -> Record {
            var result: Record
            if (rec == nil) {
                result = Record()
            }
            else if (rec is Record) {
                result = rec as! Record
            }
            else {
                throw GlobalModuleError.RECORD_EXPECTED(value: rec)
            }

            if (fields.count == 0) {
                return result
            }
            if (fields.count == 1) {
                result[fields[0]] = value
                return result
            }
            result[fields[0]] = try make_record(rec: result[fields[0]], fields: Array<String>(fields.dropFirst()), value: value)
            return result
        }

        rec = try drill_set(rec: rec, fields: fields, value: value)
        interp.stack_push(rec)
    }


    // ( content name -- )
    func word_SCREEN_bang(interp: Interpreter) throws {
        let name = try pop_string(interp: interp)
        let content = try pop_string(interp: interp)
        interp.app_module().set_screen(name: name, forthic: content)
    }

    // ( name -- content )
    // Returns screen stored in application module
    func word_SCREEN(interp: Interpreter) throws {
        let name = try pop_string(interp: interp)
        let result = try interp.app_module().get_screen(name: name)
        interp.stack_push(result)
    }

    // ( name -- ? )
    // Gets contents of screen and runs it
    func word_LOAD_SCREEN(interp: Interpreter) throws {
        let name = try pop_string(interp: interp)

        if (self.active_screens.contains(name)) {
            throw GlobalModuleError.LOAD_SCREEN_CYCLE(value: name)
        }

        let screen = try interp.app_module().get_screen(name: name)

        self.active_screens.insert(name)
        try interp.run_in_module(module: interp.app_module(), forthic: screen)
        self.active_screens.remove(name)

    }

    // ( array item -- array )
    // ( record key/val -- record )
    func word_APPEND(interp: Interpreter) throws {
        let item = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push([item])
            return
        }

        if (container is [Any]) {
            var result: List = container as! List
            result.append(item)
            interp.stack_push(result)
            return
        }
        else if (container is Record) {
            var result: Record = container as! Record
            if (!(item is [Any])) {
                throw GlobalModuleError.PAIR_EXPECTED(value: item)
            }
            let pair: [Any] = item as! [Any]
            result[pair[0] as! String] = pair[1]
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array -- array )
    // ( record -- record )
    func word_REVERSE(interp: Interpreter) throws {
        let container = try interp.stack_pop()
        if (container == nil) {
            interp.stack_push(container)
            return
        }

        if (container is List) {
            var result = container as! List
            result.reverse()
            interp.stack_push(result)
        }
        else if (container is Record) {
            var result = container as! Record
            result.reverse()
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array -- array )
    // ( record -- record )
    func word_UNIQUE(interp: Interpreter) throws {
        let container = try interp.stack_pop()
        if (container == nil) {
            interp.stack_push(container)
            return
        }

        func to_hashable_container(rec: Record) -> Record {
            var result = Record()
            for key in rec.keys {
                result[key] = rec[key] as! AnyHashable?
            }
            return result
        }

        if (container is List) {
            let array = container as! [AnyHashable?]
            let result = Array(Set(array))
            interp.stack_push(result)
        }
        else if (container is Record) {
            // Invert container to unique the values
            let hashable_container = to_hashable_container(rec: container as! Record)
            var inverse_container = Record()

            for key in hashable_container.keys {
                let value = hashable_container[key] as! AnyHashable
                inverse_container[value] = key
            }

            // Invert the inverted container to get the result
            var result = Record()
            for key in inverse_container.keys {
                let value = inverse_container[key] as! AnyHashable
                result[value] = key
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array index -- array )
    // ( record key -- record )
    func word_l_DEL(interp: Interpreter) throws {
        let _key = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(container)
            return
        }

        if (container is List) {
            var result = container as! List
            if (!(_key is Int)) {
                throw GlobalModuleError.NUMBER_EXPECTED(value: _key)
            }
            result.remove(at: _key as! Int)
            interp.stack_push(result)
            return
        }
        else if (container is Record){
            var result = container as! Record
            if (!(_key is String)) {
                throw GlobalModuleError.STRING_LIST_EXPECTED(value: _key)
            }
            result.removeValue(forKey: _key as! String)
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array old_keys new_keys -- array )
    // ( record old_keys new_keys -- record )
    func word_RELABEL(interp: Interpreter) throws {
        let _new_keys = try interp.stack_pop()
        let _old_keys = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(container)
            return
        }

        if (!(_old_keys is List)) {
            throw GlobalModuleError.LIST_EXPECTED(value: _old_keys)
        }
        if (!(_new_keys is List)) {
            throw GlobalModuleError.LIST_EXPECTED(value: _new_keys)
        }
        let old_keys = _old_keys as! List
        let new_keys = _new_keys as! List
        if (old_keys.count != new_keys.count) {
            throw GlobalModuleError.SAME_LENGTH_ARRAYS_EXPECTED(array1: old_keys, array2: new_keys)
        }

        // Create mapping of new to old
        var new_to_old = Dictionary<AnyHashable, Any>()
        for (index, old_key) in old_keys.enumerated() {
            new_to_old[new_keys[index] as! AnyHashable] = old_key
        }

        if (container is List) {
            let array = container as! List
            var result : List = []
            let keys = Array(new_to_old.keys) as! [Int]
            for new_key in keys.sorted() {
                let old_key = new_to_old[new_key] as! Int
                result.append(array[old_key])
            }
            interp.stack_push(result)
            return
        }
        else if (container is Record) {
            let record = container as! Record
            var result = Record()
            for new_key in new_to_old.keys {
                let old_key = new_to_old[new_key] as! String
                result[new_key as! String] = record[old_key]
            }
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array field -- field_to_item )
    // ( record field -- field_to_item )
    func word_BY_FIELD(interp: Interpreter) throws {
        let field = try pop_string(interp: interp)
        let container = try interp.stack_pop()

        var values: List

        if (container == nil) {
            interp.stack_push(Record())
            return
        }
        else if (container is List) {
            values = container as! List
        }
        else if (container is Record) {
            values = Array((container as! Record).values)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        var result = Record()
        for v in values {
            if (v is Record) {
                let rec = v as! Record
                let key = rec[field] as! AnyHashable
                result[key] = v
            }
        }
        interp.stack_push(result)
    }

    // ( array field -- field_to_items )
    // ( record field -- field_to_items )
    func word_GROUP_BY_FIELD(interp: Interpreter) throws {
        let field = try pop_string(interp: interp)
        let container = try interp.stack_pop()

        let values = try get_container_values(container)

        var result = Record()
        for v in values {
            let rec = v as! Record
            let field_key = rec[field] as! AnyHashable
            add_to_group(result: &result, group: field_key, value: v)
        }

        interp.stack_push(result)
    }

    // ( array forthic -- group_to_items )
    // ( record forthic -- group_to_items )
    func word_GROUP_BY(interp: Interpreter) throws {
        let forthic = try pop_string(interp: interp)
        let container = try interp.stack_pop()

        let values = try get_container_values(container)

        var result = Record()
        for v in values {
            interp.stack_push(v)
            try interp.run(forthic: forthic)
            let group = try interp.stack_pop() as! AnyHashable
            add_to_group(result: &result, group: group, value: v)
        }
        interp.stack_push(result)
    }

    // ( array forthic -- group_to_items )
    // ( record forthic -- group_to_items )
    func word_GROUP_BY_w_KEY(interp: Interpreter) throws {
        let forthic = try pop_string(interp: interp)
        let container = try interp.stack_pop()

        let keys = try get_container_keys(container)
        let values = try get_container_values(container)
        var result = Record()
        for (index, _) in keys.enumerated() {
            let k = keys[index]
            let v = values[index]
            interp.stack_push(k)
            interp.stack_push(v)
            try interp.run(forthic: forthic)
            let group = try interp.stack_pop() as! AnyHashable
            add_to_group(result: &result, group: group, value: v)
        }
        interp.stack_push(result)
    }

    // ( array n -- arrays )
    // ( record n -- records )
    func word_GROUPS_OF(interp: Interpreter) throws {
        let _size = try pop_int(interp: interp)
        let container = try interp.stack_pop()

        if (_size == nil) {
            interp.stack_push(container)
            return
        }
        let size = _size!

        if (size <= 0) {
            throw GlobalModuleError.POSITIVE_INTEGER_EXPECTED(value: size)
        }

        func group_items(items: List, group_size: Int) -> List {
            let num_groups = Int(ceil(Double(items.count) / Double(group_size)))
            var res: List = []
            var remaining = items
            for _ in 1...num_groups {
                res.append(List(remaining.prefix(group_size)))
                remaining = List(remaining.dropFirst(group_size))
            }
            return res
        }

        func extract_rec(record: Record, keys: [AnyHashable]) -> Record {
            var res: Record = [:]
            for k in keys {
                res[k] = record[k]
            }
            return res
        }

        var result : List = []
        if (container == nil) {
            interp.stack_push([])
            return
        }
        else if (container is List) {
            result = group_items(items: (container as! List), group_size: size)
            interp.stack_push(result)
            return
        }
        else if (container is Record) {
            let record = container as! Record
            let keys = Array(record.keys)
            let key_groups = group_items(items: keys, group_size: size)
            result = []
            for ks in key_groups {
                result.append(extract_rec(record: record, keys: ks as! [AnyHashable]))
            }
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array forthic -- array )
    // ( record forthic -- record )
    func word_MAP(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            var result = List()
            let list = container as! List
            for item in list {
                interp.stack_push(item)
                try interp.run(forthic: forthic)
                result.append(try interp.stack_pop())
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result = Record()
            for (k, v) in record.elements {
                interp.stack_push(v)
                try interp.run(forthic: forthic)
                result[k] = try interp.stack_pop()
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array forthic -- array )
    // ( record forthic -- record )
    func word_MAP_w_KEY(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            var result = List()
            let list = container as! List
            for (index, item) in list.enumerated() {
                interp.stack_push(index)
                interp.stack_push(item)
                try interp.run(forthic: forthic)
                result.append(try interp.stack_pop())
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result = Record()
            for (k, v) in record.elements {
                interp.stack_push(k)
                interp.stack_push(v)
                try interp.run(forthic: forthic)
                result[k] = try interp.stack_pop()
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( items forthic -- ? )
    // ( record forthic -- ? )
    func word_FOREACH(interp: Interpreter) throws {
        _ = try foreach(interp: interp)
    }

    // ( items forthic -- ? )
    // ( record forthic -- ? )
    func word_FOREACH_w_KEY(interp: Interpreter) throws {
        _ = try foreach_w_key(interp: interp)
    }

    // ( items forthic -- ? errors )
    // ( record forthic -- ? errors )
    func word_FOREACH_to_ERRORS(interp: Interpreter) throws {
        let errors = try foreach(interp: interp, return_errors: true)
        interp.stack_push(errors)
    }

    // ( items forthic -- ? errors )
    // ( record forthic -- ? errors )
    func word_FOREACH_w_KEY_to_ERRORS(interp: Interpreter) throws {
        let errors = try foreach_w_key(interp: interp)
        interp.stack_push(errors)
    }

    // ( array1 array2 -- array )
    // ( record1 record2 -- record )
    func word_ZIP(interp: Interpreter) throws {
        let container2 = try interp.stack_pop()
        let container1 = try interp.stack_pop()

        if (container1 == nil || container2 == nil) {
            interp.stack_push(nil)
            return
        }
        else if (container1 is List && container2 is List) {
            var result = List()
            let list1 = container1 as! List
            let list2 = container2 as! List

            for (index, value) in list1.enumerated() {
                if (index < list2.count) {
                    result.append([value, list2[index]])
                }
                else {
                    break
                }
            }
            interp.stack_push(result)
            return
        }
        else if (container1 is Record && container2 is Record) {
            var result = Record()
            let record1 = container1 as! Record
            let record2 = container2 as! Record
            for (k, v) in record1.elements {
                var pair = List()
                pair.append(v)
                pair.append(record2[k])
                result[k] = pair
            }
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.SAME_LENGTH_ARRAYS_EXPECTED(array1: container1, array2: container2)
        }
    }

    // ( array1 array2 forthic -- array )
    // ( record1 record2 forthic -- record )
    func word_ZIP_WITH(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container2 = try interp.stack_pop()
        let container1 = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String

        if (container1 == nil || container2 == nil) {
            interp.stack_push(nil)
            return
        }
        else if (container1 is List && container2 is List) {
            var result = List()
            let list1 = container1 as! List
            let list2 = container2 as! List

            for (index, value) in list1.enumerated() {
                if (index < list2.count) {
                    interp.stack_push(value)
                    interp.stack_push(list2[index])
                    try interp.run(forthic: forthic)
                    result.append(try interp.stack_pop())
                }
                else {
                    break
                }
            }
            interp.stack_push(result)
            return
        }
        else if (container1 is Record && container2 is Record) {
            var result = Record()
            let record1 = container1 as! Record
            let record2 = container2 as! Record
            for (k, v) in record1.elements {
                interp.stack_push(v)
                interp.stack_push(record2[k])
                try interp.run(forthic: forthic)
                result[k] = try interp.stack_pop()
            }
            interp.stack_push(result)
            return
        }
        else {
            throw GlobalModuleError.SAME_LENGTH_ARRAYS_EXPECTED(array1: container1, array2: container2)
        }
    }

    // ( array -- array )
    // ( record -- array )
    func word_KEYS(interp: Interpreter) throws {
        let container = try interp.stack_pop()
        interp.stack_push(try get_container_keys(container))
    }

    // ( array -- array )
    // ( record -- array )
    func word_VALUES(interp: Interpreter) throws {
        let container = try interp.stack_pop()
        interp.stack_push(try get_container_values(container))
    }

    // ( array -- length )
    // ( record -- length )
    func word_LENGTH(interp: Interpreter) throws {
        let container = try interp.stack_pop()
        var result: Int
        if (container == nil) {
            result = 0
        }
        else if (container is List) {
            let list = container as! List
            result = list.count
        }
        else if (container is Record) {
            let record = container as! Record
            result = record.count
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        interp.stack_push(result)
    }

    // ( array start end -- array )
    // ( record start end -- record )
    func word_SLICE(interp: Interpreter) throws {
        let _end = try interp.stack_pop()
        let _start = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_start is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _start)
        }
        let start = _start as! Int

        if (!(_end is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _end)
        }
        let end = _end as! Int


        func normalize_index(index: Int, length: Int) -> Int {
            var res = index
            if index < 0 {
                res = index + length
            }
            return res
        }

        func generate_indexes(start: Int, end: Int, length: Int) -> [Int?] {
            let norm_start = normalize_index(index: start, length: length)
            let norm_end = normalize_index(index: end, length: length)

            var step = 1
            if (norm_start > norm_end) {
                step = -1
            }
            var res: [Int?] = [norm_start]
            if (norm_start < 0 || norm_start >= length) {
                res = []
            }

            var cur = norm_start
            while (cur != norm_end) {
                cur = cur + step
                if (cur < 0 || cur >= length) {
                    res.append(nil)
                }
                else {
                    res.append(cur)
                }
            }
            return res
        }

        func extract_list_values(list: List, indexes: [Int?]) throws -> [Any?] {
            var res: [Any?] = []
            let list = container as! List
            for i in indexes {
                if (i == nil) {
                    res.append(nil)
                }
                else {
                    res.append(list[i!])
                }
            }
            return res
        }

        func extract_record_values(record: Record, indexes: [Int?]) throws -> Record {
            var res: Record = [:]
            let keys = Array(record.keys)
            for i in indexes {
                if (i != nil) {
                    let key = keys[i!]
                    res[key] = record[key]
                }
            }
            return res
        }

        // Handle each case
        if (container == nil) {
            let list: List = []
            let length = list.count
            let indexes = generate_indexes(start: start, end: end, length: length)
            interp.stack_push(try extract_list_values(list: list, indexes: indexes))
        }
        else if (container is List) {
            let list = container as! List
            let length = list.count
            let indexes = generate_indexes(start: start, end: end, length: length)
            interp.stack_push(try extract_list_values(list: list, indexes: indexes))
        }
        else if (container is Record) {
            let record = container as! Record
            let length = record.count
            let indexes = generate_indexes(start: start, end: end, length: length)
            interp.stack_push(try extract_record_values(record: record, indexes: indexes))
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    func word_DIFFERENCE(interp: Interpreter) throws {
        let rcontainer = try interp.stack_pop()
        let lcontainer = try interp.stack_pop()

        if (lcontainer ==  nil) {
            interp.stack_push(nil)
            return
        }

        if (rcontainer ==  nil) {
            interp.stack_push(lcontainer)
            return
        }

        if (lcontainer is List && rcontainer is List) {
            let llist = lcontainer as! List
            let rset = to_set(rcontainer as! List)
            var result: List = []
            for item in llist {
                if (!rset.contains(item as! AnyHashable?)) {
                    result.append(item)
                }
            }
            interp.stack_push(result)
        }
        else if (lcontainer is Record && rcontainer is Record) {
            let lrecord = lcontainer as! Record
            let rrecord = rcontainer as! Record
            let rkeys = Set(rrecord.keys)
            var result: Record = [:]
            for (k, v) in lrecord.elements {
                if (!rkeys.contains(k)) {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.MATCHING_CONTAINERS_EXPECTED(container1: lcontainer, container2: rcontainer)
        }
    }

    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    func word_INTERSECTION(interp: Interpreter) throws {
        let rcontainer = try interp.stack_pop()
        let lcontainer = try interp.stack_pop()

        if (lcontainer ==  nil) {
            interp.stack_push(nil)
            return
        }

        if (rcontainer ==  nil) {
            interp.stack_push(lcontainer)
            return
        }

        if (lcontainer is List && rcontainer is List) {
            let llist = lcontainer as! List
            let rset = to_set(rcontainer as! List)
            var result: List = []
            for item in llist {
                if (rset.contains(item as! AnyHashable?)) {
                    result.append(item)
                }
            }
            interp.stack_push(result)
        }
        else if (lcontainer is Record && rcontainer is Record) {
            let lrecord = lcontainer as! Record
            let rrecord = rcontainer as! Record
            let rkeys = Set(rrecord.keys)
            var result: Record = [:]
            for (k, v) in lrecord.elements {
                if (rkeys.contains(k)) {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.MATCHING_CONTAINERS_EXPECTED(container1: lcontainer, container2: rcontainer)
        }
    }

    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    func word_UNION(interp: Interpreter) throws {
        let rcontainer = try interp.stack_pop()
        let lcontainer = try interp.stack_pop()

        if (lcontainer ==  nil) {
            interp.stack_push(nil)
            return
        }

        if (rcontainer ==  nil) {
            interp.stack_push(lcontainer)
            return
        }

        if (lcontainer is List && rcontainer is List) {
            let llist = lcontainer as! List
            let rlist = rcontainer as! List
            var result = OrderedSet<AnyHashable?>()
            for item in llist {
                result.append(item as! AnyHashable?)
            }
            for item in rlist {
                let hashable = item as! AnyHashable?
                if (!result.contains(hashable)) {
                    result.append(hashable)
                }
            }
            interp.stack_push(Array(result))
        }
        else if (lcontainer is Record && rcontainer is Record) {
            let lrecord = lcontainer as! Record
            let rrecord = rcontainer as! Record
            var result: Record = [:]
            for (k, v) in lrecord.elements {
                result[k] = v
            }
            for (k, v) in rrecord.elements {
                if (result[k] == nil) {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.MATCHING_CONTAINERS_EXPECTED(container1: lcontainer, container2: rcontainer)
        }
    }

    // ( larray forthic -- array )
    // ( lrecord forthic -- record )
    func word_SELECT(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            let list = container as! List
            var result: List = []
            for item in list {
                interp.stack_push(item)
                try interp.run(forthic: forthic)
                let should_select = try interp.stack_pop() as! Bool
                if (should_select) {
                    result.append(item)
                }
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result: Record = [:]
            for (k, v) in record.elements {
                interp.stack_push(v)
                try interp.run(forthic: forthic)
                let should_select = try interp.stack_pop() as! Bool
                if (should_select) {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( larray forthic -- array )
    // ( lrecord forthic -- record )
    func word_SELECT_w_KEY(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            let list = container as! List
            var result: List = []
            for (index, item) in list.enumerated() {
                interp.stack_push(index)
                interp.stack_push(item)
                try interp.run(forthic: forthic)
                let should_select = try interp.stack_pop() as! Bool
                if (should_select) {
                    result.append(item)
                }
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result: Record = [:]
            for (k, v) in record.elements {
                interp.stack_push(k)
                interp.stack_push(v)
                try interp.run(forthic: forthic)
                let should_select = try interp.stack_pop() as! Bool
                if (should_select) {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }


    // ( array n -- rest array )
    // ( record n -- rest record )
    func word_TAKE(interp: Interpreter) throws {
        let _n = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_n is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _n)
        }
        let n = _n as! Int

        if (container == nil) {
            interp.stack_push(nil)
            return
        }
        else if (container is List) {
            let list = container as! List
            var rest = List()
            var result = List()
            if (n <= 0) {
                rest = []
                result = []
            }
            else {
                rest = Array(list.dropFirst(n))
                result = Array(list.prefix(n))
            }
            interp.stack_push(rest)
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result = Record()
            var rest = Record()
            if (n <= 0) {
                rest = [:]
                result = [:]
            }
            else {
                var num_appended = 0
                for (k, v) in record.elements {
                    if (num_appended < n) {
                        result[k] = v
                        num_appended += 1
                    }
                    else {
                        rest[k] = v
                    }
                }
            }
            interp.stack_push(rest)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array n -- rest array )
    // ( record n -- rest record )
    func word_DROP(interp: Interpreter) throws {
        let _n = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_n is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _n)
        }
        let n = _n as! Int

        if (container == nil) {
            interp.stack_push(nil)
            return
        }
        else if (container is List) {
            let list = container as! List
            var result = List()
            if (n <= 0) {
                result = list
            }
            else {
                result = Array(list.dropFirst(n))
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result = Record()
            if (n <= 0) {
                result = record
            }
            else {
                var num_dropped = 0
                for (k, v) in record.elements {
                    if (num_dropped >= n) {
                        result[k] = v
                    }
                    else {
                        num_dropped += 1
                    }
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array -- array )
    // ( record -- record )
    func word_ROTATE(interp: Interpreter) throws {
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(container)
        }
        else if (container is List) {
            var list = container as! List
            if (list.count > 0) {
                let item : Any? = list.popLast()!
                list.insert(item, at: 0)
            }
            interp.stack_push(list)
        }
        else if (container is Record) {
            var result: Record = [:]
            var record = container as! Record
            if (record.count == 0) {
                result = record
            }
            else {
                let item = record.removeLast()
                result[item.key] = item.value
                for (k, v) in record.elements {
                    result[k] = v
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }


    // ( array element -- array )
    // ( record element -- record )
    func word_ROTATE_ELEMENT(interp: Interpreter) throws {
        let element = try interp.stack_pop()
        let container = try interp.stack_pop()

        func is_match(_ _value: Any?) throws -> Bool {
            let value = _value as! AnyHashable
            return element as! AnyHashable == value
        }

        func find_in_record(_value: Any?, record: Record) -> AnyHashable? {
            for (k, v) in record.elements {
                if (v as! AnyHashable == element as! AnyHashable) {
                    return k
                }
            }
            return nil
        }

        if (container == nil) {
            interp.stack_push(container)
        }
        else if (container is List) {
            var list = container as! List
            if (list.count > 0) {
                let index = try list.firstIndex(where: is_match)
                if (index != nil) {
                    let item : Any? = list.remove(at:index!)
                    list.insert(item, at: 0)
                }
            }
            interp.stack_push(list)
        }
        else if (container is Record) {
            var result: Record = [:]
            let record = container as! Record
            if (record.count == 0) {
                result = record
            }
            else {
                let _key = find_in_record(_value: element, record: record)
                if (_key == nil) {
                    result = record
                }
                else {
                    let key = _key!
                    let value = record[key]
                    result[key] = value
                    for (k, v) in record.elements {
                        if (k != key) {
                            result[k] = v
                        }
                    }
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }


    // ( array -- array )
    // ( record -- record )
    func word_SHUFFLE(interp: Interpreter) throws {
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(container)
        }
        else if (container is List) {
            var list = container as! List
            list.shuffle()
            interp.stack_push(list)
        }
        else if (container is Record) {
            var record = container as! Record
            record.shuffle()
            interp.stack_push(record)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }


    // ( array -- array )
    // ( record -- record )
    func word_SORT(interp: Interpreter) throws {
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            let list = container as! List
            interp.stack_push(try list.sorted(by:compare_any))
        }
        else if (container is Record) {
            let record = container as! Record
            let sorted_elements = try record.sorted {
                (l, r) throws -> Bool in
                return try compare_any(l: l.value, r: r.value)
            }
            let result = Record(uniqueKeysWithValues: sorted_elements)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array forthic -- array )
    // ( record forthic -- record )
    func word_SORT_w_FORTHIC(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }

        let forthic = _forthic as! String

        func compare_w_forthic(l: Any?, r: Any?, forthic: String) throws -> Bool {
            interp.stack_push(l)
            try interp.run(forthic: forthic)
            let l_val = try interp.stack_pop()

            interp.stack_push(r)
            try interp.run(forthic: forthic)
            let r_val = try interp.stack_pop()
            return try compare_any(l: l_val, r: r_val)
        }

        if (container == nil) {
            interp.stack_push(container)
            return
        }
        else if (container is List) {
            let list = container as! List
            let sorted_elements = try list.sorted {
                (l, r) throws -> Bool in
                return try compare_w_forthic(l: l, r: r, forthic: forthic)
            }
            interp.stack_push(sorted_elements)
        }
        else if (container is Record) {
            let record = container as! Record
            let sorted_elements = try record.sorted {
                (l, r) throws -> Bool in
                return try compare_w_forthic(l: l.value, r: r.value, forthic: forthic)
            }
            let result = Record(uniqueKeysWithValues: sorted_elements)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array n -- item )
    // ( record n -- value )
    func word_NTH(interp: Interpreter) throws {
        let _n = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_n is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _n)
        }
        let n = _n as! Int

        if (n < 0) {
            interp.stack_push(nil)
            return
        }

        if (container == nil) {
            interp.stack_push(nil)
        }
        else if (container is List) {
            let list = container as! List
            var result: Any?
            if (n >= list.count) {
                result = nil
            }
            else {
                result = list[n]
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            let keys = Array(record.keys)
            var result: Any?
            if (n >= keys.count) {
                result = nil
            }
            else {
                result = record[keys[n]]
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array -- item )
    // ( record -- value )
    func word_LAST(interp: Interpreter) throws {
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(nil)
        }
        else if (container is List) {
            let list = container as! List
            let last = list.last
            if (last == nil) {
                interp.stack_push(nil)
            }
            else {
                interp.stack_push(list.last!)
            }
        }
        else if (container is Record) {
            let record = container as! Record
            let keys = Array(record.keys)
            let last_key = keys.last
            if (last_key == nil) {
                interp.stack_push(nil)
            }
            else {
                interp.stack_push(record[keys.last!])
            }
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( array -- a1 a2 .. an )
    // ( record -- val1 val2 .. valn )
    func word_UNPACK(interp: Interpreter) throws {
        let container = try interp.stack_pop()

        if (container == nil) {
            interp.stack_push(nil)
        }
        else if (container is List) {
            let list = container as! List
            for item in list {
                interp.stack_push(item)
            }
        }
        else if (container is Record) {
            let record = container as! Record
            for val in record.values {
                interp.stack_push(val)
            }
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( nested_arrays -- array )
    func word_FLATTEN(interp: Interpreter) throws {
        let nested = try interp.stack_pop()

        func flatten_array(items: List, accum: inout List) {
            for i in items {
                if (i is List) {
                    flatten_array(items: i as! List, accum: &accum)
                }
                else {
                    accum.append(i)
                }
            }
        }

        if (nested == nil) {
            interp.stack_push(nil)
        }
        else if (nested is List) {
            var result: List = []
            flatten_array(items: nested as! List, accum: &result)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: nested)
        }
    }

    // ( array val -- index )
    // ( record val -- key )
    func word_KEY_OF(interp: Interpreter) throws {
        let _val = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_val is AnyHashable)) {
            throw GlobalModuleError.HASHABLE_EXPECTED(item: _val)
        }
        let val = _val as! AnyHashable

        if (container == nil) {
            interp.stack_push(nil)
        }
        else if (container is List) {
            let list = container as! List
            var result: Any? = nil
            for (index, item) in list.enumerated() {
                if (item as! AnyHashable == val) {
                    result = index
                    break
                }
            }
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            var result: AnyHashable? = nil
            for (k, v) in record.elements {
                if (v as! AnyHashable == val) {
                    result = k
                    break
                }
            }
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( list initial forthic -- value )
    // ( record initial forthic -- value )
    func word_REDUCE(interp: Interpreter) throws {
        let _forthic = try interp.stack_pop()
        let initial = try interp.stack_pop()
        let container = try interp.stack_pop()

        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String


        if (container == nil) {
            interp.stack_push(initial)
        }
        else if (container is List) {
            let list = container as! List
            interp.stack_push(initial)
            for item in list {
                interp.stack_push(item)
                try interp.run(forthic: forthic)
            }
            let result = try interp.stack_pop()
            interp.stack_push(result)
        }
        else if (container is Record) {
            let record = container as! Record
            interp.stack_push(initial)
            for v in record.values {
                interp.stack_push(v)
                try interp.run(forthic: forthic)
            }
            let result = try interp.stack_pop()
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
    }

    // ( a -- )
    func word_POP(interp: Interpreter) throws {
        _ = try interp.stack_pop()
    }

    // ( a b -- b a )
    func word_SWAP(interp: Interpreter) throws {
        let b = try interp.stack_pop()
        let a = try interp.stack_pop()
        interp.stack_push(b)
        interp.stack_push(a)
    }

    // ( a -- a a )
    func word_DUP(interp: Interpreter) throws {
        let a = try interp.stack_pop()
        interp.stack_push(a)
        interp.stack_push(a)
    }

    // ( time -- time )
    func word_AM(interp: Interpreter) throws {
        let _time = try interp.stack_pop()
        if (!(_time is Date)) {
            throw GlobalModuleError.INVALID_TIME(value: _time)
        }

        // Extract the hour and minute in the global module's timezone
        let time = _time as! Date
        let calendar = get_current_calendar()
        let hour = calendar.component(.hour, from: time)
        let minute = calendar.component(.minute, from: time)

        // Update hour if necessary
        var result: Date = time
        if (hour >= 12) {
            let new_hour = hour - 12
            result = self.time_formatter.date(from: "\(new_hour):\(minute)")!
        }
        interp.stack_push(result)
    }

    // ( time -- time )
    func word_PM(interp: Interpreter) throws {
        let _time = try interp.stack_pop()
        if (!(_time is Date)) {
            throw GlobalModuleError.INVALID_TIME(value: _time)
        }

        // Extract the hour and minute in the global module's timezone
        let time = _time as! Date
        let calendar = get_current_calendar()
        let hour = calendar.component(.hour, from: time)
        let minute = calendar.component(.minute, from: time)

        // Update hour if necessary
        var result: Date = time
        if (hour < 12) {
            let new_hour = hour + 12
            result = self.time_formatter.date(from: "\(new_hour):\(minute)")!
        }
        interp.stack_push(result)
    }

    // (  -- time )
    func word_NOW(interp: Interpreter) throws {
        let result = Date()
        interp.stack_push(result)
    }

    // ( str  -- time )
    func word_to_TIME(interp: Interpreter) throws {
        let obj = try interp.stack_pop()

        if (obj is Date) {
            interp.stack_push(obj)
            return
        }
        else if (obj is String) {
            let str = obj as! String
            let result = to_time(str_val: str)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: obj)
        }
    }

    // ( time -- str )
    func word_TIME_to_STR(interp: Interpreter) throws {
        let _time = try interp.stack_pop()
        if (!(_time is Date)) {
            throw GlobalModuleError.INVALID_TIME(value: _time)
        }

        // Extract the hour and minute in the global module's timezone
        let time = _time as! Date
        let calendar = get_current_calendar()
        let hour = calendar.component(.hour, from: time)
        let minute = calendar.component(.minute, from: time)


        // Update hour if necessary
        let result = "\(hour):\(minute)"
        interp.stack_push(result)
    }

    // ( obj -- date )
    func word_to_DATE(interp: Interpreter) throws {
        let obj = try interp.stack_pop()

        if (obj is Date) {
            interp.stack_push(obj)
            return
        }
        else if (obj is String) {
            let str = obj as! String
            let result = to_date(str_val: str)
            interp.stack_push(result)
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: obj)
        }
    }

    // (  -- date )
    func word_TODAY(interp: Interpreter) throws {
        let result = Date()
        interp.stack_push(result)
    }

    // (  -- monday )
    func word_MONDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 2)
        interp.stack_push(result)
    }

    // (  -- tuesday )
    func word_TUESDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 3)
        interp.stack_push(result)
    }

    // (  -- wednesday )
    func word_WEDNESDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 4)
        interp.stack_push(result)
    }

    // (  -- thursday )
    func word_THURSDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 5)
        interp.stack_push(result)
    }

    // (  -- friday )
    func word_FRIDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 6)
        interp.stack_push(result)
    }

    // (  -- saturday )
    func word_SATURDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 7)
        interp.stack_push(result)
    }

    // (  -- sunday )
    func word_SUNDAY(interp: Interpreter) throws {
        let result = day_this_week(weekday: 1)
        interp.stack_push(result)
    }

    // ( date -- date )
    func word_NEXT(interp: Interpreter) throws {
        let _date = try interp.stack_pop()
        if (!(_date is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _date)
        }
        let date = _date as! Date
        let calendar = get_current_calendar()
        let today = Date()
        var result = date
        if date < today {
            result = calendar.date(byAdding: .day, value: 7, to: date)!
        }
        interp.stack_push(result)
    }

    // ( date num_days -- date )
    func word_ADD_DAYS(interp: Interpreter) throws {
        let _num = try interp.stack_pop()
        let _date = try interp.stack_pop()
        if (!(_num is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _num)
        }
        let num = _num as! Int

        if (!(_date is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _date)
        }
        let date = _date as! Date

        let calendar = get_current_calendar()
        let result = calendar.date(byAdding: .day, value: num, to: date)
        interp.stack_push(result)
    }

    // ( ldate rdate -- num_days )
    func word_SUBTRACT_DATES(interp: Interpreter) throws {
        let _rdate = try interp.stack_pop()
        let _ldate = try interp.stack_pop()

        if (!(_ldate is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _ldate)
        }
        var ldate = _ldate as! Date

        if (!(_rdate is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _rdate)
        }
        var rdate = _rdate as! Date

        let calendar = get_current_calendar()

        // Set dates to midnight
        ldate = calendar.startOfDay(for: ldate)
        rdate = calendar.startOfDay(for: rdate)

        let components = calendar.dateComponents([.day], from: rdate, to: ldate)
        let result = components.day
        interp.stack_push(result)
    }

    // ( ltime rtime -- num_seconds )
    func word_SUBTRACT_TIMES(interp: Interpreter) throws {
        let _rtime = try interp.stack_pop()
        let _ltime = try interp.stack_pop()

        if (!(_ltime is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _ltime)
        }
        let ltime = _ltime as! Date

        if (!(_rtime is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _rtime)
        }
        let rtime = _rtime as! Date

        let calendar = get_current_calendar()

        let components = calendar.dateComponents([.second], from: rtime, to: ltime)
        let result = components.second
        interp.stack_push(result)
    }

    // ( date -- str )
    func word_DATE_to_STR(interp: Interpreter) throws {
        let _date = try interp.stack_pop()
        if (!(_date is Date)) {
            throw GlobalModuleError.INVALID_TIME(value: _date)
        }

        // Extract the hour and minute in the global module's timezone
        let date = _date as! Date
        let calendar = get_current_calendar()
        let year = calendar.component(.year, from: date)
        let month = calendar.component(.month, from: date)
        let day = calendar.component(.day, from: date)

        func pad_number(_ number: Int) -> String {
            var res = "\(number)"
            if number < 10 {
                res = "0\(number)"
            }
            return res
        }

        let result = "\(year)-\(pad_number(month))-\(pad_number(day))"
        interp.stack_push(result)
    }

    // ( date time -- date )
    func word_DATE_TIME_to_DATETIME(interp: Interpreter) throws {
        let _time = try interp.stack_pop()
        if (!(_time is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _time)
        }
        let time = _time as! Date

        let _date = try interp.stack_pop()
        if (!(_date is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _date)
        }
        let date = _date as! Date

        let calendar = get_current_calendar()
        let hour = calendar.component(.hour, from: time)
        let minute = calendar.component(.minute, from: time)

        let result = calendar.date(bySettingHour: hour, minute: minute, second: 0, of: date)

        interp.stack_push(result)
    }

    // ( datetime -- timestamp )
    func word_DATETIME_to_TIMESTAMP(interp: Interpreter) throws {
        let _datetime = try interp.stack_pop()
        if (!(_datetime is Date)) {
            throw GlobalModuleError.DATE_EXPECTED(value: _datetime)
        }
        let datetime = _datetime as! Date
        let result = Int(datetime.timeIntervalSince1970)
        interp.stack_push(result)
    }

    // ( timestamp -- datetime )
    func word_TIMESTAMP_to_DATETIME(interp: Interpreter) throws {
        let _timestamp = try interp.stack_pop()
        if (!(_timestamp is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _timestamp)
        }
        let timestamp = _timestamp as! Int
        let result = Date(timeIntervalSince1970: Double(timestamp))
        interp.stack_push(result)
    }

    // ( str -- datetime )
    func word_STR_to_DATETIME(interp: Interpreter) throws {
        let str = try pop_string(interp: interp)
        let result = to_time(str_val: str)
        interp.stack_push(result)
    }

    // ( a b -- sum )
    // ( [a1 a2...] -- sum )
    func word_plus(interp: Interpreter) throws {
        let b = try interp.stack_pop()

        var result: Double
        if (b is [Any]) {
            result = try self.sum_as_doubles(b as! [Any])
        }
        else {
            let a = try interp.stack_pop()
            result = try self.sum_as_doubles([a, b])
        }
        interp.stack_push(result)
    }

    // ( a b -- res )
    func word_minus(interp: Interpreter) throws {
        let _b = try pop_double(interp: interp)
        let _a = try pop_double(interp: interp)

        if (_a == nil) {
            interp.stack_push(nil)
            return
        }
        let a = _a!

        if (_b == nil) {
            interp.stack_push(nil)
            return
        }
        let b = _b!


        let result = a - b
        interp.stack_push(result)
    }

    // ( a b -- product )
    // ( [a1 a2...] -- sum )
    func word_times(interp: Interpreter) throws {
        let b = try interp.stack_pop()

        var result: Double
        if (b is [Any]) {
            result = try self.product_as_doubles(b as! [Any])
        }
        else {
            let a = try interp.stack_pop()
            result = try self.product_as_doubles([a, b])
        }
        interp.stack_push(result)
    }

    // ( a b -- res )
    func word_divide_by(interp: Interpreter) throws {
        let _b = try pop_double(interp: interp)
        let _a = try pop_double(interp: interp)

        if (_a == nil) {
            interp.stack_push(nil)
            return
        }
        let a = _a!

        if (_b == nil) {
            interp.stack_push(nil)
            return
        }
        let b = _b!


        let result = a / b
        interp.stack_push(result)
    }

    // ( m n -- m%n )
    func word_MOD(interp: Interpreter) throws {
        let _n = try interp.stack_pop()
        let _m = try interp.stack_pop()

        // Condition n and m
        if (_n == nil) {
            interp.stack_push(nil)
            return
        }
        else if (!(_n is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _n)
        }
        let n = _n as! Int

        if (_m == nil) {
            interp.stack_push(nil)
            return
        }
        else if (!(_m is Int)) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _m)
        }
        let m = _m as! Int

        interp.stack_push(m % n)
    }

    // ( [a1 a2...] -- max )
    func word_MAX(interp: Interpreter) throws {
        let list = try pop_list(interp: interp)

        if (list is [String]) {
            let string_list = list as! [String]
            interp.stack_push(string_list.max())
        }
        else if (list is [Int]) {
            let int_list = list as! [Int]
            interp.stack_push(int_list.max())
        }
        else if (list is [Double]) {
            let double_list = list as! [Double]
            interp.stack_push(double_list.max())
        }
        else if (list is [Date]) {
            let date_list = list as! [Date]
            interp.stack_push(date_list.max())
        }
        else {
            throw GlobalModuleError.COMPARABLE_LIST_EXPECTED(item: list)
        }
    }

    // ( [a1 a2...] -- min )
    func word_MIN(interp: Interpreter) throws {
        let list = try pop_list(interp: interp)

        if (list is [String]) {
            let string_list = list as! [String]
            interp.stack_push(string_list.min())
        }
        else if (list is [Int]) {
            let int_list = list as! [Int]
            interp.stack_push(int_list.min())
        }
        else if (list is [Double]) {
            let double_list = list as! [Double]
            interp.stack_push(double_list.min())
        }
        else if (list is [Date]) {
            let date_list = list as! [Date]
            interp.stack_push(date_list.min())
        }
        else {
            throw GlobalModuleError.COMPARABLE_LIST_EXPECTED(item: list)
        }
    }

    // ( a -- a_int )
    func word_to_INT(interp: Interpreter) throws {
        let a = try interp.stack_pop()

        if (a == nil) {
            interp.stack_push(0)
        }
        else if (a is List) {
            interp.stack_push((a as! List).count)
        }
        else if (a is Record) {
            interp.stack_push((a as! Record).count)
        }
        else if (a is Int ){
            interp.stack_push(a as! Int)
        }
        else if (a is Float) {
            interp.stack_push(Int(a as! Float))
        }
        else if (a is Double) {
            interp.stack_push(Int(a as! Double))
        }
        else if (a is String) {
            interp.stack_push(Int(a as! String))
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: a)
        }
    }

    // ( a -- a_bool )
    func word_to_BOOL(interp: Interpreter) throws {
        let a = try interp.stack_pop()

        if (a == nil) {
            interp.stack_push(false)
        }
        else if (a is Bool) {
            interp.stack_push(a as! Bool)
        }
        else if (a is Int) {
            let a_int = a as! Int
            if (a_int == 0) {
                interp.stack_push(false)
            }
            else {
                interp.stack_push(true)
            }
        }
        else if (a is String) {
            let a_string = a as! String
            if (a_string == "") {
                interp.stack_push(false)
            }
            else {
                interp.stack_push(true)
            }
        }
        else {
            interp.stack_push(true)
        }
    }

    // ( a -- a_double )
    func word_to_DOUBLE(interp: Interpreter) throws {
        let a = try interp.stack_pop()

        if (a == nil) {
            interp.stack_push(0.0)
        }
        else if (a is List) {
            interp.stack_push(Double((a as! List).count))
        }
        else if (a is Record) {
            interp.stack_push(Double((a as! Record).count))
        }
        else if (a is Int ){
            interp.stack_push(Double(a as! Int))
        }
        else if (a is Float) {
            interp.stack_push(Double(a as! Float))
        }
        else if (a is Double) {
            interp.stack_push(a as! Double)
        }
        else if (a is String) {
            interp.stack_push(Double(a as! String))
        }
        else {
            throw GlobalModuleError.STRING_EXPECTED(value: a)
        }
    }

    // ( m n -- bool )
    func word_equal_equal(interp: Interpreter) throws {
        let n = try interp.stack_pop() as! AnyHashable?
        let m = try interp.stack_pop() as! AnyHashable?
        interp.stack_push(m == n)
    }

    // ( m n -- bool )
    func word_not_equal(interp: Interpreter) throws {
        let n = try interp.stack_pop() as! AnyHashable?
        let m = try interp.stack_pop() as! AnyHashable?
        interp.stack_push(m != n)
    }

    // ( m n -- bool )
    func word_greater_than(interp: Interpreter) throws {
        let n = try interp.stack_pop()
        let m = try interp.stack_pop()
        if (m is String && n is String) {
            interp.stack_push(m as! String > n as! String)
        }
        else if (m is Int && n is Int) {
            interp.stack_push(m as! Int > n as! Int)
        }
        else if (m is Double && n is Double) {
            interp.stack_push(m as! Double > n as! Double)
        }
        else if (m is Date && n is Date) {
            interp.stack_push(m as! Date > n as! Date)
        }
        else {
            throw GlobalModuleError.COMPARABLE_ITEMS_EXPECTED(item1: m, item2: n)
        }
    }

    // ( m n -- bool )
    func word_greater_than_or_equal(interp: Interpreter) throws {
        let n = try interp.stack_pop()
        let m = try interp.stack_pop()
        if (m is String && n is String) {
            interp.stack_push(m as! String >= n as! String)
        }
        else if (m is Int && n is Int) {
            interp.stack_push(m as! Int >= n as! Int)
        }
        else if (m is Double && n is Double) {
            interp.stack_push(m as! Double >= n as! Double)
        }
        else if (m is Date && n is Date) {
            interp.stack_push(m as! Date >= n as! Date)
        }
        else {
            throw GlobalModuleError.COMPARABLE_ITEMS_EXPECTED(item1: m, item2: n)
        }
    }

    // ( m n -- bool )
    func word_less_than(interp: Interpreter) throws {
        let n = try interp.stack_pop()
        let m = try interp.stack_pop()
        if (m is String && n is String) {
            interp.stack_push((m as! String) < (n as! String))
        }
        else if (m is Int && n is Int) {
            interp.stack_push((m as! Int) < (n as! Int))
        }
        else if (m is Double && n is Double) {
            interp.stack_push((m as! Double) < (n as! Double))
        }
        else if (m is Date && n is Date) {
            interp.stack_push((m as! Date) < (n as! Date))
        }
        else {
            throw GlobalModuleError.COMPARABLE_ITEMS_EXPECTED(item1: m, item2: n)
        }
    }

    // ( m n -- bool )
    func word_less_than_or_equal(interp: Interpreter) throws {
        let n = try interp.stack_pop()
        let m = try interp.stack_pop()
        if (m is String && n is String) {
            interp.stack_push((m as! String) <= (n as! String))
        }
        else if (m is Int && n is Int) {
            interp.stack_push((m as! Int) <= (n as! Int))
        }
        else if (m is Double && n is Double) {
            interp.stack_push((m as! Double) <= (n as! Double))
        }
        else if (m is Date && n is Date) {
            interp.stack_push((m as! Date) <= (n as! Date))
        }
        else {
            throw GlobalModuleError.COMPARABLE_ITEMS_EXPECTED(item1: m, item2: n)
        }
    }

    // ( a b -- bool )
    // ( [a1 a2 ...] -- bool )
    func word_OR(interp: Interpreter) throws {
        let b = try interp.stack_pop()
        if (b is List) {
            let bool_list = b as! [Bool]
            let result = bool_list.contains(where: {(val:Bool) -> Bool in
                return val == true
            })
            interp.stack_push(result)
            return
        }
        else if (b is Bool) {
            let a = try interp.stack_pop()
            if (a is Bool) {
                interp.stack_push(a as! Bool || b as! Bool)
                return
            }
        }

        throw GlobalModuleError.BOOL_EXPECTED(value: b)
    }

    // ( a b -- bool )
    // ( [a1 a2 ...] -- bool )
    func word_AND(interp: Interpreter) throws {
        let b = try interp.stack_pop()
        if (b is List) {
            let bool_list = b as! [Bool]
            let result = bool_list.allSatisfy({(val:Bool) -> Bool in
                return val == true
            })
            interp.stack_push(result)
            return
        }
        else if (b is Bool) {
            let a = try interp.stack_pop()
            if (a is Bool) {
                interp.stack_push(a as! Bool && b as! Bool)
                return
            }
        }
        throw GlobalModuleError.BOOL_EXPECTED(value: b)
    }

    // ( a -- bool )
    func word_NOT(interp: Interpreter) throws {
        let a = try interp.stack_pop()
        if (a is Bool) {
            interp.stack_push(!(a as! Bool))
        }
        else {
            throw GlobalModuleError.BOOL_EXPECTED(value: a)
        }
    }

    // ( item items -- bool )
    func word_IN(interp: Interpreter) throws {
        let items = Set(try pop_list(interp: interp) as! [AnyHashable])
        let item = try interp.stack_pop()

        interp.stack_push(items.contains(item as! AnyHashable))
    }

    // ( vals required_vals -- bool )
    func word_ANY(interp: Interpreter) throws {
        let required_vals = try pop_list(interp: interp) as! [AnyHashable]
        let vals = Set(try pop_list(interp: interp) as! [AnyHashable])
        var result = false

        for rv in required_vals {
            if vals.contains(rv) {
                result = true
                break
            }
        }

        // If nothing is required, then all values are true
        if (required_vals.count == 0) {
            result = true
        }

        interp.stack_push(result)
    }

    // ( vals required_vals -- bool )
    func word_ALL(interp: Interpreter) throws {
        let required_vals = Set(try pop_list(interp: interp) as! [AnyHashable])
        let vals = Set(try pop_list(interp: interp) as! [AnyHashable])

        let intersection_set = required_vals.intersection(vals)
        let result = intersection_set == required_vals

        interp.stack_push(result)
    }


    // ( str1 str2 -- string )
    // ( array_of_str -- string )
    func word_CONCAT(interp: Interpreter) throws {
        let str2 = try interp.stack_pop()

        // Condition args
        var list: List
        if (str2 is List) {
            list = str2 as! List
        }
        else {
            let str1 = try interp.stack_pop()
            list = [str1, str2]
        }

        var result = ""
        for item in list {
            result += to_string(item)
        }
        interp.stack_push(result)
    }

    // ( string sep -- items )
    func word_SPLIT(interp: Interpreter) throws {
        let _sep = try interp.stack_pop()
        let _string = try interp.stack_pop()

        if (!(_sep is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _sep)
        }
        let sep = _sep as! String

        if (!(_string is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _string)
        }
        let string = _string as! String

        var result: [String] = []
        if (sep == "") {
            result = [string]
        }
        else {
            result = string.components(separatedBy: sep)
        }
        interp.stack_push(result)
    }

    // ( array sep -- string )
    func word_JOIN(interp: Interpreter) throws {
        let sep = try pop_string(interp: interp)
        let list = try pop_list(interp: interp)

        var string_list: [String] = []
        for item in list {
            string_list.append(to_string(item))
        }

        let result = string_list.joined(separator: sep)
        interp.stack_push(result)
    }


    // ( -- \n )
    func word_slash_N(interp: Interpreter) throws {
        interp.stack_push("\n")
    }

    // ( -- \r )
    func word_slash_R(interp: Interpreter) throws {
        interp.stack_push("\r")
    }

    // ( -- \t )
    func word_slash_T(interp: Interpreter) throws {
        interp.stack_push("\t")
    }

    // ( STRING -- string )
    func word_LOWER(interp: Interpreter) throws {
        let string = try pop_string(interp: interp)
        let result = string.lowercased()
        interp.stack_push(result)
    }

    // ( string -- string )
    func word_STRIP(interp: Interpreter) throws {
        let string = try pop_string(interp: interp)

        let result = string.trimmingCharacters(in: [" ", "\t", "\r", "\n"])
        interp.stack_push(result)
    }

    // ( string s r -- string )
    func word_REPLACE(interp: Interpreter) throws {
        let r = try pop_string(interp: interp)
        let s = try pop_string(interp: interp)
        let string = try pop_string(interp: interp)

        let result = string.replacingOccurrences(of: s, with: r)
        interp.stack_push(result)
    }

    // ( string regex -- match )
    func word_RE_MATCH(interp: Interpreter) throws {
        let regex = try pop_string(interp: interp)
        let string = try pop_string(interp: interp)

        let range = NSRange(string.startIndex..<string.endIndex, in: string)
        let captureRegex = try! NSRegularExpression(pattern: regex, options: [])
        let matches = captureRegex.matches(in: string, options: [], range: range)

        if (matches.count > 0) {
            let match = matches.first!
            let result = self.get_match_groups(string: string, match: match)
            interp.stack_push(result)
        }
        else {
            interp.stack_push(nil)
        }
    }

    // ( string regex -- match )
    func word_RE_MATCH_ALL(interp: Interpreter) throws {
        let regex = try pop_string(interp: interp)
        let string = try pop_string(interp: interp)

        let range = NSRange(string.startIndex..<string.endIndex, in: string)
        let captureRegex = try! NSRegularExpression(pattern: regex, options: [])
        let matches = captureRegex.matches(in: string, options: [], range: range)

        if (matches.count > 0) {
            var result : [[String]] = []
            for match in matches {
                result.append(get_match_groups(string: string, match: match))
            }
            interp.stack_push(result)
        }
        else {
            interp.stack_push(nil)
        }
    }


    // ( object -- string )
    func word_to_STR(interp: Interpreter) throws {
        let object = try interp.stack_pop()
        interp.stack_push(to_string(object))
    }

    // ( string -- string )
    func word_URL_ENCODE(interp: Interpreter) throws {
        let string = try pop_string(interp: interp)
        let result = string.addingPercentEncoding(withAllowedCharacters: .urlHostAllowed)
        interp.stack_push(result)
    }

    // ( string -- string )
    func word_URL_DECODE(interp: Interpreter) throws {
        let string = try pop_string(interp: interp)
        let result = string.removingPercentEncoding
        interp.stack_push(result)
    }

    // ( -- nil )
    func word_NULL(interp: Interpreter) throws {
        interp.stack_push(nil)
    }

    // ( -- quote_char )
    func word_QUOTE_CHAR(interp: Interpreter) throws {
        interp.stack_push(DLE)
    }

    // ( string -- quoted_string )
    func word_QUOTED(interp: Interpreter) throws {
        let string = try pop_string(interp: interp)
        var clean_string: String = ""
        for c in string {
            if (c == DLE) {
                clean_string.append(" ")
            }
            else {
                clean_string.append(c)
            }
        }
        let result = "\(DLE)\(clean_string)\(DLE)"
        interp.stack_push(result)
    }

    // ( value default_value -- val )
    func word_DEFAULT(interp: Interpreter) throws {
        let default_value = try interp.stack_pop()
        let value = try interp.stack_pop()

        if (value == nil) {
            interp.stack_push(default_value)
        }
        else if (value is String && value as! String == "") {
            interp.stack_push(default_value)
        }
        else {
            interp.stack_push(value)
        }
    }

    // ( value default_forthic -- val )
    func word_star_DEFAULT(interp: Interpreter) throws {
        let default_forthic = try pop_string(interp: interp)
        let value = try interp.stack_pop()

        if (value == nil) {
            try interp.run(forthic: default_forthic)
        }
        else if (value is String && value as! String == "") {
            try interp.run(forthic: default_forthic)
        }
        else {
            interp.stack_push(value)
        }
    }

    // ( item forthic num-times -- ? )
    func word_l_REPEAT(interp: Interpreter) throws {
        let _num_times = try pop_int(interp: interp)
        let forthic = try pop_string(interp: interp)

        if (_num_times == nil) {
            return
        }
        let num_times = _num_times!

        for _ in 0..<num_times {
            let item = try interp.stack_pop()
            interp.stack_push(item)
            try interp.run(forthic: forthic)
            let res = try interp.stack_pop()

            interp.stack_push(item)
            interp.stack_push(res)
        }
    }

    // ( -- )
    func word_IDENTITY(interp: Interpreter) throws {
    }

    // ( num digits -- str )
    func word_to_FIXED(interp: Interpreter) throws {
        let _digits = try pop_int(interp: interp)
        let _num = try interp.stack_pop()

        if (_digits == nil) {
            throw GlobalModuleError.INTEGER_EXPECTED(value: _digits)
        }
        let digits = _digits!

        let formatter = NumberFormatter()
        formatter.minimumFractionDigits = digits
        formatter.maximumFractionDigits = digits

        if (_num == nil) {
            interp.stack_push(nil)
        }
        else if (_num is Int) {
            let num = NSNumber(integerLiteral: _num as! Int)
            interp.stack_push(formatter.string(from: num))
        }
        else if (_num is Float) {
            let num = NSNumber(floatLiteral: Double(_num as! Float))
            interp.stack_push(formatter.string(from: num))
        }
        else if (_num is Double) {
            let num = NSNumber(floatLiteral: _num as! Double)
            interp.stack_push(formatter.string(from: num))
        }
        else {
            throw GlobalModuleError.NUMBER_EXPECTED(value: _num)
        }
    }

    // ( obj -- json )
    func word_to_JSON(interp: Interpreter) throws {
        let obj = try interp.stack_pop()
        let encoder = JSONEncoder()
        encoder.outputFormatting = .prettyPrinted

        let json_obj = try to_json_object(obj)

        let data = try encoder.encode(json_obj)
        let result = String(data: data, encoding: .utf8)!
        interp.stack_push(result)
    }

    // ( json -- obj )
    func word_JSON_to(interp: Interpreter) throws {
        let json = try pop_string(interp: interp)

        let data = Data(json.utf8)
        let object = try JSONSerialization.jsonObject(with: data, options: [])
        let result = to_forthic_object(object)
        interp.stack_push(result)
    }

    // ( -- )
    func word_dot_s(interp: Interpreter) throws {
        throw GlobalModuleError.STACK_DUMP(item: interp.stack)
    }

    // ----- Internal Functions ---------------------------------------------------------------------------------------
    func compare_any(l: Any?, r: Any?) throws -> Bool {
        if (l == nil) {
            return false
        }
        if (r == nil) {
            return true
        }

        if (l is Int && r is Int) {
            return (l as! Int) < (r as! Int)
        }
        else if (l is Float && r is Float) {
            return (l as! Float) < (r as! Float)
        }
        else if (l is Double && r is Double) {
            return (l as! Double) < (r as! Double)
        }
        else if (l is String && r is String) {
            return (l as! String) < (r as! String)
        }
        else {
            throw GlobalModuleError.COMPARABLE_ITEMS_EXPECTED(item1: l, item2: r)
        }
    }

    func foreach(interp: Interpreter, return_errors: Bool=false) throws -> List {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        // Check _forthic
        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String


        func process_item(interp: Interpreter, item: Any?, return_errors: Bool, errors: inout List) throws {
            interp.stack_push(item)
            if (return_errors) {
                errors.append(run_returning_error(interp, forthic))
            }
            else {
                try interp.run(forthic: forthic)
            }
        }

        // Handle container cases
        var errors = List()
        if (container == nil) {
            return errors
        }
        else if (container is List) {
            let items = container as! List
            for item in items {
                try process_item(interp: interp, item: item, return_errors: return_errors, errors: &errors)
            }
        }
        else if (container is Record) {
            let record = container as! Record
            for item in record.values {
                try process_item(interp: interp, item: item, return_errors: return_errors, errors: &errors)
            }
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        return errors
    }

    func foreach_w_key(interp: Interpreter, return_errors: Bool=false) throws -> List {
        let _forthic = try interp.stack_pop()
        let container = try interp.stack_pop()

        // Check _forthic
        if (!(_forthic is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _forthic)
        }
        let forthic = _forthic as! String


        func process_item(interp: Interpreter, key: AnyHashable, item: Any?, return_errors: Bool, errors: inout List) throws {
            interp.stack_push(key)
            interp.stack_push(item)
            if (return_errors) {
                errors.append(run_returning_error(interp, forthic))
            }
            else {
                try interp.run(forthic: forthic)
            }
        }

        // Handle container cases
        var errors = List()
        if (container == nil) {
            return errors
        }
        else if (container is List) {
            let items = container as! List
            for (index, item) in items.enumerated() {
                try process_item(interp: interp, key: index, item: item, return_errors: return_errors, errors: &errors)
            }
        }
        else if (container is Record) {
            let record = container as! Record
            for (key, item) in record.elements {
                try process_item(interp: interp, key: key, item: item, return_errors: return_errors, errors: &errors)
            }
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        return errors
    }

    func run_returning_error(_ interp: Interpreter, _ forthic: String) -> Error? {
        do {
            try interp.run(forthic: forthic)
        } catch {
            return error
        }
        return nil
    }

    func to_string(_ object: Any?) -> String {
        var result: String = ""
        if (object == nil) {
            result = ""
        }
        else if (object is String) {
            result = object as! String
        }
        else if (object is Int) {
            result = (object as! Int).description
        }
        else if (object is Float) {
            result = (object as! Float).description
        }
        else if (object is Double) {
            result = (object as! Double).description
        }
        else {
            result = String(describing: object)
        }
        return result
    }

    func to_set(_ list: List) -> OrderedSet<AnyHashable?> {
        var result = OrderedSet<AnyHashable?>()
        for item in list {
            let hashable = item as! AnyHashable?
            result.append(hashable)
        }
        return result
    }

    func get_container_keys(_ container: Any?) throws -> List {
        var result: List
        if (container == nil) {
            result = []
        }
        else if (container is List) {
            let list = container as! List
            result = Array<Int>(0..<list.count)
        }
        else if (container is Record) {
            result = Array((container as! Record).keys)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        return result
    }

    func get_container_values(_ container: Any?) throws -> List {
        var values: List
        if (container == nil) {
            values = []
        }
        else if (container is List) {
            values = container as! List
        }
        else if (container is Record) {
            values = Array((container as! Record).values)
        }
        else {
            throw GlobalModuleError.LIST_EXPECTED(value: container)
        }
        return values
    }

    func add_to_group(result: inout Record, group: AnyHashable, value: Any?) {
        if (result[group] == nil) {
            result[group] = [value]
        }
        else {
            var list = result[group] as! List
            list.append(value)
            result[group] = list
        }
    }


    // Descends into record using an array of fields, returning final value or nil
    func drill_for_value(rec: Record, fields: [String]) throws -> Any? {
        var result: Any? = rec

        for f in fields {
            if (result == nil) {
                break
            }
            else if (result is Record) {
                result = (result as! Record)[f]
            }
            else {
                throw GlobalModuleError.RECORD_EXPECTED(value: result)
            }
        }
        return result
    }

    func sum_as_doubles(_ numbers: List) throws -> Double {
        var result: Double = 0
        for _n in numbers {
            if (_n is Int) {
                let n = _n as! Int
                result += Double(n)
            }
            else if (_n is Float) {
                let n = _n as! Float
                result += Double(n)
            }
            else if (_n is Double) {
                let n = _n as! Double
                result += Double(n)
            }
            else {
                throw GlobalModuleError.NUMBER_EXPECTED(value: _n)
            }
        }
        return result
    }

    func product_as_doubles(_ numbers: List) throws -> Double {
        var result: Double = 1
        for _n in numbers {
            if (_n is Int) {
                let n = _n as! Int
                result *= Double(n)
            }
            else if (_n is Float) {
                let n = _n as! Float
                result *= Double(n)
            }
            else if (_n is Double) {
                let n = _n as! Double
                result *= Double(n)
            }
            else {
                throw GlobalModuleError.NUMBER_EXPECTED(value: _n)
            }
        }
        return result
    }

    /// Converts a string into a literal using one of the registered converters
    func find_literal_word(_ str: String) -> Word? {
        for handler in self.literal_handlers {
            let value = handler(str)
            if (value != nil) {
                return PushValueWord(name: str, value: value!)
            }
        }
        return nil
    }

    func get_match_groups(string: String, match: NSTextCheckingResult) -> [String] {
        var res: [String] = []
        for rangeIndex in 0..<match.numberOfRanges {
            let matchRange = match.range(at: rangeIndex)

            // Extract the substring matching the capture group
            if let substringRange = Range(matchRange, in: string) {
                let capture = String(string[substringRange])
                res.append(capture)
            }
        }
        return res
    }

    func pop_string(interp: Interpreter) throws -> String {
        let _result = try interp.stack_pop()

        if (!(_result is String)) {
            throw GlobalModuleError.STRING_EXPECTED(value: _result)
        }
        let result = _result as! String
        return result
    }

    func pop_int(interp: Interpreter) throws -> Int? {
        let _result = try interp.stack_pop()

        if (_result == nil) {
            return nil
        }
        else if (!(_result is Int)) {
            throw GlobalModuleError.NUMBER_EXPECTED(value: _result)
        }
        let result = _result as! Int
        return result
    }

    func pop_double(interp: Interpreter) throws -> Double? {
        let _result = try interp.stack_pop()

        if (_result == nil) {
            return nil
        }
        else if (_result is Int) {
            return Double(_result as! Int)
        }
        else if (_result is Float) {
            return Double(_result as! Float)
        }
        else if (_result is Double) {
            return _result as? Double
        }
        else {
            throw GlobalModuleError.NUMBER_EXPECTED(value: _result)
        }
    }

    func pop_list(interp: Interpreter) throws -> List {
        let _result = try interp.stack_pop()

        if (!(_result is List)) {
            throw GlobalModuleError.LIST_EXPECTED(value: _result)
        }
        let result = _result as! List
        return result
    }

    func to_forthic_object(_ obj: Any) -> Any {
        if (obj is Dictionary<String, Any>) {
            let dict = obj as! Dictionary<String, Any>
            var result = Record()
            for k in dict.keys {
                result[k] = to_forthic_object(dict[k]!)
            }
            return result
        }
        else if (obj is Array<Any>) {
            var result = List()
            let array = obj as! Array<Any>
            for item in array {
                result.append(to_forthic_object(item))
            }
            return result
        }
        else {
            return obj
        }
    }

    class JSONCodable : Encodable {
        enum JSONType {
            case string
            case int
            case bool
            case double
            case list
            case record
        }

        var type: JSONType
        var string_val: String = ""
        var int_val: Int = 0
        var double_val: Double = 0.0
        var bool_val: Bool = false
        var list_val: [JSONCodable] = []
        var record_val: Dictionary<String, JSONCodable> = [:]

        init(withString: String) {
            self.type = JSONType.string
            self.string_val = withString
        }

        init(withInt: Int) {
            self.type = JSONType.int
            self.int_val = withInt
        }

        init(withDouble: Double) {
            self.type = JSONType.double
            self.double_val = withDouble
        }

        init(withBool: Bool) {
            self.type = JSONType.bool
            self.bool_val = withBool
        }

        init(forType: JSONType) {
            self.type = forType
        }

        func append(item: JSONCodable) throws {
            switch (self.type) {
            case .list:
                self.list_val.append(item)
            default:
                throw GlobalModuleError.LIST_EXPECTED(value: self.type)
            }
        }

        func set_value(key: String, item: JSONCodable) throws {
            switch (self.type) {
            case .record:
                self.record_val[key] = item
            default:
                throw GlobalModuleError.RECORD_EXPECTED(value: self.type)
            }
        }

        func encode(to encoder: Encoder) throws {
            var container = encoder.singleValueContainer()
            switch (self.type) {
            case .string:
                try container.encode(self.string_val)

            case .int:
                try container.encode(self.int_val)

            case .double:
                try container.encode(self.double_val)

            case .bool:
                try container.encode(self.bool_val)

            case.list:
                try container.encode(self.list_val)

            case.record:
                try container.encode(self.record_val)
            }
        }
    }


    func to_json_object(_ obj: Any?) throws -> JSONCodable {
        var result : JSONCodable

        if (obj == nil) {
            result = JSONCodable(withString: "")
        }
        else if (obj is List) {
            let list = obj as! List
            result = JSONCodable.init(forType: .list)
            for item in list {
                try result.append(item: to_json_object(item))
            }
        }
        else if (obj is Record) {
            let record = obj as! Record
            result = JSONCodable.init(forType: .record)
            for (key, value) in record.elements {
                let key_string = to_string(key)
                try result.set_value(key: key_string, item: to_json_object(value))
            }
        }
        else if (obj is String) {
            result = JSONCodable(withString: obj as! String)
        }
        else if (obj is Int) {
            result = JSONCodable(withInt: obj as! Int)
        }
        else if (obj is Float) {
            result = JSONCodable(withDouble: Double(obj as! Float))
        }
        else if (obj is Double) {
            result = JSONCodable(withDouble: obj as! Double)
        }
        else if (obj is Bool) {
            result = JSONCodable(withBool: obj as! Bool)
        }
        else {
            throw GlobalModuleError.CANNOT_DUMP_AS_JSON(item: obj)
        }

        return result
    }

    func day_this_week(weekday: Int) -> Date {
        // For weekday, Sunday is 1

        // NOTE: We treat Monday as the start of the week, so we need to make Monday 1
        func adjust_weekday(weekday: Int) -> Int {
            var res = weekday - 1;
            if res == 0 {  // Handle Sunday
                res = 7
            }
            return res
        }
        let weekday_adj = adjust_weekday(weekday: weekday)

        let today = Date()
        let calendar = get_current_calendar()
        let current_weekday = calendar.component(.weekday, from: today)
        let current_weekday_adj = adjust_weekday(weekday: current_weekday)
        var delta_days = (weekday_adj - current_weekday_adj) % 7
        if weekday_adj < current_weekday_adj && delta_days > 0 {
            delta_days -= 7
        }
        else if weekday_adj > current_weekday_adj && delta_days < 0 {
            delta_days += 7
        }
        let result = calendar.date(byAdding: .day, value: delta_days, to: today)
        return result!
    }

    func get_current_calendar() -> Calendar {
        var result = Calendar.current
        result.timeZone = self.timezone
        return result
    }

}
