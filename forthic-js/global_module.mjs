import { Module, Word, PushValueWord } from './module.mjs';

let DLE = String.fromCharCode(16);  // ASCII char for "Data Link Escape" used as an untypeable quote


class MemoWord extends Word {
    constructor(name, varname) {
        super(name);
        this.varname = varname;
    }

    async execute(interp) {
        // If varname is null, execute name!
        await interp.run(this.varname + " @");
        let var_value = interp.stack_pop();
        if (!var_value)   await interp.run(this.name + "!");

        // Return value of variable
        await interp.run(this.varname + " @");
    }
}

class GlobalModule extends Module {
    constructor(interp) {
        super("<GLOBAL>", interp);
        this.literal_handlers = [this.to_bool, this.to_float, this.to_int, this.to_date, this.to_time];

        // --------------------
        // Base words
        this.add_module_word("VARIABLES", this.word_VARIABLES);
        this.add_module_word("!", this.word_bang);
        this.add_module_word("@", this.word_at);
        this.add_module_word("!@", this.word_bang_at);
        this.add_module_word("INTERPRET", this.word_INTERPRET);
        this.add_module_word("MEMO", this.word_MEMO);
        this.add_module_word("EXPORT", this.word_EXPORT);
        this.add_module_word("USE-MODULES", this.word_USE_MODULES);
        this.add_module_word("REC", this.word_REC);
        this.add_module_word("REC@", this.word_REC_at);
        this.add_module_word("<REC!", this.word_l_REC_bang);

        // ----------------
        // Array/Record words
        this.add_module_word("APPEND", this.word_APPEND);
        this.add_module_word("REVERSE", this.word_REVERSE);
        this.add_module_word("UNIQUE", this.word_UNIQUE);
        this.add_module_word("<DEL", this.word_L_DEL);
        this.add_module_word("RELABEL", this.word_RELABEL);
        this.add_module_word("BY-FIELD", this.word_BY_FIELD);
        this.add_module_word("GROUP-BY-FIELD", this.word_GROUP_BY_FIELD);
        this.add_module_word("GROUP-BY", this.word_GROUP_BY);
        this.add_module_word("GROUP-BY-w/KEY", this.word_GROUP_BY_w_KEY);
        this.add_module_word("GROUPS-OF", this.word_GROUPS_OF);
        this.add_module_word("INDEX", this.word_INDEX);
        this.add_module_word("MAP", this.word_MAP);
        this.add_module_word("MAP-w/KEY", this.word_MAP_w_KEY);
        this.add_module_word("FOREACH", this.word_FOREACH);
        this.add_module_word("FOREACH-w/KEY", this.word_FOREACH_w_KEY);
        this.add_module_word("ZIP", this.word_ZIP);
        this.add_module_word("ZIP-WITH", this.word_ZIP_WITH);
        this.add_module_word("KEYS", this.word_KEYS);
        this.add_module_word("VALUES", this.word_VALUES);
        this.add_module_word("LENGTH", this.word_LENGTH);
        this.add_module_word("SLICE", this.word_SLICE);
        this.add_module_word("DIFFERENCE", this.word_DIFFERENCE);
        this.add_module_word("INTERSECTION", this.word_INTERSECTION);
        this.add_module_word("UNION", this.word_UNION);
        this.add_module_word("SELECT", this.word_SELECT);
        this.add_module_word("SELECT-w/KEY", this.word_SELECT_w_KEY);
        this.add_module_word("TAKE", this.word_TAKE);
        this.add_module_word("DROP", this.word_DROP);
        this.add_module_word("ROTATE", this.word_ROTATE);
        this.add_module_word("ROTATE-ELEMENT", this.word_ROTATE_ELEMENT);

        this.add_module_word("SHUFFLE", this.word_SHUFFLE);
        this.add_module_word("SORT", this.word_SORT);
        this.add_module_word("SORT-w/FORTHIC", this.word_SORT_w_FORTHIC);
        this.add_module_word("SORT-w/KEY-FUNC", this.word_SORT_w_KEY_FUNC);
        this.add_module_word("FIELD-KEY-FUNC", this.word_FIELD_KEY_FUNC);
        this.add_module_word("NTH", this.word_NTH);
        this.add_module_word("LAST", this.word_LAST);
        this.add_module_word("UNPACK", this.word_UNPACK);
        this.add_module_word("FLATTEN", this.word_FLATTEN);
        this.add_module_word("KEY-OF", this.word_KEY_OF);
        this.add_module_word("REDUCE", this.word_REDUCE);

        // --------------------
        // Stack words
        this.add_module_word("POP", this.word_POP);
        this.add_module_word("DUP", this.word_DUP);
        this.add_module_word("SWAP", this.word_SWAP);

        // --------------------
        // String words
        this.add_module_word(">STR", this.word_to_STR);
        this.add_module_word("CONCAT", this.word_CONCAT);
        this.add_module_word("SPLIT", this.word_SPLIT);
        this.add_module_word("JOIN", this.word_JOIN);
        this.add_module_word("/N", this.word_slash_N);
        this.add_module_word("/R", this.word_slash_R);
        this.add_module_word("/T", this.word_slash_T);
        this.add_module_word("|LOWER", this.word_pipe_LOWER);
        this.add_module_word("|UPPER", this.word_pipe_UPPER);
        this.add_module_word("|ASCII", this.word_pipe_ASCII);
        this.add_module_word("STRIP", this.word_STRIP);
        this.add_module_word("REPLACE", this.word_REPLACE);
        this.add_module_word("RE-MATCH", this.word_RE_MATCH);
        this.add_module_word("RE-MATCH-GROUP", this.word_RE_MATCH_GROUP);
        this.add_module_word("RE-MATCH-ALL", this.word_RE_MATCH_ALL);


        // --------------------
        // Misc words
        this.add_module_word("NULL", this.word_NULL);
        this.add_module_word("DEFAULT", this.word_DEFAULT);
        this.add_module_word("*DEFAULT", this.word_star_DEFAULT);
        this.add_module_word("REC-DEFAULTS", this.word_REC_DEFAULTS);
        this.add_module_word("<REPEAT", this.word_l_REPEAT);
        this.add_module_word("IDENTITY", this.word_IDENTITY);
        this.add_module_word(">FIXED", this.word_to_FIXED);
        this.add_module_word(">JSON", this.word_to_JSON);
        this.add_module_word("JSON>", this.word_JSON_to);
        this.add_module_word(".s", this.word_dot_s);
        this.add_module_word(".p", this.word_dot_p);

        // --------------------
        // Date/time words
        this.add_module_word("AM", this.word_AM);
        this.add_module_word("PM", this.word_PM);
        this.add_module_word("NOW", this.word_NOW);
        this.add_module_word(">TIME", this.word_to_TIME);
        this.add_module_word("TIME>STR", this.word_TIME_to_STR);
        this.add_module_word(">DATE", this.word_to_DATE);
        this.add_module_word("TODAY", this.word_TODAY);
        this.add_module_word("MONDAY", this.word_MONDAY);
        this.add_module_word("TUESDAY", this.word_TUESDAY);
        this.add_module_word("WEDNESDAY", this.word_WEDNESDAY);
        this.add_module_word("THURSDAY", this.word_THURSDAY);
        this.add_module_word("FRIDAY", this.word_FRIDAY);
        this.add_module_word("SATURDAY", this.word_SATURDAY);
        this.add_module_word("SUNDAY", this.word_SUNDAY);
        this.add_module_word("+DAYS", this.word_plus_DAYS);
        this.add_module_word("SUBTRACT-DATES", this.word_SUBTRACT_DATES);
        this.add_module_word("DATE>STR", this.word_DATE_to_STR);
        this.add_module_word("DATE-TIME>DATETIME", this.word_DATE_TIME_to_DATETIME);
        this.add_module_word("DATETIME>TIMESTAMP", this.word_DATETIME_to_TIMESTAMP);
        this.add_module_word("TIMESTAMP>DATETIME", this.word_TIMESTAMP_to_DATETIME);
        this.add_module_word("STR>DATETIME", this.word_STR_to_DATETIME);

        // --------------------
        // Math words
        this.add_module_word("+", this.word_plus);
        this.add_module_word("-", this.word_minus);
        this.add_module_word("*", this.word_times);
        this.add_module_word("/", this.word_divide_by);
        this.add_module_word("MOD", this.word_MOD);
        this.add_module_word("ROUND", this.word_ROUND);
        this.add_module_word("==", this.word_equal_equal);
        this.add_module_word("!=", this.word_not_equal);
        this.add_module_word(">", this.word_greater_than);
        this.add_module_word(">=", this.word_greater_than_or_equal);
        this.add_module_word("<", this.word_less_than);
        this.add_module_word("<=", this.word_less_than_or_equal);
        this.add_module_word("OR", this.word_OR);
        this.add_module_word("AND", this.word_AND);
        this.add_module_word("NOT", this.word_NOT);
        this.add_module_word("IN", this.word_IN);
        this.add_module_word("ANY", this.word_ANY);
        this.add_module_word("ALL", this.word_ALL);
        this.add_module_word(">BOOL", this.word_to_BOOL);
        this.add_module_word(">INT", this.word_to_INT);
        this.add_module_word(">FLOAT", this.word_to_FLOAT);
        this.add_module_word("BUCKET", this.word_BUCKET);
        this.add_module_word("UNIFORM-RANDOM", this.word_UNIFORM_RANDOM);
        this.add_module_word("RANGE-INDEX", this.word_RANGE_INDEX);

        // --------------------
        // Ajax words (js-specific)
        this.add_module_word("AJAX-GET", this.word_AJAX_GET);

        // --------------------
        // Hash params words (js-specific)
        this.add_module_word("LOCATION", this.word_LOCATION);
        this.add_module_word("QUERY-PARAMS", this.word_QUERY_PARAMS);
        this.add_module_word("QUERY-PARAMS!", this.word_QUERY_PARAMS_bang);

        this.add_module_word("CONFIGURE-HASH-PARAMS", this.word_CONFIGURE_HASH_PARAMS);
        this.add_module_word("HASH-PARAMS", this.word_HASH_PARAMS);
        this.add_module_word("HASH-PARAMS!", this.word_HASH_PARAMS_bang);
        this.add_module_word("ON-HASH-CHANGE", this.word_ON_HASH_CHANGE);

        // --------------------
        // Misc words (js-specific)
        this.add_module_word("ENCODE-URI-COMPONENT", this.word_ENCODE_URI_COMPONENT);
        this.add_module_word("DECODE-URI-COMPONENT", this.word_DECODE_URI_COMPONENT);
        this.add_module_word("QUOTE-CHAR", this.word_QUOTE_CHAR);
        this.add_module_word("QUOTED", this.word_QUOTED);
        this.add_module_word("ASYNC-LOOP", this.word_ASYNC_LOOP);

        // --------------------
        // Profiling words
        this.add_module_word("PROFILE-START", this.word_PROFILE_START);
        this.add_module_word("PROFILE-TIMESTAMP", this.word_PROFILE_TIMESTAMP);
        this.add_module_word("PROFILE-END", this.word_PROFILE_END);
        this.add_module_word("PROFILE-DATA", this.word_PROFILE_DATA);
    }

    find_word(name) {
        let result = super.find_word(name);
        if (!result)   result = this.find_literal_word(name);
        return result;
    }

    find_literal_word(string) {
        let self = this;
        let value = null;
        for (let i=0; i < self.literal_handlers.length; i++) {
            value = self.literal_handlers[i](string);
            if (value !== null)   return new PushValueWord(string, value);
        }
        return null;
    }

    // =======================
    // Literal handlers
    to_bool(str_val) {
        if (str_val == "TRUE") return true;
        else if (str_val == "FALSE") return false;
        else return null;
    }

    to_int(str_val) {
        let result = parseInt(str_val);
        if (isNaN(result))       return null;
        if (result != str_val)   return null;
        return result;
    }

    to_float(str_val) {
        if (str_val.indexOf(".") == -1) return null;
        let result = parseFloat(str_val);
        if (isNaN(result)) return null;
        else return result;
    }

    to_date(str_val) {
        let match = str_val.match(/(\d{4})-(\d{2})-(\d{2})/);
        if (!match)   return null;
        let year = match[1];
        let month = match[2];
        let day = match[3];
        let result = new Date();
        result.setFullYear(year);
        result.setMonth(month-1);
        result.setDate(day);
        return result;
    }

    to_time(str_val) {
        let match = str_val.match(/(\d{1,2}):(\d{2})/);
        if (!match)   return null;

        let hours = match[1];
        let minutes = match[2];

        if (hours > 23)      return null;
        if (minutes >= 60)   return null;

        let result = new Date();
        result.setHours(hours);
        result.setMinutes(minutes);
        return result;
    }

    // =======================
    // Words

    // ( varnames -- )
    word_VARIABLES(interp) {
        let varnames = interp.stack_pop();
        let module = interp.cur_module();
        varnames.forEach(v => {
            module.add_variable(v);
        });
    }

    // ( value variable -- )
    word_bang(interp) {
        let variable = interp.stack_pop()
        let value = interp.stack_pop()
        variable.value = value
    }

    // ( variable -- value )
    word_at(interp) {
        let variable = interp.stack_pop()
        interp.stack_push(variable.value)
    }

    // ( value variable -- value )
    word_bang_at(interp) {
        let variable = interp.stack_pop()
        let value = interp.stack_pop()
        variable.value = value
        interp.stack_push(variable.value)
    }

    // ( string -- )
    async word_INTERPRET(interp) {
        let string = interp.stack_pop();
        if (string)   await interp.run(string);
    }


    // ( name forthic -- )
    async word_MEMO(interp) {
        let forthic = interp.stack_pop();
        let name = interp.stack_pop();
        let name_bang = name + "!";
        let var_name = `<memo_var_${name}>`;

        // Create variable
        await interp.run(`['${var_name}'] VARIABLES  NULL ${var_name} !`);

        // name! word
        await interp.run(": " + name_bang + "   " + forthic + " " + var_name + " ! ;");

        // name!@ word
        let name_bang_at = name + "!@"
        await interp.run(`: ${name_bang_at}   ${name_bang} ${var_name} @;`)

        // name word
        let word = new MemoWord(name, var_name);
        interp.cur_module().add_word(word);
    }

    // ( names -- )
    word_EXPORT(interp) {
        let names = interp.stack_pop();
        interp.cur_module().add_exportable(names);
    }

    // ( names -- )
    // ( names path -- )
    async word_USE_MODULES(interp) {
        let names;
        let path = interp.stack_pop();

        if (path instanceof Array) {  // Handle this case: ( names -- )
            names = path;
            path = "./modules";
        }
        else {                        // Handle this case: ( names path -- )
            names = interp.stack_pop();
        }
        let cur_module = interp.cur_module();
        if (cur_module != interp.app_module)
            throw "USE-MODULES can only be called within the app module";

        for (let i=0; i < names.length; i++) {
            let name = names[i];
            let module_name = name;
            let prefix = name;
            if (name instanceof Array) {
                module_name = name[0];
                prefix = name[1];
            }
            let module_filename = `${path}/${module_name}_module.mjs`;
            let download_module = await import(module_filename);
            await interp.register_module(download_module.new_module(interp))

            let module = interp.find_module(module_name);
            await interp.app_module.import_module(prefix, module, interp)
        }
    }

    // ( key_vals -- rec )
    word_REC(interp) {
        let key_vals = interp.stack_pop();
        if (!key_vals)   key_vals = [];
        let result = {};
        key_vals.forEach(pair => {
            let key = null;
            let val = null;
            if (pair) {
                if (pair.length >= 1)   key = pair[0];
                if (pair.length >= 2)   val = pair[1];
            }
            result[key] = val;
        });
        interp.stack_push(result);
    }

    // ( rec field -- value )
    // ( rec fields -- value )
    word_REC_at(interp) {
        let field = interp.stack_pop();
        let rec = interp.stack_pop();

        if (!rec) {
            interp.stack_push(null);
            return;
        }

        let fields = [field];
        if (field instanceof Array)
            fields = field;
        let result = drill_for_value(rec, fields);
        interp.stack_push(result);
    }

    // ( rec value field -- rec )
    word_l_REC_bang(interp) {
        let field = interp.stack_pop();
        let value = interp.stack_pop();
        let rec = interp.stack_pop();

        if (!rec)   rec = {};

        let fields = null;
        if (field instanceof Array)   fields = field;
        else                          fields = [field];

        function ensure_field(rec, field) {
            let res = rec[field];
            if (res === undefined) {
                res = {};
                rec[field] = res;
            }
            return res
        }

        let cur_rec = rec;
        // Drill down up until the last value
       for (let i = 0; i < fields.length - 1; i++) {
            cur_rec = ensure_field(cur_rec, fields[i]);
       }

        // Set the value at the right depth within rec
        cur_rec[fields[fields.length-1]] = value;

        interp.stack_push(rec);
    }


    // ( array item -- array )
    // ( record key/val -- record )
    word_APPEND(interp) {
        let item = interp.stack_pop();
        let result = interp.stack_pop();

        if (!result)   result = [];

        if (result instanceof Array)
            result.push(item);
        else    // If not a list, treat as record
            result[item[0]] = item[1]

        interp.stack_push(result);
    }

    // ( array -- array )
    // ( record -- record )
    word_REVERSE(interp) {
        let result = interp.stack_pop();

        if (!result) {
            interp.stack_push(result);
            return;
        }

        if (result instanceof Array)   result = result.reverse()

        interp.stack_push(result);
    }

    // ( array -- array )
    // ( record -- record )
    word_UNIQUE(interp) {
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(container);
            return;
        }

        function invert_rec(rec) {
            let res = {};
            Object.keys(rec).forEach(k => {
                res[rec[k]] = k;
            });
            return res;
        };

        let result = null;
        if (container instanceof Array) {
            let set = {};
            container.forEach(item => {
                set[item] = true;
            });
            result = Object.keys(set);
        }
        else {
            result = invert_rec(invert_rec(container));
        }

        interp.stack_push(result);
    }

    // ( array index -- array )
    // ( record key -- record )
    word_L_DEL(interp) {
        let key = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(container);
            return;
        }

        if (container instanceof Array)   container.splice(key, 1);
        else                              delete container[key];
        interp.stack_push(container);
    }


    // ( array old_keys new_keys -- array )
    // ( record old_keys new_keys -- record )
    word_RELABEL(interp) {
        let new_keys = interp.stack_pop();
        let old_keys = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(container);
            return;
        }

        if (old_keys.length != new_keys.length)
            throw("RELABEL: old_keys and new_keys must be same length");

        let new_to_old = {};
        for(let i=0; i < old_keys.length; i++) {
            new_to_old[new_keys[i]] = old_keys[i];
        }

        let result = [];
        if (container instanceof Array) {
            Object.keys(new_to_old).sort().forEach(k => result.push(container[new_to_old[k]]));
        }
        else {
            result = {};
            Object.keys(new_to_old).forEach(k => result[k] = container[new_to_old[k]]);
        }

        interp.stack_push(result);
    }

    // ( array field -- field_to_item )
    // ( record field -- field_to_item )
    word_BY_FIELD(interp) {
        let field = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let values = null;
        if (container instanceof Array) {
            values = container;
        }
        else {
            values = [];
            Object.keys(container).forEach(k => {
                values.push(container[k]);
            })
        }

        let result = {};
        values.forEach(v => {
            result[v[field]] = v;
        })

        interp.stack_push(result);
    }


    // ( array field -- field_to_items )
    // ( record field -- field_to_items )
    word_GROUP_BY_FIELD(interp) {
        let field = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let values = [];
        if (container instanceof Array)   values = container;
        else                              values = Object.keys(container).map(k => container[k]);

        let result = {};
        values.forEach(v => {
            let field_val = v[field];
            if (!result[field_val])   result[field_val] = [];
            result[field_val].push(v);
        });

        interp.stack_push(result)
    }

    // ( array forthic -- group_to_items )
    // ( record forthic -- group_to_items )
    async word_GROUP_BY(interp) {
        let forthic = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let values = [];
        if (container instanceof Array)   values = container;
        else                              values = Object.keys(container).map(k => container[k]);

        let result = {};
        for (let i=0; i < values.length; i++) {
            let v = values[i];
            interp.stack_push(v);
            await interp.run(forthic);
            let group = interp.stack_pop();
            if (!result[group])   result[group] = [];
            result[group].push(v);
        }

        interp.stack_push(result)
        return
    }

    // ( array forthic -- group_to_items )
    // ( record forthic -- group_to_items )
    async word_GROUP_BY_w_KEY(interp) {
        let forthic = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let keys, values;

        if (container instanceof Array) {
            keys = [];
            for (let i=0; i < container.length; i++)   keys.push(i);
            values = container;
        }
        else {
            keys = Object.keys(container);
            values = keys.map(k => container[k]);
        }

        let result = {};
        for(let i=0; i < values.length; i++) {
            let key = keys[i];
            let value = values[i];
            interp.stack_push(key)
            interp.stack_push(value)
            await interp.run(forthic)
            let group = interp.stack_pop();
            if (!result[group])   result[group] = [];
            result[group].push(value);
        }

        interp.stack_push(result);
    }

    // ( array n -- arrays )
    // ( record n -- records )
    word_GROUPS_OF(interp) {
        let size = interp.stack_pop();
        let container = interp.stack_pop();
        if (size <= 0) throw ("GROUPS-OF requires group size > 0");

        if (!container)   container = [];

        function group_items(items, group_size) {
            let num_groups = Math.ceil(items.length/group_size);
            let res = [];
            let remaining = items.slice();
            for (let i=0; i < num_groups; i++) {
                res.push(remaining.slice(0,group_size));
                remaining = remaining.slice(group_size);
            }

            return res;
        }

        function extract_rec(record, keys) {
            let res = {};
            keys.forEach(k => res[k] = record[k]);
            return res;
        }

        let result;
        if (container instanceof Array) {
            result = group_items(container, size);
        }
        else {
            let keys = Object.keys(container);
            let key_groups = group_items(keys, size);
            result = key_groups.map(ks => extract_rec(container, ks));
        }

        interp.stack_push(result)
        return
    }

    // ( array forthic -- record )
    async word_INDEX(interp) {
        const forthic = interp.stack_pop();
        const items = interp.stack_pop();

        if (!items) {
            interp.stack_push(items);
            return;
        }

        let result = {};
        for (let i=0; i < items.length; i++) {
            let item = items[i];
            interp.stack_push(item);
            await interp.run(forthic);
            let keys = interp.stack_pop();
            keys.forEach(k => {
                let lowercased_key = k.toLowerCase()
                if (result[lowercased_key])   result[lowercased_key].push(item)
                else                          result[lowercased_key] = [item]
            })
        }
        interp.stack_push(result)
    }

    // ( items word -- [ ? ] )
    async word_MAP(interp) {
        let string = interp.stack_pop();
        let items = interp.stack_pop();

        if (!items) {
            interp.stack_push(items);
            return;
        }

        let result = [];
        if (items instanceof Array) {
            for (let i=0; i < items.length; i++) {
                let item = items[i];
                interp.stack_push(item);
                await interp.run(string);
                let value = interp.stack_pop();
                result.push(value);
            }
        }
        else {
            result = {};
            let keys = Object.keys(items);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let item = items[k];
                interp.stack_push(item);
                await interp.run(string);
                let value = interp.stack_pop();
                result[k] = value;
            }
        }
        interp.stack_push(result)
    }

    // ( items word -- [ ? ] )
    async word_MAP_w_KEY(interp) {
        let string = interp.stack_pop();
        let items = interp.stack_pop();

        if (!items) {
            interp.stack_push(items);
            return;
        }

        let result = [];
        if (items instanceof Array) {
            for (let i=0; i < items.length; i++) {
                let item = items[i];
                interp.stack_push(i);
                interp.stack_push(item);
                await interp.run(string);
                let value = interp.stack_pop();
                result.push(value);
            };
        }
        else {
            result = {};
            let keys = Object.keys(items);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let item = items[k];
                interp.stack_push(k);
                interp.stack_push(item);
                await interp.run(string);
                let value = interp.stack_pop();
                result[k] = value;
            };
        }
        interp.stack_push(result)
    }

    // ( items word -- ? )
    async word_FOREACH(interp) {
        let string = interp.stack_pop()
        let items = interp.stack_pop()

        if (!items)   items = [];

        if (items instanceof Array) {
            for (let i=0; i < items.length; i++) {
                let item = items[i];
                interp.stack_push(item);
                await interp.run(string);
            };
        }
        else {
            let keys = Object.keys(items);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let item = items[k];
                interp.stack_push(item);
                await interp.run(string);
            };
        }
    }

    // ( items word -- ? )
    async word_FOREACH_w_KEY(interp) {
        let string = interp.stack_pop()
        let items = interp.stack_pop()

        if (!items)   items = [];

        if (items instanceof Array) {
            for (let i=0; i < items.length; i++) {
                let item = items[i];
                interp.stack_push(i);
                interp.stack_push(item);
                await interp.run(string);
            };
        }
        else {
            let keys = Object.keys(items);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let item = items[k];
                interp.stack_push(k);
                interp.stack_push(item);
                await interp.run(string);
            };
        }
    }

    // ( array1 array2 -- array )
    // ( record1 record2 -- record )
    word_ZIP(interp) {
        let container2 = interp.stack_pop();
        let container1 = interp.stack_pop();

        if (!container1)   container1 = [];
        if (!container2)   container2 = [];

        let result;
        if (container2 instanceof Array) {
            result = [];
            for (let i=0; i < container1.length; i++) {
                let value2 = null;
                if (i < container2.length)   value2 = container2[i];
                result.push([container1[i], value2]);
            }
        }
        else {
            result = {};
            Object.keys(container1).forEach(k => {
                let v = container1[k];
                result[k] = [v, container2[k]];
            });
        }

        interp.stack_push(result);
    }


    // ( array1 array2 forthic -- array )
    // ( record1 record2 forthic -- record )
    async word_ZIP_WITH(interp) {
        let forthic = interp.stack_pop();
        let container2 = interp.stack_pop();
        let container1 = interp.stack_pop();

        if (!container1)   container1 = [];
        if (!container2)   container2 = [];

        let result;
        if (container2 instanceof Array) {
            result = [];
            for (let i=0; i < container1.length; i++) {
                let value2 = null;
                if (i < container2.length)   value2 = container2[i];
                interp.stack_push(container1[i]);
                interp.stack_push(value2);
                await interp.run(forthic);
                let res = interp.stack_pop();
                result.push(res);
            }
        }
        else {
            result = {};
            let keys = Object.keys(container1);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                interp.stack_push(container1[k]);
                interp.stack_push(container2[k]);
                await interp.run(forthic);
                let res = interp.stack_pop();
                result[k] = res;
            }
        }

        interp.stack_push(result);
    }


    // ( array -- array )
    // ( record -- array )
    word_KEYS(interp) {
        let container = interp.stack_pop();

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            result = [];
            for (let i=0; i < container.length; i++)    result.push(i);
        }
        else {
            result = Object.keys(container);
        }

        interp.stack_push(result)
    }

    // ( array -- array )
    // ( record -- array )
    word_VALUES(interp) {
        let container = interp.stack_pop();

        if (!container) container = [];

        let result;
        if (container instanceof Array) {
            result = container;
        }
        else {
            result = [];
            Object.keys(container).forEach(k => result.push(container[k]));
        }

        interp.stack_push(result)
    }

    // ( array -- length )
    // ( record -- length )
    word_LENGTH(interp) {
        let container = interp.stack_pop()

        if (!container)   container = [];

        let result;
        if (container instanceof Array || typeof(container) == 'string') {
            result = container.length;
        }
        else {
            result = Object.keys(container).length;
        }

        interp.stack_push(result)
    }

    // ( array start end -- array )
    // ( record start end -- record )
    word_SLICE(interp) {
        let end = Math.trunc(interp.stack_pop());
        let start = Math.trunc(interp.stack_pop());
        let container = interp.stack_pop();

        if (!container)   container = [];

        let length;
        if (container instanceof Array) {
            length = container.length;
        }
        else {
            length = Object.keys(container).length;
        }

        function normalize_index(index) {
           let res = index;
           if (index < 0)   res = index + length;
           return res;
        }

        start = normalize_index(start);
        end = normalize_index(end);

        let step = 1;
        if (start > end)   step = -1;

        let indexes = [start];
        if (start < 0 || start >= length)   indexes = []

        while (start != end) {
            start = start + step;
            if (start < 0 || start >= length)   indexes.push(null);
            else                                indexes.push(start);
        }

        let result;
        if (container instanceof Array) {
            result = [];
            indexes.forEach(i => {
                if (i === null)   result.push(null);
                else              result.push(container[i]);
            })
        }
        else {
            let keys = Object.keys(container).sort();
            result = {};
            indexes.forEach(i => {
                if (i !== null) {
                    let k = keys[i];
                    result[k] = container[k];
                }
            });
        }

        interp.stack_push(result);
    }


    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    word_DIFFERENCE(interp) {
        let rcontainer = interp.stack_pop();
        let lcontainer = interp.stack_pop();

        if (!lcontainer)   lcontainer = [];
        if (!rcontainer)   rcontainer = [];

        function difference(l, r) {
            let res = [];
            l.forEach(item => {
                if (r.indexOf(item) < 0)   res.push(item);
            });
            return res;
        }

        let result;
        if (rcontainer instanceof Array) {
            result = difference(lcontainer, rcontainer);
        }
        else {
            let lkeys = Object.keys(lcontainer);
            let rkeys = Object.keys(rcontainer);

            let diff = difference(lkeys, rkeys);
            result = {};
            diff.forEach(k => result[k] = lcontainer[k]);
        }

        interp.stack_push(result)
    }

    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    word_INTERSECTION(interp) {
        let rcontainer = interp.stack_pop()
        let lcontainer = interp.stack_pop()

        if (!lcontainer)   lcontainer = [];
        if (!rcontainer)   rcontainer = [];

        function intersection(l, r) {
            let res = [];
            l.forEach(item => {
                if (r.indexOf(item) >= 0)   res.push(item);
            });
            return res;
        }

        let result;
        if (rcontainer instanceof Array) {
            result = intersection(lcontainer, rcontainer);
        }
        else {
            let lkeys = Object.keys(lcontainer);
            let rkeys = Object.keys(rcontainer);

            let intersect = intersection(lkeys, rkeys);
            result = {};
            intersect.forEach(k => result[k] = lcontainer[k]);
        }
        interp.stack_push(result);
    }

    // ( larray rarray -- array )
    // ( lrecord rrecord -- record )
    word_UNION(interp) {
        let rcontainer = interp.stack_pop();
        let lcontainer = interp.stack_pop();

        if (!lcontainer)   lcontainer = [];
        if (!rcontainer)   rcontainer = [];

        function union(l, r) {
            let keyset = {};
            l.forEach(item => {
                keyset[item] = true;
            });
            r.forEach(item => {
                keyset[item] = true;
            });
            let res = Object.keys(keyset);
            return res;
        }

        let result;
        if (rcontainer instanceof Array) {
            result = union(lcontainer, rcontainer);
        }
        else {
            let lkeys = Object.keys(lcontainer);
            let rkeys = Object.keys(rcontainer);

            let keys = union(lkeys, rkeys);
            result = {};
            keys.forEach(k => {
                let val = lcontainer[k];
                if (val === undefined)   val = rcontainer[k];
                result[k] = val;
            });
        }

        interp.stack_push(result)
    }


    // ( larray forthic -- array )
    // ( lrecord forthic -- record )
    async word_SELECT(interp) {
        let forthic = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(container);
            return;
        }

        let result;
        if (container instanceof Array) {
            result = [];
            for (let i=0; i < container.length; i++) {
                let item = container[i];
                interp.stack_push(item);
                await interp.run(forthic);
                let should_select = interp.stack_pop();
                if (should_select)   result.push(item);
            }
        }
        else {
            result = {};
            let keys = Object.keys(container);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let v = container[k];
                interp.stack_push(v);
                await interp.run(forthic);
                let should_select = interp.stack_pop();
                if (should_select)   result[k] = v;
            }
        }

        interp.stack_push(result);
    }


    // ( larray forthic -- array )
    // ( lrecord forthic -- record )
    async word_SELECT_w_KEY(interp) {
        let forthic = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(container);
            return;
        }

        let result;
        if (container instanceof Array) {
            result = [];
            for (let i=0; i < container.length; i++) {
                let item = container[i];
                interp.stack_push(i);
                interp.stack_push(item);
                await interp.run(forthic);
                let should_select = interp.stack_pop();
                if (should_select)   result.push(item);
            };
        }
        else {
            result = {};
            let keys = Object.keys(container);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let v = container[k];
                interp.stack_push(k);
                interp.stack_push(v);
                await interp.run(forthic);
                let should_select = interp.stack_pop();
                if (should_select)   result[k] = v;
            };
        }

        interp.stack_push(result);
    }


    // ( array n -- rest taken )
    // ( record n -- rest taken )
    word_TAKE(interp) {
        let n = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let rest, taken;
        if (container instanceof Array) {
            taken = container.slice(0, n);
            rest = container.slice(n);
        }
        else {
            let keys = Object.keys(container).sort();
            let taken_keys = keys.slice(0, n);
            let rest_keys = keys.slice(n);
            taken = taken_keys.map(k => container[k]);
            rest = rest_keys.map(k => container[k]);
        }

        interp.stack_push(rest);
        interp.stack_push(taken);
    }

    // ( array n -- array )
    // ( record n -- record )
    word_DROP(interp) {
        let n = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            result = container.slice(n);
        }
        else {
            let keys = Object.keys(container).sort();
            let rest_keys = keys.slice(n);
            result = rest_keys.map(k => container[k]);
        }

        interp.stack_push(result);
    }

    // ( array  -- array )
    // ( record  -- record )
    word_ROTATE(interp) {
        let container = interp.stack_pop();

        let result;
        if (!container) {
            result = container;
        }
        else if (container instanceof Array) {
            result = container;
            if (container.length > 0)  {
                let val = result.pop();
                result.unshift(val);
            }
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }

    // ( array element -- array )
    // ( record element -- record )
    // Moves element to front of array
    word_ROTATE_ELEMENT(interp) {
        let element = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            let index = container.indexOf(element);
            result = container;
            if (index > 0) {
               result.splice(index, 1);
               result.unshift(element);
            }
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }

    // ( array -- array )
    // ( record -- record )
    word_SHUFFLE(interp) {
        let container = interp.stack_pop();

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            // See: https://medium.com/@nitinpatel_20236/
            //    how-to-shuffle-correctly-shuffle-an-array-in-javascript-15ea3f84bfb
            result = container;
            for (let i=result.length-1; i > 0; i--) {
              const j = Math.floor(Math.random() * i);
              const temp = result[i];
              result[i] = result[j];
              result[j] = temp;
            }
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }

    // ( array -- array )
    // ( record -- record )
    word_SORT(interp) {
        let container = interp.stack_pop();

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            result = container;
            result.sort();
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }


    // ( array forthic -- array )
    // ( record forthic -- record )
    async word_SORT_w_FORTHIC(interp) {
        let forthic = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        async function make_aug_array(vals) {
            let res = [];
            for (let i=0; i < vals.length; i++) {
                let val = vals[i];
                interp.stack_push(val);
                await interp.run(forthic);
                let aug_val = interp.stack_pop();
                res.push([val, aug_val]);
            }
            return res;
        }

        function cmp_items(l, r) {
            let l_val = l[1];
            let r_val = r[1];

            if (l_val < r_val)       return -1;
            else if (l_val > r_val)  return 1;
            else                     return 0;
        }

        function de_aug_array(aug_vals) {
            let res = aug_vals.map(aug_val => aug_val[0]);
            return res;
        }

        let result;
        if (container instanceof Array) {
            let aug_array = await make_aug_array(container);
            aug_array.sort(cmp_items);
            result = de_aug_array(aug_array);
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }

    // ( field -- key_func )
    word_FIELD_KEY_FUNC(interp) {
        let field = interp.stack_pop();

        function result(record) {
            return record[field];
        }

        interp.stack_push(result)
    }

    // ( array key_func -- array )
    // ( record key_func -- record )
    word_SORT_w_KEY_FUNC(interp) {
        let key_func = interp.stack_pop();
        let container = interp.stack_pop();

        if (!container)   container = [];

        function cmp_items(l, r) {
            let l_val = key_func(l);
            let r_val = key_func(r);
            if (l_val < r_val)       return -1;
            else if (l_val > r_val)  return 1;
            else                     return 0;
        }

        let result;
        if (container instanceof Array) {
            result = container;
            result.sort(cmp_items);
        }
        else {
            result = container
        }

        interp.stack_push(result);
    }

    // ( array n -- item )
    // ( record n -- value )
    word_NTH(interp) {
        let n = interp.stack_pop();
        let container = interp.stack_pop();

        if (n === null || !container) {
            interp.stack_push(null);
            return;
        }

        let result;
        if (container instanceof Array) {
            if (n < 0 || n >= container.length) {
                interp.stack_push(null);
                return;
            }
            result = container[n];
        }
        else {
            if (n < 0 || n >= Object.keys(container).length) {
                interp.stack_push(null);
                return;
            }
            let keys = Object.keys(container).sort();
            let key = keys[n];
            result = container[key];
        }

        interp.stack_push(result);
    }

    // ( array -- item )
    // ( record -- value )
    word_LAST(interp) {
        let container = interp.stack_pop();

        if (!container) {
            interp.stack_push(null);
            return;
        }

        let result;
        if (container instanceof Array) {
            if (container.length == 0)   result = null;
            else                         result = container[container.length-1];
        }
        else {
            let keys = Object.keys(container).sort();
            if (keys.length == 0)   result = null;
            else                    result = container[keys[keys.length-1]];
        }

        interp.stack_push(result);
    }

    // ( array -- a1 a2 .. an )
    // ( record -- v1 v2 .. vn )
    word_UNPACK(interp) {
        let container = interp.stack_pop()

        if (!container)   container = [];

        if (container instanceof Array) {
            container.forEach(item => {
                interp.stack_push(item);
            });
        }
        else {
            let keys = Object.keys(container).sort();
            keys.forEach(k => {
                interp.stack_push(container[k]);
            });
        }
    }

    // ( array -- array )
    // ( record -- record )
    word_FLATTEN(interp) {
        let nested = interp.stack_pop()

        if (!nested)   nested = [];

        function flatten_array(items, accum=[]) {
            items.forEach(i => {
                if (i instanceof Array) flatten_array(i, accum);
                else accum.push(i);
            });
            return accum;
        }

        function is_record(obj) {
            let keys = Object.keys(obj);
            return keys.length > 0;
        }

        function flatten_record(record, res={}, keys=[]) {
            Object.keys(record).forEach(k => {
                let v = record[k];
                if (is_record(v)) {
                    flatten_record(v, res, keys.concat([k]));
                }
                else {
                    let key = keys.concat([k]).join("\t");
                    res[key] = v;
                }
            });
            return res;
        }

        let result;
        if (nested instanceof Array) {
            result = flatten_array(nested);
        }
        else {
            result = flatten_record(nested);
        }

        interp.stack_push(result)
        return
    }

    // ( array item -- index )
    // ( record item -- key )
    word_KEY_OF(interp) {
        let item = interp.stack_pop()
        let container = interp.stack_pop()

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            let index = container.indexOf(item);
            if (index < 0)   result = null;
            else             result = index;
        }
        else {
            result = null;
            let keys = Object.keys(container);
            for (let i=0; i < keys.length; i++) {
                let k = keys[i];
                let v = container[k];
                if (v == item) {
                    result = k;
                    break;
                }
            };
        }

        interp.stack_push(result)
        return
    }


    // ( array init forthic -- value )
    // ( record init forthic -- value )
    async word_REDUCE(interp) {
        let forthic = interp.stack_pop()
        let initial = interp.stack_pop()
        let container = interp.stack_pop()

        if (!container)   container = [];

        let result;
        if (container instanceof Array) {
            interp.stack_push(initial);
            container.forEach(async (item) => {
                interp.stack_push(item);
                await interp.run(forthic);
            });
            result = interp.stack_pop()
        }
        else {
            interp.stack_push(initial);
            Object.keys(container).forEach(async (k) => {
                let v = container[k];
                interp.stack_push(v);
                await interp.run(forthic);
            });
            result = interp.stack_pop();
        }

        interp.stack_push(result)
    }


    // ( a -- )
    word_POP(interp) {
        interp.stack_pop();
    }

    // ( a -- a a )
    word_DUP(interp) {
        let a = interp.stack_pop();
        interp.stack_push(a);
        interp.stack_push(a);
    }


    // ( a b -- b a )
    word_SWAP(interp) {
        let b = interp.stack_pop();
        let a = interp.stack_pop();
        interp.stack_push(b);
        interp.stack_push(a);
    }

    // ( item -- str )
    word_to_STR(interp) {
        let item = interp.stack_pop();
        interp.stack_push(item.toString());
    }

    // ( str1 str2 -- str )
    // ( array_of_str -- str )
    word_CONCAT(interp) {
        let str2 = interp.stack_pop();
        let array;
        if (str2 instanceof Array) {
            array = str2;
        }
        else {
            let str1 = interp.stack_pop();
            array = [str1, str2];
        }

        let result = array.join("");
        interp.stack_push(result);
    }

    // ( string sep -- items )
    word_SPLIT(interp) {
        let sep = interp.stack_pop();
        let string = interp.stack_pop();

        if (!string)   string = ""

        let result = string.split(sep);
        interp.stack_push(result);
    }

    // ( strings sep -- string )
    word_JOIN(interp) {
        let sep = interp.stack_pop();
        let strings = interp.stack_pop();

        if (!strings)   strings = [];

        let result = strings.join(sep);
        interp.stack_push(result);
    }

    // ( -- char )
    word_slash_N(interp) {
        interp.stack_push('\n');
    }

    // ( -- char )
    word_slash_R(interp) {
        interp.stack_push('\r');
    }

    // ( -- char )
    word_slash_T(interp) {
        interp.stack_push('\t');
    }

    // ( A -- a )
    word_pipe_LOWER(interp) {
        let string = interp.stack_pop();
        let result = string.toLowerCase();
        interp.stack_push(result);
    }

    // ( a -- A )
    word_pipe_UPPER(interp) {
        let string = interp.stack_pop();
        let result = string.toUpperCase();
        interp.stack_push(result);
    }

    // ( string -- string )
    word_pipe_ASCII(interp) {
        let string = interp.stack_pop();
        let result = "";
        for (let i=0; i < string.length; i++) {
            let ch = string[i];
            if (ch.charCodeAt(0) < 256)   result += ch;
        }
        interp.stack_push(result);
    }


    // ( str -- str )
    word_STRIP(interp) {
        let string = interp.stack_pop();
        let result = string;
        if (result)   result = result.trim();
        interp.stack_push(result);
    }

    // ( string text replace -- string )
    word_REPLACE(interp) {
        let replace = interp.stack_pop();
        let text = interp.stack_pop();
        let string = interp.stack_pop();

        let result = string;
        if (string) {
            let pattern = new RegExp(text, "g");
            result = string.replace(pattern, replace);
        }
        interp.stack_push(result);
    }

    // ( string pattern -- match )
    word_RE_MATCH(interp) {
        let pattern = interp.stack_pop();
        let string = interp.stack_pop();

        let re_pattern = new RegExp(pattern);
        let result = false;
        if (string !== null)   result = string.match(re_pattern);
        interp.stack_push(result);
    }


    // ( string pattern -- matches )
    word_RE_MATCH_ALL(interp) {
        let pattern = interp.stack_pop();
        let string = interp.stack_pop();

        let re_pattern = new RegExp(pattern, 'g');
        let matches = [];
        if (string !== null)   matches = string.matchAll(re_pattern);
        let result = Array.from(matches).map(v => v[1]);

        interp.stack_push(result);
    }


    // ( match num -- string )
    word_RE_MATCH_GROUP(interp) {
        let num = interp.stack_pop();
        let match = interp.stack_pop();
        let result = null;
        if (match)   result = match[num]
        interp.stack_push(result)
    }


    // ( time -- time )
    word_AM(interp) {
        let time = interp.stack_pop();
        if (! time instanceof Date)   throw("AM expecting a time");

        let result = time;
        if (time.getHours() >= 12) {
            result = new Date();
            result.setHours(time.getHours() - 12);
            result.setMinutes(time.getMinutes());
        }
        interp.stack_push(result);
    }

    // ( time -- time )
    word_PM(interp) {
        let time = interp.stack_pop();
        if (! time instanceof Date)   throw("PM expecting a time");

        let result = time;
        if (time.getHours() < 12) {
            result = new Date();
            result.setHours(time.getHours() + 12);
            result.setMinutes(time.getMinutes());
        }
        interp.stack_push(result);
    }

    // ( -- time )
    word_NOW(interp) {
        let result = new Date();
        interp.stack_push(result);
    }

    // ( item -- time )
    word_to_TIME(interp) {
        let item = interp.stack_pop();
        let result;
        if (item instanceof Date) {
            result = item;
        }
        else {
            // NB: We need a date in order for Date.parse to succeed. Also assuming str is a time
            let date_string = "Jan 1, 2000 " + item;
            result = new Date(Date.parse(date_string));
        }

        interp.stack_push(result);
    }

    // ( time -- str )
    word_TIME_to_STR(interp) {
        let time = interp.stack_pop();
        let result = time.getHours() + ":" + time.getMinutes();
        interp.stack_push(result);
    }

    // ( str -- date )
    word_to_DATE(interp) {
        let s = interp.stack_pop();
        function isValidDate(date) {
            return s != null && date instanceof Date && !isNaN(date);
        }

        let result;
        if (isValidDate(s)){
            result = s;
        }
        else {
            result = new Date(s);
            if (!isValidDate(result))   result = null;
        }
        interp.stack_push(result);
    }

    // ( -- date )
    word_TODAY(interp) {
        interp.stack_push(new Date());
    }

    // ( -- date )
    word_MONDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(0));
    }

    // ( -- date )
    word_TUESDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(1));
    }

    // ( -- date )
    word_WEDNESDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(2));
    }

    // ( -- date )
    word_THURSDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(3));
    }

    // ( -- date )
    word_FRIDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(4));
    }

    // ( -- date )
    word_SATURDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(5));
    }

    // ( -- date )
    word_SUNDAY(interp) {
        interp.stack_push(GlobalModule.get_day_this_week(6));
    }


    static get_day_this_week(day_of_week) {
        // NOTE: Monday is start of week
        function normalize_day(day) {
            if (day == 0)   return 6;  // Sunday maps to 6
            else            return day - 1;
        }
        let today = new Date();
        let delta_days = (day_of_week - normalize_day(today.getDay())) % 7
        if (day_of_week < today.getDay())   delta_days -= 7

        let result = today;
        result.setDate(result.getDate() + delta_days);
        return result
    }

    // ( date num-days -- date )
    word_plus_DAYS(interp) {
        let num_days = interp.stack_pop();
        let date = interp.stack_pop();

        let result = new Date(date);
        result.setDate(result.getDate() + num_days);
        interp.stack_push(result);
    }

    // ( l_date r_date -- num_days )
    word_SUBTRACT_DATES(interp) {
        let r_date = interp.stack_pop();
        let l_date = interp.stack_pop();
        let ms_per_day = 1000*60*60*24;
        let result = Math.round((l_date.getTime() - r_date.getTime())/ms_per_day);
        interp.stack_push(result);
    }

    // ( date -- str )
    word_DATE_to_STR(interp) {
        let date = interp.stack_pop();
        if (date instanceof Date)   interp.stack_push(date.toLocaleDateString());
        else                        interp.stack_push("");
    }

    // ( date time -- datetime )
    word_DATE_TIME_to_DATETIME(interp) {
        let time = interp.stack_pop()
        let date = interp.stack_pop()
        let dt_string = `${date.getFullYear()}-${date.getMonth()+1}-${date.getDate()} ${time.getHours()}:${time.getMinutes()}`;
        let result = new Date(dt_string);
        interp.stack_push(result)
    }

    // ( datetime -- timestamp )
    word_DATETIME_to_TIMESTAMP(interp) {
        let datetime = interp.stack_pop()
        let result = Math.round(datetime.getTime()/1000);
        interp.stack_push(result)
    }

    // ( timestamp -- datetime )
    word_TIMESTAMP_to_DATETIME(interp) {
        let timestamp = interp.stack_pop()
        let result = new Date(timestamp*1000);
        interp.stack_push(result)
    }

    // ( str -- datetime )
    word_STR_to_DATETIME(interp) {
        let s = interp.stack_pop();
        result = new Date(s);
        interp.stack_push(result)
    }

    // ( a b -- a+b )
    // ( items -- sum )
    word_plus(interp) {
        let items = interp.stack_pop();

        if (items instanceof Array) {
            let sum = 0;
            items.forEach(item => {sum += item});
            interp.stack_push(sum);
        }
        else {
            let b = items;
            let a = interp.stack_pop();
            interp.stack_push(a+b);
        }
    }

    // ( a b -- a*b )
    word_times(interp) {
        let b = interp.stack_pop();
        let a = interp.stack_pop();
        interp.stack_push(a*b);
    }

    // ( a b -- a/b )
    word_divide_by(interp) {
        let b = interp.stack_pop();
        let a = interp.stack_pop();
        interp.stack_push(a/b);
    }

    // ( a b -- res )
    word_MOD(interp) {
        let b = interp.stack_pop();
        let a = interp.stack_pop();
        interp.stack_push(a % b);
    }

    // ( num -- int )
    word_ROUND(interp) {
        let num = interp.stack_pop();
        interp.stack_push(Math.round(num))
    }

    // ( l r -- bool )
    word_equal_equal(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();
        interp.stack_push(l == r);
    }

    // ( l r -- bool )
    word_not_equal(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();
        interp.stack_push(l != r);
    }

    // ( l r -- bool )
    word_greater_than(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();

        if (l === null || r === null) {
            interp.stack_push(null);
            return;
        }

        interp.stack_push(l > r);
    }

    // ( l r -- bool )
    word_greater_than_or_equal(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();

        if (l === null || r === null) {
            interp.stack_push(null);
            return;
        }

        interp.stack_push(l >= r);
    }

    // ( l r -- bool )
    word_less_than(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();

        if (l === null || r === null) {
            interp.stack_push(null);
            return;
        }

        interp.stack_push(l < r);
    }

    // ( l r -- bool )
    word_less_than_or_equal(interp) {
        let r = interp.stack_pop();
        let l = interp.stack_pop();

        if (l === null || r === null) {
            interp.stack_push(null);
            return;
        }

        interp.stack_push(l <= r);
    }

    // ( l r -- bool )
    // ( items -- bool )
    word_OR(interp) {
        let r = interp.stack_pop();

        let items;
        if (r instanceof Array) {
            items = r;
        }
        else {
            let l = interp.stack_pop();
            items = [l, r];
        }
        let result = items.some(item => item);
        interp.stack_push(result);
    }

    // ( l r -- bool )
    // ( items -- bool )
    word_AND(interp) {
        let r = interp.stack_pop();

        let items;
        if (r instanceof Array) {
            items = r;
        }
        else {
            let l = interp.stack_pop();
            items = [l, r];
        }
        let result = items.every(item => item);
        interp.stack_push(result);
    }

    // ( bool -- bool )
    word_NOT(interp) {
        let value = interp.stack_pop();
        interp.stack_push(!value);
    }

    // ( item items -- bool )
    word_IN(interp) {
        let items = interp.stack_pop();
        let item = interp.stack_pop();
        if (!items)   items = [];
        let result = items.indexOf(item) >= 0;
        interp.stack_push(result)
    }

    // ( vals required_vals -- bool )
    word_ANY(interp) {
        let required_vals = interp.stack_pop()
        let vals = interp.stack_pop()

        let result = false
        for(let i=0; i < required_vals.length; i++) {
            let rv = required_vals[i];
            if (vals.indexOf(rv) >= 0) {
                result = true;
                break;
            }
        }

        // If nothing is required, then all values are true
        if (required_vals.length == 0)   result = true;

        interp.stack_push(result);
    }

    // ( vals required_vals -- bool )
    word_ALL(interp) {
        let required_vals = interp.stack_pop()
        let vals = interp.stack_pop()

        if (!vals)   vals = [];
        if (!required_vals)   required_vals = [];

        let result = required_vals.every(val => vals.indexOf(val) >= 0);
        interp.stack_push(result);
    }

    // ( item -- bool )
    word_to_BOOL(interp) {
        let item = interp.stack_pop();
        let result = !!item;
        interp.stack_push(result);
    }

    // ( item -- int )
    word_to_INT(interp) {
        let str = interp.stack_pop();
        let result = parseInt(str);
        interp.stack_push(result);
    }

    // ( item -- int )
    word_to_FLOAT(interp) {
        let str = interp.stack_pop();
        let result = parseFloat(str);
        interp.stack_push(result);
    }

    // ( num buckets -- bucket )
    // Each bucket has three elements: [low high value]. If num is >= low and < high, it's in the bucket
    // If a number isn't in any bucket, null is returned
    word_BUCKET(interp) {
        let buckets = interp.stack_pop();
        let num = interp.stack_pop();

        let result = null;
        for (let i=0; i < buckets.length; i++) {
            let low = buckets[i][0];
            let high = buckets[i][1];
            let value = buckets[i][2];

            if (num >= low && num < high) {
                result = value;
                break;
            }
        }
        if (num == undefined)   result = "";
        interp.stack_push(result);
    }

    // ( low high -- int )
    word_UNIFORM_RANDOM(interp) {
        let high = interp.stack_pop();
        let low = interp.stack_pop();

        // From: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
        function getRandomIntInclusive(min, max) {
            min = Math.ceil(min);
            max = Math.floor(max);
            return Math.floor(Math.random() * (max - min + 1) + min); //The maximum is inclusive and the minimum is inclusive
        }
        let result = getRandomIntInclusive(low, high);
        interp.stack_push(result);
    }

    // ( val start_ranges -- index )
    word_RANGE_INDEX(interp) {
        let start_ranges = interp.stack_pop();
        let val = interp.stack_pop();

        // Cap off the value ranges with infinity
        start_ranges.push(Infinity);

        if (val === null ||  !start_ranges) {
            interp.stack_push(null);
            return;
        }

        if (val < start_ranges[0]) {
            interp.stack_push(null);
            return;
        }

        result = null;
        for (let i=0; i < start_ranges.length-1; i++) {
            if (val >= start_ranges[i] && val < start_ranges[i + 1]) {
                result = i;
                break;
            }
        }

        interp.stack_push(result);
    }

    // ( url -- response )
    word_AJAX_GET(interp) {
        let url = interp.stack_pop();
        $.ajax({
          async:false,
          url: url,
        })
        .done(function(res) {
          interp.stack_push(res);
        })
        .fail(function(err) {
          interp.stack_push(err);
        });
    }

    // ( -- location_rec )
    word_LOCATION(interp) {
        interp.stack_push(location)
    }

    // ( -- query_params_rec )
    word_QUERY_PARAMS(interp) {
        let search = location.search.slice(1);

        let result = {};
        if (!search) {
            interp.stack_push(result);
            return;
        }

        let params = search.split("&");
        params.forEach(p => {
            let pair = p.split('=')
            let key = pair[0];
            let value = pair[1];
            // NOTE: We don't handle the case where there are duplicate keys
            result[key] = value;
        })
        interp.stack_push(result);
    }

    // ( query_params_rec -- )
    word_QUERY_PARAMS_bang(interp) {
        let query_params_rec = interp.stack_pop();

        function to_query_string(rec) {
            let res = Object.keys(rec).map(key => {
                return `${key}=${encodeURIComponent(rec[key])}`;
            }).join("&");
            return res;
        }
        let query_string = to_query_string(query_params_rec);
        let new_location = location.origin + location.pathname + '?' + query_string;
        window.location = new_location;
    }

    // ( config -- )
    // config is an array of arrays with the following info:
    //    [ getter_name query_param default_val getter_transform setter_transform ]
    //        getter_name:   The name of the Forthic word that will be created to get the query param.
    //                       The setter name will be <getter_name>!
    //        query_param:   The query parameter in question
    //        default_val (optional):       Default value of query param
    //        getter_transform (optional):  A Forthic string that's run on the value before returning via the getter
    //        setter_transform (optional):  A Forthic string that's run on the value before setting it to the query string
    async word_CONFIGURE_HASH_PARAMS(interp) {
        let config = interp.stack_pop()

        await interp.run("HASH-PARAMS");
        let hash_params = interp.stack_pop();

        async function create_getter(getter_name, query_param) {
            await interp.run(`: ${getter_name}   HASH-PARAMS '${query_param}' REC@ { ; }`);
        }

        async function create_getter_w_transform(getter_name, getter_transform, query_param) {
            await interp.run(`: ${getter_name}   HASH-PARAMS '${query_param}' REC@ ${getter_transform} { ; }`);
        }

        async function create_setter(getter_name, query_param) {
            // ( value -- )
            await interp.run(`: ${getter_name}!   HASH-PARAMS SWAP '${query_param}' <REC! HASH-PARAMS! { ; }`);
        }

        async function create_setter_w_transform(getter_name, setter_transform, query_param) {
            // ( value -- )
            await interp.run(`: ${getter_name}!   HASH-PARAMS SWAP ${setter_transform} '${query_param}' <REC! HASH-PARAMS! { ; }`);
        }

        function apply_default_if_needed(query_param, default_val) {
            if (hash_params[query_param] == undefined)   hash_params[query_param] = default_val;
        }

        for (let i=0; i < config.length; i++) {
            let row = config[i];
            let getter_name, query_param, default_val, getter_transform, setter_transform;

            switch(row.length) {
                case 2:
                    getter_name = row[0];
                    query_param = row[1];
                    await create_getter(getter_name, query_param);
                    await create_setter(getter_name, query_param);
                    break;

                case 3:
                    getter_name = row[0];
                    query_param = row[1];
                    default_val = row[2];
                    await create_getter(getter_name, query_param);
                    await create_setter(getter_name, query_param);
                    apply_default_if_needed(query_param, default_val);
                    break;

                case 4:
                    getter_name = row[0];
                    query_param = row[1];
                    default_val = row[2];
                    getter_transform = row[3];
                    await create_getter_w_transform(getter_name, getter_transform, query_param);
                    await create_setter(getter_name, query_param);
                    apply_default_if_needed(query_param, default_val);
                    break;

                case 5:
                    getter_name = row[0];
                    query_param = row[1];
                    default_val = row[2];
                    getter_transform = row[3];
                    setter_transform = row[4];
                    await create_getter_w_transform(getter_name, getter_transform, query_param);
                    await create_setter_w_transform(getter_name, setter_transform, query_param);
                    apply_default_if_needed(query_param, default_val);
                    break;

                default:
                    throw(`CONFIGURE-HASH-PARAMS: Invalid data ${row}`);
            }
        }

        // Apply hash params
        interp.stack_push(hash_params);
        await interp.run("HASH-PARAMS!");
    }

    // ( -- params )
    word_HASH_PARAMS(interp) {
        let hash = window.location.hash.slice(1);
        let pieces = hash.split("&");
        let key_vals = pieces.map(piece => piece.split("="));
        let result = {};
        key_vals.forEach(kv => result[kv[0]] = kv[1])
        interp.stack_push(result);
    }

    // (params -- )
    word_HASH_PARAMS_bang(interp) {
        let params = interp.stack_pop();
        let pieces = [];
        Object.keys(params).forEach(key => {
            if (key) {
                let val = params[key];
                pieces.push(`${key}=${val}`);
            }
        });
        window.location.hash = pieces.join("&");
    }

    // (forthic -- )
    word_ON_HASH_CHANGE(interp) {
        let forthic = interp.stack_pop();
        window.onhashchange = function() {
            interp.run(forthic);
        }
    }

    // (str -- encoded)
    word_ENCODE_URI_COMPONENT(interp) {
        let str = interp.stack_pop();
        let result = '';
        if (str)   result = encodeURIComponent(str);
        interp.stack_push(result);
    }

    // (urlencoded -- decoded)
    word_DECODE_URI_COMPONENT(interp) {
        let urlencoded = interp.stack_pop();
        let result = '';
        if (urlencoded)   result = decodeURIComponent(urlencoded);
        interp.stack_push(result);
    }

    // ( -- char)
    word_QUOTE_CHAR(interp) {
        interp.stack_push(DLE);
    }

    // ( string -- quoted_string)
    word_QUOTED(interp) {
        let string = interp.stack_pop();
        let clean_string = '';
        for(let i=0; i < string.length; i++) {
            let c = string[i];
            if (c == DLE)   c = " ";
            clean_string += c;
        }
        let result = `${DLE}${clean_string}${DLE}`;
        interp.stack_push(result);
    }

    // ( period_s fwhile_condition forthic -- )
    async word_ASYNC_LOOP(interp) {
        let forthic = interp.stack_pop();
        let fwhile_condition = interp.stack_pop();
        let period_s = interp.stack_pop();

        async function run_loop() {
            console.log("Looping...")
            await interp.run(forthic);
            await interp.run(fwhile_condition);
            let while_condition = interp.stack_pop();
            if (while_condition) {
                setTimeout(run_loop, period_s*1000)
            }
        }
        await run_loop();
    }


    // ( -- )
    word_PROFILE_START(interp) {
        interp.start_profiling()
    }


    // ( -- )
    word_PROFILE_END(interp) {
        interp.stop_profiling()
    }


    // ( label -- )
    word_PROFILE_TIMESTAMP(interp) {
        let label = interp.stack_pop()
        interp.add_timestamp(label)
    }


    // ( -- )
    word_PROFILE_DATA(interp) {
        let histogram = interp.word_histogram()
        let timestamps = interp.profile_timestamps()

        let result = {
            "word_counts": [],
            "timestamps": []
        }
        histogram.forEach(val => {
            let rec = {"word": val["word"],
                       "count": val["count"]};
            result["word_counts"].push(rec);
        });

        let prev_time = 0.0
        timestamps.forEach(t => {
            let rec = {"label": t["label"],
                       "time_ms": t["time_ms"],
                       "delta": t["time_ms"] - prev_time};
            prev_time = t["time_ms"];
            result["timestamps"].push(rec);
        });

        interp.stack_push(result)
    }


    // ( -- null )
    word_NULL(interp) {
        interp.stack_push(null);
    }

    // ( value default_value -- value )
    word_DEFAULT(interp) {
        let default_value = interp.stack_pop();
        let value = interp.stack_pop();
        if (value === undefined || value === null || value === "")   value = default_value;
        interp.stack_push(value);
    }

    // ( value default_forthic -- value )
    word_star_DEFAULT(interp) {
        let default_forthic = interp.stack_pop();
        let value = interp.stack_pop();
        if (value === undefined || value === null || value == "") {
            interp.run(default_forthic);
            value = interp.stack_pop();
        }
        interp.stack_push(value);
    }

    // ( Record default_key/vals -- Record )
    word_REC_DEFAULTS(interp) {
        let key_vals = interp.stack_pop();
        let record = interp.stack_pop();
        key_vals.forEach(key_val => {
            let key = key_val[0];
            let value = record[key];
            if (value === undefined || value === null || value == "") {
                record[key] = key_val[1];
            }
        });

        interp.stack_push(record);
    }

    // ( item string num-times -- ? )
    async word_l_REPEAT(interp) {
        let num_times = interp.stack_pop();
        let string = interp.stack_pop();
        for (let i=0; i < num_times; i++) {
           // Store item so we can push it back later
           let item = interp.stack_pop();
           interp.stack_push(item);

           await interp.run(string);
           let res = interp.stack_pop();

           // Push original item and result
           interp.stack_push(item);
           interp.stack_push(res);
        }
    }

    // ( a -- a )
    async word_IDENTITY(interp) {
    }

    // ( value num_places -- str )
    word_to_FIXED(interp) {
        let num_places = interp.stack_pop();
        let value = interp.stack_pop();
        let result = value;
        if (value === undefined || value === null)  result = '';
        else if (!isNaN(value)) result = value.toFixed(num_places);
        interp.stack_push(result);
    }

    // ( object -- json )
    word_to_JSON(interp) {
        let object = interp.stack_pop();
        let result = JSON.stringify(object);
        interp.stack_push(result);
    }

    // ( json -- object )
    word_JSON_to(interp) {
        let str = interp.stack_pop();
        let result = null;
        if (str)   result = JSON.parse(str);
        interp.stack_push(result);
    }

    // ( -- )
    word_dot_s(interp) {
        console.log(interp.stack);
        debugger
    }

    // ( text -- )
    word_dot_p(interp) {
        let text = interp.stack_pop();
        console.log(text);
    }

    // ( a b -- a - b )
    word_minus(interp) {
        let b = interp.stack_pop();
        let a = interp.stack_pop();
        interp.stack_push(a-b);
    }
}

// Descends into record using an array of fields, returning final value or null
function drill_for_value(record, fields) {
    let result = record;
    for (let i=0; i < fields.length; i++) {
        let f = fields[i];
        if (result == null)   return null;
        result = result[f];
    }
    return result
}

export { GlobalModule };
