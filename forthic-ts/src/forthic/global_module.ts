import { Module, Word, PushValueWord } from "./module";
import {
  is_array,
  is_record,
  is_string,
  pretty_print,
  to_date,
  date_to_string,
  date_to_int,
} from "./utils";


import { MapWord } from "./global_module/map_word";
import { CodeLocation } from "./tokenizer";
import { type Interpreter } from "./interpreter";
import { IntentionalStopError, InvalidVariableNameError } from "./errors";


const DLE = String.fromCharCode(16); // ASCII char for "Data Link Escape" used as an untypeable quote

export class GlobalModule extends Module {
  module_id: string;
  literal_handlers: Array<(value: any) => any>;

  constructor(interp: Interpreter) {
    super("<GLOBAL>", interp);

    this.module_id = `<GLOBAL>-${Math.floor(Math.random() * 1000000)}`;

    // Set default flags
    interp.set_flags(this.module_id, {
      with_key: null,
      push_error: null,
      comparator: null,
      push_rest: null,
      depth: null,
      overwrite: null,
      delay: null,
      interps: 1,
      note_progress: null,
    });

    this.literal_handlers = [
      this.to_bool,
      this.to_float,
      this.to_literal_date,
      this.to_time,
      this.to_int,
    ];

    // --------------------
    // Base words
    this.add_module_word("VARIABLES", this.word_VARIABLES);
    this.add_module_word("!", this.word_bang);
    this.add_module_word("@", this.word_at);
    this.add_module_word("!@", this.word_bang_at);
    this.add_module_word("INTERPRET", this.word_INTERPRET);

    this.add_module_word("EXPORT", this.word_EXPORT);
    this.add_module_word("USE-MODULES", this.word_USE_MODULES);
    this.add_module_word("REC", this.word_REC);
    this.add_module_word("REC@", this.word_REC_at);
    this.add_module_word("|REC@", this.word_pipe_REC_at);
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
    this.add_module_word("GROUPS-OF", this.word_GROUPS_OF);
    this.add_module_word("INDEX", this.word_INDEX);
    this.add_module_word("MAP", this.word_MAP);
    this.add_module_word("FOREACH", this.word_FOREACH);
    this.add_module_word("INVERT-KEYS", this.word_INVERT_KEYS);
    this.add_module_word("ZIP", this.word_ZIP);
    this.add_module_word("ZIP-WITH", this.word_ZIP_WITH);
    this.add_module_word("KEYS", this.word_KEYS);
    this.add_module_word("VALUES", this.word_VALUES);
    this.add_module_word("LENGTH", this.word_LENGTH);
    this.add_module_word("RANGE", this.word_RANGE);
    this.add_module_word("SLICE", this.word_SLICE);
    this.add_module_word("DIFFERENCE", this.word_DIFFERENCE);
    this.add_module_word("INTERSECTION", this.word_INTERSECTION);
    this.add_module_word("UNION", this.word_UNION);
    this.add_module_word("SELECT", this.word_SELECT);
    this.add_module_word("TAKE", this.word_TAKE);
    this.add_module_word("DROP", this.word_DROP);
    this.add_module_word("ROTATE", this.word_ROTATE);
    this.add_module_word("ROTATE-ELEMENT", this.word_ROTATE_ELEMENT);
    this.add_module_word("ARRAY?", this.word_ARRAY_q);

    this.add_module_word("SHUFFLE", this.word_SHUFFLE);
    this.add_module_word("SORT", this.word_SORT);
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
    this.add_module_word("LOWERCASE", this.word_LOWERCASE);
    this.add_module_word("UPPERCASE", this.word_UPPERCASE);
    this.add_module_word("ASCII", this.word_ASCII);
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
    this.add_module_word("PRETTIFY", this.word_PRETTIFY);
    this.add_module_word("JSON>", this.word_JSON_to);
    this.add_module_word("LOAD-SCREEN", this.word_LOAD_SCREEN);
    this.add_module_word(".s", this.word_dot_s);
    this.add_module_word(".S", this.word_dot_S);

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
    this.add_module_word("ADD-DAYS", this.word_ADD_DAYS);
    this.add_module_word("SUBTRACT-DATES", this.word_SUBTRACT_DATES);
    this.add_module_word("DATE>STR", this.word_DATE_to_STR);
    this.add_module_word("DATE>INT", this.word_DATE_to_INT);
    this.add_module_word("DATE-TIME>DATETIME", this.word_DATE_TIME_to_DATETIME);
    this.add_module_word("DATETIME>TIMESTAMP", this.word_DATETIME_to_TIMESTAMP);
    this.add_module_word("TIMESTAMP>DATETIME", this.word_TIMESTAMP_to_DATETIME);
    this.add_module_word("STR>DATETIME", this.word_STR_to_DATETIME);
    this.add_module_word("STR>TIMESTAMP", this.word_STR_to_TIMESTAMP);
    this.add_module_word("DAYS-AGO", this.word_DAYS_AGO);

    // --------------------
    // Math words
    this.add_module_word("+", this.word_plus);
    this.add_module_word("-", this.word_minus);
    this.add_module_word("*", this.word_times);
    this.add_module_word("/", this.word_divide_by);
    this.add_module_word("ADD", this.word_plus);
    this.add_module_word("SUBTRACT", this.word_minus);
    this.add_module_word("MULTIPLY", this.word_times);
    this.add_module_word("DIVIDE", this.word_divide_by);
    this.add_module_word("MOD", this.word_MOD);
    this.add_module_word("MEAN", this.word_MEAN);
    this.add_module_word("MAX", this.word_MAX);
    this.add_module_word("MIN", this.word_MIN);
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
    this.add_module_word("RANGE-BUCKETS", this.word_RANGE_BUCKETS);
    this.add_module_word("INFINITY", this.word_INFINITY);


    // ----------------
    // Flag words
    this.add_module_word("!PUSH-ERROR", this.word_bang_PUSH_ERROR);
    this.add_module_word("!WITH-KEY", this.word_bang_WITH_KEY);
    this.add_module_word("!COMPARATOR", this.word_bang_COMPARATOR);
    this.add_module_word("!PUSH-REST", this.word_bang_PUSH_REST);
    this.add_module_word("!DEPTH", this.word_bang_DEPTH);
    this.add_module_word("!OVERWRITE", this.word_bang_OVERWRITE);
    this.add_module_word("!DELAY", this.word_bang_DELAY);
    this.add_module_word("!INTERPS", this.word_bang_INTERPS);
    this.add_module_word("!NOTE-PROGRESS", this.word_bang_NOTE_PROGRESS);

    // --------------------
    // Misc words (js-specific)
    this.add_module_word("URL-ENCODE", this.word_URL_ENCODE);
    this.add_module_word("URL-DECODE", this.word_URL_DECODE);
    this.add_module_word("QUOTE-CHAR", this.word_QUOTE_CHAR);
    this.add_module_word("QUOTED", this.word_QUOTED);
    this.add_module_word("CONSOLE.LOG", this.word_CONSOLE_LOG);

    // --------------------
    // Profiling words
    this.add_module_word("PROFILE-START", this.word_PROFILE_START);
    this.add_module_word("PROFILE-TIMESTAMP", this.word_PROFILE_TIMESTAMP);
    this.add_module_word("PROFILE-END", this.word_PROFILE_END);
    this.add_module_word("PROFILE-DATA", this.word_PROFILE_DATA);
  }

  find_word(name: string): Word | null {
    let result = super.find_word(name);
    if (!result) result = this.find_literal_word(name);
    return result;
  }

  find_literal_word(string: string): Word | null {
    const self = this;
    let value = null;
    for (let i = 0; i < self.literal_handlers.length; i++) {
      value = self.literal_handlers[i](string);
      if (value !== null) return new PushValueWord(string, value);
    }
    return null;
  }

  // =======================
  // Literal handlers
  to_bool(str_val: string): boolean | null {
    if (str_val == "TRUE") return true;
    else if (str_val == "FALSE") return false;
    else return null;
  }

  to_int(str_val: string): number | null {
    const result = parseInt(str_val);
    if (isNaN(result)) return null;
    return result;
  }

  to_float(str_val: string): number | null {
    if (str_val.indexOf(".") == -1) return null;
    const result = parseFloat(str_val);
    if (isNaN(result)) return null;
    else return result;
  }

  to_literal_date(str_val: string): Date | null {
    const match = str_val.match(/(\d{4})-(\d{2})-(\d{2})/);
    if (!match) return null;
    const year = Number(match[1]);
    const month = Number(match[2]);
    const day = Number(match[3]);
    const result = new Date();
    result.setUTCFullYear(year, month - 1, day);
    result.setUTCHours(0, 0, 0, 0);

    return result;
  }

  to_time(str_val: string): Date | null {
    const match = str_val.match(/(\d{1,2}):(\d{2})/);
    if (!match) return null;

    const hours = Number(match[1]);
    const minutes = Number(match[2]);

    if (hours > 23) return null;
    if (minutes >= 60) return null;

    const result = new Date();
    result.setHours(hours, minutes, 0, 0);
    return result;
  }

  // Convenience function to create element word handlers
  make_element_word(
    element_name: string,
  ): (interp: Interpreter) => Promise<void> {
    async function Result(interp: Interpreter) {
      interp.stack_push(`${element_name}`);
      await interp.run("Element");
    }
    return Result;
  }

  add_element_word(element_name: string) {
    this.add_module_word(element_name, this.make_element_word(element_name));
  }

  // =======================
  // Words

  // ( varnames -- )
  word_VARIABLES(interp: Interpreter): void {
    const varnames = interp.stack_pop();
    const module = interp.cur_module();
    varnames.forEach((v) => {
      if (v.match(/__.*/)) {
        throw new InvalidVariableNameError(v, "Variable names cannot begin with '__'", interp.get_string_location());
      }
      module.add_variable(v);
    });
  }

  // ( value variable -- )
  word_bang(interp: Interpreter) {
    const variable = interp.stack_pop();
    const value = interp.stack_pop();
    variable.value = value;
  }

  // ( variable -- value )
  word_at(interp: Interpreter) {
    const variable = interp.stack_pop();
    interp.stack_push(variable.value);
  }

  // ( value variable -- value )
  word_bang_at(interp: Interpreter) {
    const variable = interp.stack_pop();
    const value = interp.stack_pop();
    variable.value = value;
    interp.stack_push(variable.value);
  }

  // ( string -- )
  async word_INTERPRET(interp: Interpreter) {
    const string = interp.stack_pop();
    const string_location = interp.get_string_location();
    if (string) await interp.run(string, string_location);
  }

  // ( names -- )
  word_EXPORT(interp: Interpreter) {
    const names = interp.stack_pop();
    interp.cur_module().add_exportable(names);
  }

  // ( names -- )
  async word_USE_MODULES(interp: Interpreter) {
    const names = interp.stack_pop();

    for (let i = 0; i < names.length; i++) {
      const name = names[i];
      let module_name = name;
      let prefix = name;
      if (name instanceof Array) {
        module_name = name[0];
        prefix = name[1];
      }

      const module = interp.find_module(module_name);
      interp.get_app_module().import_module(prefix, module, interp);
    }
  }

  // ( key_vals -- rec )
  word_REC(interp: Interpreter) {
    let key_vals = interp.stack_pop();
    if (!key_vals) key_vals = [];
    const result = {};
    key_vals.forEach((pair) => {
      let key = null;
      let val = null;
      if (pair) {
        if (pair.length >= 1) key = pair[0];
        if (pair.length >= 2) val = pair[1];
      }
      result[key] = val;
    });
    interp.stack_push(result);
  }

  // ( rec field -- value )
  // ( rec fields -- value )
  word_REC_at(interp: Interpreter) {
    const field = interp.stack_pop();
    const rec = interp.stack_pop();

    if (!rec) {
      interp.stack_push(null);
      return;
    }

    let fields = [field];
    if (field instanceof Array) fields = field;
    const result = drill_for_value(rec, fields);
    interp.stack_push(result);
  }

  // ( records field -- values )
  // ( records fields -- values )
  async word_pipe_REC_at(interp: Interpreter) {
    const fields = interp.stack_pop();
    await interp.run(`'${JSON.stringify(fields)} REC@' MAP`);
  }

  // ( rec value field -- rec )
  word_l_REC_bang(interp: Interpreter) {
    const field = interp.stack_pop();
    const value = interp.stack_pop();
    let rec = interp.stack_pop();

    if (!rec) rec = {};

    let fields = null;
    if (field instanceof Array) fields = field;
    else fields = [field];

    function ensure_field(rec, field) {
      let res = rec[field];
      if (res === undefined) {
        res = {};
        rec[field] = res;
      }
      return res;
    }

    let cur_rec = rec;
    // Drill down up until the last value
    for (let i = 0; i < fields.length - 1; i++) {
      cur_rec = ensure_field(cur_rec, fields[i]);
    }

    // Set the value at the right depth within rec
    cur_rec[fields[fields.length - 1]] = value;

    interp.stack_push(rec);
  }

  // ( array item -- array )
  // ( record key/val -- record )
  word_APPEND(interp: Interpreter) {
    const item = interp.stack_pop();
    let result = interp.stack_pop();

    if (!result) result = [];

    if (result instanceof Array) result.push(item);
    // If not a list, treat as record
    else result[item[0]] = item[1];

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- record )
  word_REVERSE(interp: Interpreter) {
    let result = interp.stack_pop();

    if (!result) {
      interp.stack_push(result);
      return;
    }

    if (result instanceof Array) result = result.reverse();

    interp.stack_push(result);
  }

  // ( array -- array )
  word_UNIQUE(interp: Interpreter) {
    const container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    let result = container;
    if (container instanceof Array) {
      result = [...new Set(container)];
    }

    interp.stack_push(result);
  }

  // ( array index -- array )
  // ( record key -- record )
  word_L_DEL(interp: Interpreter) {
    const key = interp.stack_pop();
    const container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    if (container instanceof Array) container.splice(key, 1);
    else delete container[key];
    interp.stack_push(container);
  }

  // ( array old_keys new_keys -- array )
  // ( record old_keys new_keys -- record )
  word_RELABEL(interp: Interpreter) {
    const new_keys = interp.stack_pop();
    const old_keys = interp.stack_pop();
    const container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    if (old_keys.length != new_keys.length)
      throw "RELABEL: old_keys and new_keys must be same length";

    const new_to_old = {};
    for (let i = 0; i < old_keys.length; i++) {
      new_to_old[new_keys[i]] = old_keys[i];
    }

    let result: any = [];
    if (container instanceof Array) {
      Object.keys(new_to_old)
        .sort()
        .forEach((k) => result.push(container[new_to_old[k]]));
    } else {
      result = {};
      Object.keys(new_to_old).forEach(
        (k) => (result[k] = container[new_to_old[k]]),
      );
    }

    interp.stack_push(result);
  }

  // ( array field -- field_to_item )
  // ( record field -- field_to_item )
  word_BY_FIELD(interp: Interpreter) {
    const field = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let values = null;
    if (container instanceof Array) {
      values = container;
    } else {
      values = [];
      Object.keys(container).forEach((k) => {
        values.push(container[k]);
      });
    }

    const result = {};
    values.forEach((v) => {
      if (v) result[v[field]] = v;
    });

    interp.stack_push(result);
  }

  // ( array field -- field_to_items )
  // ( record field -- field_to_items )
  word_GROUP_BY_FIELD(interp: Interpreter) {
    const field = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let values = [];
    if (container instanceof Array) values = container;
    else values = Object.keys(container).map((k) => container[k]);

    const result = {};
    values.forEach((v) => {
      const field_val = v[field];
      if (field_val instanceof Array) {
        for (const fv of field_val) {
          if (!result[fv]) result[fv] = [];
          result[fv].push(v);
        }
      } else {
        if (!result[field_val]) result[field_val] = [];
        result[field_val].push(v);
      }
    });

    interp.stack_push(result);
  }

  // ( array forthic -- group_to_items )
  // ( record forthic -- group_to_items )
  async word_GROUP_BY(interp: Interpreter) {
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    let container = interp.stack_pop();

    const flags = interp.get_flags(this.module_id);

    if (!container) container = [];

    let keys, values;

    if (container instanceof Array) {
      keys = [];
      for (let i = 0; i < container.length; i++) keys.push(i);
      values = container;
    } else {
      keys = Object.keys(container);
      values = keys.map((k) => container[k]);
    }

    const result = {};
    for (let i = 0; i < values.length; i++) {
      const key = keys[i];
      const value = values[i];
      if (flags.with_key) interp.stack_push(key);
      interp.stack_push(value);
      await interp.run(forthic, string_location);
      const group = interp.stack_pop();
      if (!result[group]) result[group] = [];
      result[group].push(value);
    }

    interp.stack_push(result);
  }

  // ( array n -- arrays )
  // ( record n -- records )
  word_GROUPS_OF(interp: Interpreter) {
    const size = interp.stack_pop();
    let container = interp.stack_pop();
    if (size <= 0) throw "GROUPS-OF requires group size > 0";

    if (!container) container = [];

    function group_items(items, group_size) {
      const num_groups = Math.ceil(items.length / group_size);
      const res = [];
      let remaining = items.slice();
      for (let i = 0; i < num_groups; i++) {
        res.push(remaining.slice(0, group_size));
        remaining = remaining.slice(group_size);
      }

      return res;
    }

    function extract_rec(record, keys) {
      const res = {};
      keys.forEach((k) => (res[k] = record[k]));
      return res;
    }

    let result;
    if (container instanceof Array) {
      result = group_items(container, size);
    } else {
      const keys = Object.keys(container);
      const key_groups = group_items(keys, size);
      result = key_groups.map((ks) => extract_rec(container, ks));
    }

    interp.stack_push(result);
    return;
  }

  // ( array forthic -- record )
  async word_INDEX(interp: Interpreter) {
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();
    const items = interp.stack_pop();

    if (!items) {
      interp.stack_push(items);
      return;
    }

    const result = {};
    for (let i = 0; i < items.length; i++) {
      const item = items[i];
      interp.stack_push(item);
      await interp.run(forthic, string_location);
      const keys = interp.stack_pop();
      keys.forEach((k) => {
        const lowercased_key = k.toLowerCase();
        if (result[lowercased_key]) result[lowercased_key].push(item);
        else result[lowercased_key] = [item];
      });
    }
    interp.stack_push(result);
  }

  // ( items forthic -- [ ? ] )
  async word_MAP(interp: Interpreter) {
    const forthic: string = interp.stack_pop();
    const string_location = interp.get_string_location();
    const items = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);

    const map_word = new MapWord(items, forthic, string_location, flags);
    await map_word.execute(interp);
  }

  // ( items word -- ? )
  async word_FOREACH(interp: Interpreter) {
    const forthic: string = interp.stack_pop();
    const string_location = interp.get_string_location();

    let items = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);

    if (!items) items = [];

    const errors = [];
    if (items instanceof Array) {
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        if (flags.with_key) interp.stack_push(i);
        interp.stack_push(item);
        if (flags.push_error)
          errors.push(
            await execute_returning_error(interp, forthic, string_location),
          );
        else await interp.run(forthic, string_location );
      }
    } else {
      const keys = Object.keys(items);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        const item = items[k];
        if (flags.with_key) interp.stack_push(k);
        interp.stack_push(item);
        if (flags.push_error)
          errors.push(
            await execute_returning_error(interp, forthic, string_location),
          );
        else await interp.run(forthic, string_location );
      }
    }

    if (flags.push_error) interp.stack_push(errors);
  }

  // ( record -- record )
  word_INVERT_KEYS(interp: Interpreter) {
    const record = interp.stack_pop();
    const result = {};
    Object.keys(record).forEach((first_key) => {
      const sub_record = record[first_key];
      Object.keys(sub_record).forEach((second_key) => {
        const value = sub_record[second_key];
        if (!result[second_key]) result[second_key] = {};
        result[second_key][first_key] = value;
      });
    });
    interp.stack_push(result);
  }

  // ( array1 array2 -- array )
  // ( record1 record2 -- record )
  word_ZIP(interp: Interpreter) {
    let container2 = interp.stack_pop();
    let container1 = interp.stack_pop();

    if (!container1) container1 = [];
    if (!container2) container2 = [];

    let result;
    if (container2 instanceof Array) {
      result = [];
      for (let i = 0; i < container1.length; i++) {
        let value2 = null;
        if (i < container2.length) value2 = container2[i];
        result.push([container1[i], value2]);
      }
    } else {
      result = {};
      Object.keys(container1).forEach((k) => {
        const v = container1[k];
        result[k] = [v, container2[k]];
      });
    }

    interp.stack_push(result);
  }

  // ( array1 array2 forthic -- array )
  // ( record1 record2 forthic -- record )
  async word_ZIP_WITH(interp: Interpreter) {
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    let container2 = interp.stack_pop();
    let container1 = interp.stack_pop();

    if (!container1) container1 = [];
    if (!container2) container2 = [];

    let result;
    if (container2 instanceof Array) {
      result = [];
      for (let i = 0; i < container1.length; i++) {
        let value2 = null;
        if (i < container2.length) value2 = container2[i];
        interp.stack_push(container1[i]);
        interp.stack_push(value2);
        await interp.run(forthic, string_location);
        const res = interp.stack_pop();
        result.push(res);
      }
    } else {
      result = {};
      const keys = Object.keys(container1);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        interp.stack_push(container1[k]);
        interp.stack_push(container2[k]);
        await interp.run(forthic, string_location );
        const res = interp.stack_pop();
        result[k] = res;
      }
    }

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- array )
  word_KEYS(interp: Interpreter) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = [];
      for (let i = 0; i < container.length; i++) result.push(i);
    } else {
      result = Object.keys(container);
    }

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- array )
  word_VALUES(interp: Interpreter) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = container;
    } else {
      result = [];
      Object.keys(container).forEach((k) => result.push(container[k]));
    }

    interp.stack_push(result);
  }

  // ( array -- length )
  // ( record -- length )
  word_LENGTH(interp: Interpreter) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array || typeof container == "string") {
      result = container.length;
    } else {
      result = Object.keys(container).length;
    }

    interp.stack_push(result);
  }

  // ( array fstart fend -- indices )
  async word_RANGE(interp: Interpreter) {
    const fend = interp.stack_pop();
    const fend_string_location = interp.get_string_location();

    const fstart = interp.stack_pop();
    const fstart_string_location = interp.get_string_location();

    let array = interp.stack_pop();

    if (!array) array = [];

    let start_found = false;
    let end_found = false;

    let start_index = null;
    let end_index = null;

    for (let index = 0; index < array.length; index++) {
      const item = array[index];

      if (!start_found) {
        interp.stack_push(item);
        await interp.run(fstart, fstart_string_location);
        start_found = interp.stack_pop();
        if (start_found) {
          start_index = index;
        }
      }

      if (start_found && !end_found) {
        interp.stack_push(item);
        await interp.run(fend, fend_string_location);
        end_found = interp.stack_pop();
        if (end_found) {
          end_index = index;
          break;
        }
      }
    }
    interp.stack_push([start_index, end_index]);
  }

  // ( array start end -- array )
  // ( record start end -- record )
  word_SLICE(interp: Interpreter) {
    let end = Math.trunc(interp.stack_pop());
    let start = Math.trunc(interp.stack_pop());
    let container = interp.stack_pop();

    if (!container) container = [];

    let length;
    if (container instanceof Array) {
      length = container.length;
    } else {
      length = Object.keys(container).length;
    }

    function normalize_index(index) {
      let res = index;
      if (index < 0) res = index + length;
      return res;
    }

    start = normalize_index(start);
    end = normalize_index(end);

    let step = 1;
    if (start > end) step = -1;

    let indexes = [start];
    if (start < 0 || start >= length) indexes = [];

    while (start != end) {
      start = start + step;
      if (start < 0 || start >= length) indexes.push(null);
      else indexes.push(start);
    }

    let result;
    if (container instanceof Array) {
      result = [];
      indexes.forEach((i) => {
        if (i === null) result.push(null);
        else result.push(container[i]);
      });
    } else {
      const keys = Object.keys(container).sort();
      result = {};
      indexes.forEach((i) => {
        if (i !== null) {
          const k = keys[i];
          result[k] = container[k];
        }
      });
    }

    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_DIFFERENCE(interp: Interpreter) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function difference(l, r) {
      const res = [];
      l.forEach((item) => {
        if (r.indexOf(item) < 0) res.push(item);
      });
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = difference(lcontainer, rcontainer);
    } else {
      const lkeys = Object.keys(lcontainer);
      const rkeys = Object.keys(rcontainer);

      const diff = difference(lkeys, rkeys);
      result = {};
      diff.forEach((k) => (result[k] = lcontainer[k]));
    }

    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_INTERSECTION(interp: Interpreter) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function intersection(l, r) {
      const res = [];
      l.forEach((item) => {
        if (r.indexOf(item) >= 0) res.push(item);
      });
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = intersection(lcontainer, rcontainer);
    } else {
      const lkeys = Object.keys(lcontainer);
      const rkeys = Object.keys(rcontainer);

      const intersect = intersection(lkeys, rkeys);
      result = {};
      intersect.forEach((k) => (result[k] = lcontainer[k]));
    }
    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_UNION(interp: Interpreter) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function union(l, r) {
      const keyset = {};
      l.forEach((item) => {
        keyset[item] = true;
      });
      r.forEach((item) => {
        keyset[item] = true;
      });
      const res = Object.keys(keyset);
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = union(lcontainer, rcontainer);
    } else {
      const lkeys = Object.keys(lcontainer);
      const rkeys = Object.keys(rcontainer);

      const keys = union(lkeys, rkeys);
      result = {};
      keys.forEach((k) => {
        let val = lcontainer[k];
        if (val === undefined) val = rcontainer[k];
        result[k] = val;
      });
    }

    interp.stack_push(result);
  }

  // ( larray forthic -- array )
  // ( lrecord forthic -- record )
  async word_SELECT(interp: Interpreter) {
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    const container = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);

    if (!container) {
      interp.stack_push(container);
      return;
    }

    let result;
    if (container instanceof Array) {
      result = [];
      for (let i = 0; i < container.length; i++) {
        const item = container[i];
        if (flags.with_key) interp.stack_push(i);
        interp.stack_push(item);
        await interp.run(forthic, string_location);
        const should_select = interp.stack_pop();
        if (should_select) result.push(item);
      }
    } else {
      result = {};
      const keys = Object.keys(container);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        const v = container[k];
        if (flags.with_key) interp.stack_push(k);
        interp.stack_push(v);
        await interp.run(forthic, string_location);
        const should_select = interp.stack_pop();
        if (should_select) result[k] = v;
      }
    }

    interp.stack_push(result);
  }

  // ( array n -- rest taken )
  // ( record n -- rest taken )
  word_TAKE(interp: Interpreter) {
    const n = interp.stack_pop();
    let container = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);

    if (!container) container = [];

    let rest, taken;
    if (container instanceof Array) {
      taken = container.slice(0, n);
      rest = container.slice(n);
    } else {
      const keys = Object.keys(container).sort();
      const taken_keys = keys.slice(0, n);
      const rest_keys = keys.slice(n);
      taken = taken_keys.map((k) => container[k]);
      rest = rest_keys.map((k) => container[k]);
    }

    interp.stack_push(taken);
    if (flags.push_rest) interp.stack_push(rest);
  }

  // ( array n -- array )
  // ( record n -- record )
  word_DROP(interp: Interpreter) {
    const n = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = container.slice(n);
    } else {
      const keys = Object.keys(container).sort();
      const rest_keys = keys.slice(n);
      result = rest_keys.map((k) => container[k]);
    }

    interp.stack_push(result);
  }

  // ( array  -- array )
  // ( record  -- record )
  word_ROTATE(interp: Interpreter) {
    const container = interp.stack_pop();

    let result;
    if (!container) {
      result = container;
    } else if (container instanceof Array) {
      result = container;
      if (container.length > 0) {
        const val = result.pop();
        result.unshift(val);
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }

  // ( array element -- array )
  // ( record element -- record )
  // Moves element to front of array
  word_ROTATE_ELEMENT(interp: Interpreter) {
    const element = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      const index = container.indexOf(element);
      result = container;
      if (index > 0) {
        result.splice(index, 1);
        result.unshift(element);
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }

  // ( val -- bool )
  word_ARRAY_q(interp: Interpreter) {
    const val = interp.stack_pop();
    const result = val instanceof Array;
    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- record )
  word_SHUFFLE(interp: Interpreter) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      // See: https://medium.com/@nitinpatel_20236/
      //    how-to-shuffle-correctly-shuffle-an-array-in-javascript-15ea3f84bfb
      result = container;
      for (let i = result.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * i);
        const temp = result[i];
        result[i] = result[j];
        result[j] = temp;
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }

  // ( field -- key_func )
  word_FIELD_KEY_FUNC(interp: Interpreter) {
    const field = interp.stack_pop();

    function result(record) {
      return record[field];
    }

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- record )
  async word_SORT(interp: Interpreter) {
    const flag_string_position = interp.get_string_location(); // NOTE: If the user specified a comparator flag, we want to get the string position of it
    let container = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);
    const comparator = flags["comparator"];

    if (!container) container = [];
    if (!(container instanceof Array)) {
      interp.stack_push(container);
      return;
    }

    // -----
    // Default sort
    function sort_without_comparator() {
      return container.sort();
    }

    // -----
    // Sort using a forthic string
    async function sort_with_forthic(forthic) {
      async function make_aug_array(vals) {
        const res = [];
        for (let i = 0; i < vals.length; i++) {
          const val = vals[i];
          interp.stack_push(val);
          await interp.run(forthic, flag_string_position );
          const aug_val = interp.stack_pop();
          res.push([val, aug_val]);
        }
        return res;
      }

      function cmp_items(l, r) {
        const l_val = l[1];
        const r_val = r[1];

        if (l_val < r_val) return -1;
        else if (l_val > r_val) return 1;
        else return 0;
      }

      function de_aug_array(aug_vals) {
        const res = aug_vals.map((aug_val) => aug_val[0]);
        return res;
      }

      // Create an augmented array, sort it and then return the underlying values
      // NOTE: We're doing it this way because sort is synchronous
      const aug_array = await make_aug_array(container);
      aug_array.sort(cmp_items);
      return de_aug_array(aug_array);
    }

    // -----
    // Sort with key func
    function sort_with_key_func(key_func) {
      function cmp_items(l, r) {
        const l_val = key_func(l);
        const r_val = key_func(r);
        if (l_val < r_val) return -1;
        else if (l_val > r_val) return 1;
        else return 0;
      }

      return container.sort(cmp_items);
    }

    // Figure out what to do
    let result;
    if (typeof comparator == "string") {
      result = await sort_with_forthic(comparator);
    } else if (comparator === undefined) {
      result = sort_without_comparator();
    } else {
      result = sort_with_key_func(comparator);
    }

    interp.stack_push(result);
  }

  // ( array n -- item )
  // ( record n -- value )
  word_NTH(interp: Interpreter) {
    const n = interp.stack_pop();
    const container = interp.stack_pop();

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
    } else {
      if (n < 0 || n >= Object.keys(container).length) {
        interp.stack_push(null);
        return;
      }
      const keys = Object.keys(container).sort();
      const key = keys[n];
      result = container[key];
    }

    interp.stack_push(result);
  }

  // ( array -- item )
  // ( record -- value )
  word_LAST(interp: Interpreter) {
    const container = interp.stack_pop();

    if (!container) {
      interp.stack_push(null);
      return;
    }

    let result;
    if (container instanceof Array) {
      if (container.length == 0) result = null;
      else result = container[container.length - 1];
    } else {
      const keys = Object.keys(container).sort();
      if (keys.length == 0) result = null;
      else result = container[keys[keys.length - 1]];
    }

    interp.stack_push(result);
  }

  // ( array -- a1 a2 .. an )
  // ( record -- v1 v2 .. vn )
  word_UNPACK(interp: Interpreter) {
    let container = interp.stack_pop();

    if (!container) container = [];

    if (container instanceof Array) {
      container.forEach((item) => {
        interp.stack_push(item);
      });
    } else {
      const keys = Object.keys(container).sort();
      keys.forEach((k) => {
        interp.stack_push(container[k]);
      });
    }
  }

  // ( array -- array )
  // ( record -- record )
  word_FLATTEN(interp: Interpreter) {
    let nested = interp.stack_pop();
    const flags = interp.get_flags(this.module_id);

    if (!nested) nested = [];
    const depth = flags.depth;

    function is_record(obj) {
      const keys = Object.keys(obj);
      return keys.length > 0;
    }

    function add_to_record_result(item, key, keys, result) {
      const new_key = keys.concat([key]).join("\t");
      result[new_key] = item;
    }

    function fully_flatten_record(record, res, keys) {
      const record_keys = Object.keys(record);
      for (const k of record_keys) {
        const item = record[k];
        if (is_record(item)) fully_flatten_record(item, res, keys.concat([k]));
        else add_to_record_result(item, k, keys, res);
      }
      return res;
    }

    function flatten_record(record, depth, res, keys) {
      if (depth === undefined) return fully_flatten_record(record, res, keys);

      const record_keys = Object.keys(record);
      for (const k of record_keys) {
        const item = record[k];
        if (depth > 0 && is_record(item))
          flatten_record(item, depth - 1, res, keys.concat([k]));
        else add_to_record_result(item, k, keys, res);
      }
      return res;
    }

    let result;
    if (nested instanceof Array) {
      result = flatten_array(nested, depth);
    } else {
      result = flatten_record(nested, depth, {}, []);
    }

    interp.stack_push(result);
    return;
  }

  // ( array item -- index )
  // ( record item -- key )
  word_KEY_OF(interp: Interpreter) {
    const item = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      const index = container.indexOf(item);
      if (index < 0) result = null;
      else result = index;
    } else {
      result = null;
      const keys = Object.keys(container);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        const v = container[k];
        if (v == item) {
          result = k;
          break;
        }
      }
    }

    interp.stack_push(result);
    return;
  }

  // ( array init forthic -- value )
  // ( record init forthic -- value )
  async word_REDUCE(interp: Interpreter) {
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    const initial = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      interp.stack_push(initial);
      container.forEach(async (item) => {
        interp.stack_push(item);
        await interp.run(forthic, string_location);
      });
      result = interp.stack_pop();
    } else {
      interp.stack_push(initial);
      Object.keys(container).forEach(async (k) => {
        const v = container[k];
        interp.stack_push(v);
        await interp.run(forthic, string_location);
      });
      result = interp.stack_pop();
    }

    interp.stack_push(result);
  }

  // ( a -- )
  word_POP(interp: Interpreter) {
    interp.stack_pop();
  }

  // ( a -- a a )
  word_DUP(interp: Interpreter) {
    const a = interp.stack_pop();
    interp.stack_push(a);
    interp.stack_push(a);
  }

  // ( a b -- b a )
  word_SWAP(interp: Interpreter) {
    const b = interp.stack_pop();
    const a = interp.stack_pop();
    interp.stack_push(b);
    interp.stack_push(a);
  }

  // ( item -- str )
  word_to_STR(interp: Interpreter) {
    const item = interp.stack_pop();
    interp.stack_push(item.toString());
  }

  // ( str1 str2 -- str )
  // ( array_of_str -- str )
  word_CONCAT(interp: Interpreter) {
    const str2 = interp.stack_pop();
    let array;
    if (str2 instanceof Array) {
      array = str2;
    } else {
      const str1 = interp.stack_pop();
      array = [str1, str2];
    }

    const result = array.join("");
    interp.stack_push(result);
  }

  // ( string sep -- items )
  word_SPLIT(interp: Interpreter) {
    const sep = interp.stack_pop();
    let string = interp.stack_pop();

    if (!string) string = "";

    const result = string.split(sep);
    interp.stack_push(result);
  }

  // ( strings sep -- string )
  word_JOIN(interp: Interpreter) {
    const sep = interp.stack_pop();
    let strings = interp.stack_pop();

    if (!strings) strings = [];

    const result = strings.join(sep);
    interp.stack_push(result);
  }

  // ( -- char )
  word_slash_N(interp: Interpreter) {
    interp.stack_push("\n");
  }

  // ( -- char )
  word_slash_R(interp: Interpreter) {
    interp.stack_push("\r");
  }

  // ( -- char )
  word_slash_T(interp: Interpreter) {
    interp.stack_push("\t");
  }

  // ( A -- a )
  word_LOWERCASE(interp: Interpreter) {
    const string = interp.stack_pop();
    let result = "";
    if (string) result = string.toLowerCase();
    interp.stack_push(result);
  }

  // ( a -- A )
  word_UPPERCASE(interp: Interpreter) {
    const string = interp.stack_pop();
    let result = "";
    if (string) result = string.toUpperCase();
    interp.stack_push(result);
  }

  // ( string -- string )
  word_ASCII(interp: Interpreter) {
    let string = interp.stack_pop();

    if (!string) string = "";

    let result = "";
    for (let i = 0; i < string.length; i++) {
      const ch = string[i];
      if (ch.charCodeAt(0) < 256) result += ch;
    }
    interp.stack_push(result);
  }

  // ( str -- str )
  word_STRIP(interp: Interpreter) {
    const string = interp.stack_pop();
    let result = string;
    if (result) result = result.trim();
    interp.stack_push(result);
  }

  // ( string text replace -- string )
  word_REPLACE(interp: Interpreter) {
    const replace = interp.stack_pop();
    const text = interp.stack_pop();
    const string = interp.stack_pop();

    let result = string;
    if (string) {
      const pattern = new RegExp(text, "g");
      result = string.replace(pattern, replace);
    }
    interp.stack_push(result);
  }

  // ( string pattern -- match )
  word_RE_MATCH(interp: Interpreter) {
    const pattern = interp.stack_pop();
    const string = interp.stack_pop();

    const re_pattern = new RegExp(pattern);
    let result = false;
    if (string !== null) result = string.match(re_pattern);
    interp.stack_push(result);
  }

  // ( string pattern -- matches )
  word_RE_MATCH_ALL(interp: Interpreter) {
    const pattern = interp.stack_pop();
    const string = interp.stack_pop();

    const re_pattern = new RegExp(pattern, "g");
    let matches = [];
    if (string !== null) matches = string.matchAll(re_pattern);
    const result = Array.from(matches).map((v) => v[1]);

    interp.stack_push(result);
  }

  // ( match num -- string )
  word_RE_MATCH_GROUP(interp: Interpreter) {
    const num = interp.stack_pop();
    const match = interp.stack_pop();
    let result = null;
    if (match) result = match[num];
    interp.stack_push(result);
  }

  // ( time -- time )
  word_AM(interp: Interpreter) {
    const time = interp.stack_pop();
    if (!(time instanceof Date)) throw "AM expecting a time";

    let result = time;
    if (time.getHours() >= 12) {
      result = new Date();
      result.setHours(time.getHours() - 12);
      result.setMinutes(time.getMinutes());
    }
    interp.stack_push(result);
  }

  // ( time -- time )
  word_PM(interp: Interpreter) {
    const time = interp.stack_pop();
    if (!(time instanceof Date)) throw "PM expecting a time";

    let result = time;
    if (time.getHours() < 12) {
      result = new Date();
      result.setHours(time.getHours() + 12);
      result.setMinutes(time.getMinutes());
    }
    interp.stack_push(result);
  }

  // ( -- time )
  word_NOW(interp: Interpreter) {
    const result = new Date();
    interp.stack_push(result);
  }

  // ( item -- time )
  word_to_TIME(interp: Interpreter) {
    const item = interp.stack_pop();
    let result;
    if (item instanceof Date) {
      result = item;
    } else {
      // NB: We need a date in order for Date.parse to succeed. Also assuming str is a time
      const date_string = "Jan 1, 2000 " + item;
      result = new Date(Date.parse(date_string));
    }

    interp.stack_push(result);
  }

  // ( time -- str )
  word_TIME_to_STR(interp: Interpreter) {
    const time = interp.stack_pop();
    const result = time.getHours() + ":" + time.getMinutes();
    interp.stack_push(result);
  }

  // ( str -- date )
  word_to_DATE(interp: Interpreter) {
    const s = interp.stack_pop();
    const result = to_date(s);
    interp.stack_push(result);
  }

  // ( -- date )
  word_TODAY(interp: Interpreter) {
    const result = new Date();
    result.setHours(0, 0, 0, 0);
    interp.stack_push(result);
  }

  // ( -- date )
  word_MONDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(0));
  }

  // ( -- date )
  word_TUESDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(1));
  }

  // ( -- date )
  word_WEDNESDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(2));
  }

  // ( -- date )
  word_THURSDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(3));
  }

  // ( -- date )
  word_FRIDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(4));
  }

  // ( -- date )
  word_SATURDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(5));
  }

  // ( -- date )
  word_SUNDAY(interp: Interpreter) {
    interp.stack_push(GlobalModule.get_day_this_week(6));
  }

  static get_day_this_week(day_of_week) {
    // NOTE: Monday is start of week
    function normalize_day(day) {
      return day;
    }
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const delta_days = (day_of_week - normalize_day(today.getDay())) % 7;

    const result = today;
    result.setDate(result.getDate() + delta_days + 1);
    return result;
  }

  // ( date num-days -- date )
  word_ADD_DAYS(interp: Interpreter) {
    const num_days = interp.stack_pop();
    const date = interp.stack_pop();

    const result = new Date(date);
    result.setDate(result.getDate() + num_days);
    interp.stack_push(result);
  }

  // ( l_date r_date -- num_days )
  word_SUBTRACT_DATES(interp: Interpreter) {
    const r_date = interp.stack_pop();
    const l_date = interp.stack_pop();
    const ms_per_day = 1000 * 60 * 60 * 24;
    const result = Math.round(
      (l_date.getTime() - r_date.getTime()) / ms_per_day,
    );
    interp.stack_push(result);
  }

  // ( date -- str )
  word_DATE_to_STR(interp: Interpreter) {
    const date = interp.stack_pop();
    const result = date_to_string(date);
    interp.stack_push(result);
  }

  // ( date -- int )
  // Converts a date like 2023-11-12 to 20231112
  word_DATE_to_INT(interp: Interpreter) {
    const date = interp.stack_pop();
    const result = date_to_int(date);
    interp.stack_push(result);
  }

  // ( date time -- datetime )
  word_DATE_TIME_to_DATETIME(interp: Interpreter) {
    const time = interp.stack_pop();
    const date = interp.stack_pop();
    const dt_string = `${date.getFullYear()}-${
      date.getMonth() + 1
    }-${date.getDate()} ${time.getHours()}:${time.getMinutes()}`;
    const result = new Date(dt_string);
    interp.stack_push(result);
  }

  // ( datetime -- timestamp )
  word_DATETIME_to_TIMESTAMP(interp: Interpreter) {
    const datetime = interp.stack_pop();
    const result = Math.round(datetime.getTime() / 1000);
    interp.stack_push(result);
  }

  // ( timestamp -- datetime )
  word_TIMESTAMP_to_DATETIME(interp: Interpreter) {
    const timestamp = interp.stack_pop();
    const result = new Date(timestamp * 1000);
    interp.stack_push(result);
  }

  // ( str -- datetime )
  word_STR_to_DATETIME(interp: Interpreter) {
    const s = interp.stack_pop();
    const result = new Date(s);
    interp.stack_push(result);
  }

  // ( str -- timestamp )
  word_STR_to_TIMESTAMP(interp: Interpreter) {
    const s = interp.stack_pop();
    const datetime = new Date(s);
    const result = Math.round(datetime.getTime() / 1000);
    interp.stack_push(result);
  }

  // ( num_days -- days_ago-str )
  word_DAYS_AGO(interp: Interpreter) {
    const num_days = interp.stack_pop();

    let result;
    if (num_days == 0) {
      result = "today";
    } else if (num_days == 1) {
      result = "yesterday";
    } else if (num_days == -1) {
      result = "tomorrow";
    } else if (num_days > 0) {
      result = `${num_days} days ago`;
    } else {
      result = `${Math.abs(num_days)} days from now`;
    }
    interp.stack_push(result);
  }

  // ( a b -- a+b )
  // ( items -- sum )
  word_plus(interp: Interpreter) {
    const items = interp.stack_pop();

    if (items instanceof Array) {
      let sum = 0;
      items.forEach((item) => {
        sum += item;
      });
      interp.stack_push(sum);
    } else {
      const b = items;
      const a = interp.stack_pop();
      interp.stack_push(a + b);
    }
  }

  // ( a b -- a*b )
  word_times(interp: Interpreter) {
    const b = interp.stack_pop();
    let result = 1;
    let numbers = [];
    if (b instanceof Array) {
      numbers = b;
    } else {
      const a = interp.stack_pop();
      numbers = [a, b];
    }
    for (const num of numbers) {
      if (num === null || num === undefined) {
        interp.stack_push(null);
        return;
      }
      result = result * num;
    }
    interp.stack_push(result);
  }

  // ( a b -- a/b )
  word_divide_by(interp: Interpreter) {
    const b = interp.stack_pop();
    const a = interp.stack_pop();
    interp.stack_push(a / b);
  }

  // ( a b -- res )
  word_MOD(interp: Interpreter) {
    const b = interp.stack_pop();
    const a = interp.stack_pop();
    interp.stack_push(a % b);
  }

  // ( numbers -- mean )
  // ( records -- mean_record)
  word_MEAN(interp: Interpreter) {
    const values = interp.stack_pop();

    if (!values || values.length == 0) {
      interp.stack_push(0);
      return;
    }

    function compute_number_mean(numbers) {
      let sum = 0;
      for (const num of numbers) {
        sum += num;
      }
      const res = sum / numbers.length;
      return res;
    }

    function compute_non_number_mean(objects: any[]) {
      const non_null_objects = objects.filter(
        (obj) => obj !== null || obj !== undefined,
      );
      const res: { [key: string]: number } = {};

      non_null_objects.forEach((obj) => {
        const obj_str = is_string(obj) ? obj : JSON.stringify(obj);
        if (res[obj_str]) res[obj_str]++;
        else res[obj_str] = 1;
      });

      Object.entries(res).forEach(([key, value]) => {
        res[key] = value / non_null_objects.length;
      });
      return res;
    }

    function compute_object_mean(records: any[]) {
      const res = {};
      records.forEach((record) => {
        Object.entries(record).forEach(([key, value]) => {
          if (res[key]) res[key].push(value);
          else res[key] = [value];
        });
      });

      Object.entries(res).forEach(([key, values]) => {
        res[key] = compute_mean(values);
      });
      return res;
    }

    function is_all_numbers(values: any[]) {
      return values.every((val) => typeof val == "number");
    }

    function is_all_records(values: any[]) {
      return values.every((val) => is_record(val));
    }

    function select_non_null_values(values: any[]): any {
      return values.filter((val) => val !== null && val !== undefined);
    }

    function compute_mean(values: any): any {
      let result: any;
      if (is_array(values)) {
        const non_null_values = select_non_null_values(values);
        if (is_all_numbers(non_null_values)) {
          result = compute_number_mean(non_null_values);
        } else if (is_all_records(non_null_values)) {
          result = compute_object_mean(non_null_values);
        } else {
          result = compute_non_number_mean(non_null_values);
        }
      } else {
        result = null;
      }
      return result;
    }

    const result = compute_mean(values);
    interp.stack_push(result);
  }

  // ( num1 num2 -- num )
  // ( numbers -- num )
  word_MAX(interp: Interpreter) {
    const num2 = interp.stack_pop();

    let numbers = [];
    if (is_array(num2)) {
      numbers = num2;
    } else {
      const num1 = interp.stack_pop();
      numbers = [num1, num2];
    }

    interp.stack_push(Math.max(...numbers));
  }

  // ( num1 num2 -- num )
  // ( numbers -- num )
  word_MIN(interp: Interpreter) {
    const num2 = interp.stack_pop();

    let numbers = [];
    if (is_array(num2)) {
      numbers = num2;
    } else {
      const num1 = interp.stack_pop();
      numbers = [num1, num2];
    }

    interp.stack_push(Math.min(...numbers));
  }

  // ( num -- int )
  word_ROUND(interp: Interpreter) {
    const num = interp.stack_pop();
    interp.stack_push(Math.round(num));
  }

  // ( l r -- bool )
  word_equal_equal(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();
    interp.stack_push(l == r);
  }

  // ( l r -- bool )
  word_not_equal(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();
    interp.stack_push(l != r);
  }

  // ( l r -- bool )
  word_greater_than(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l > r);
  }

  // ( l r -- bool )
  word_greater_than_or_equal(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l >= r);
  }

  // ( l r -- bool )
  word_less_than(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l < r);
  }

  // ( l r -- bool )
  word_less_than_or_equal(interp: Interpreter) {
    const r = interp.stack_pop();
    const l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l <= r);
  }

  // ( l r -- bool )
  // ( items -- bool )
  word_OR(interp: Interpreter) {
    const r = interp.stack_pop();

    let items;
    if (r instanceof Array) {
      items = r;
    } else {
      const l = interp.stack_pop();
      items = [l, r];
    }
    const result = items.some((item) => item);
    interp.stack_push(result);
  }

  // ( l r -- bool )
  // ( items -- bool )
  word_AND(interp: Interpreter) {
    const r = interp.stack_pop();

    let items;
    if (r instanceof Array) {
      items = r;
    } else {
      const l = interp.stack_pop();
      items = [l, r];
    }
    const result = items.every((item) => item);
    interp.stack_push(result);
  }

  // ( bool -- bool )
  word_NOT(interp: Interpreter) {
    const value = interp.stack_pop();
    interp.stack_push(!value);
  }

  // ( item items -- bool )
  word_IN(interp: Interpreter) {
    let items = interp.stack_pop();
    const item = interp.stack_pop();
    if (!items) items = [];
    const result = items.indexOf(item) >= 0;
    interp.stack_push(result);
  }

  // ( vals required_vals -- bool )
  word_ANY(interp: Interpreter) {
    const required_vals = interp.stack_pop();
    const vals = interp.stack_pop();

    let result = false;
    for (let i = 0; i < required_vals.length; i++) {
      const rv = required_vals[i];
      if (vals.indexOf(rv) >= 0) {
        result = true;
        break;
      }
    }

    // If nothing is required, then all values are true
    if (required_vals.length == 0) result = true;

    interp.stack_push(result);
  }

  // ( vals required_vals -- bool )
  word_ALL(interp: Interpreter) {
    let required_vals = interp.stack_pop();
    let vals = interp.stack_pop();

    if (!vals) vals = [];
    if (!required_vals) required_vals = [];

    const result = required_vals.every((val) => vals.indexOf(val) >= 0);
    interp.stack_push(result);
  }

  // ( item -- bool )
  word_to_BOOL(interp: Interpreter) {
    const item = interp.stack_pop();
    const result = !!item;
    interp.stack_push(result);
  }

  // ( item -- int )
  word_to_INT(interp: Interpreter) {
    const str = interp.stack_pop();
    const result = parseInt(str);
    interp.stack_push(result);
  }

  // ( item -- int )
  word_to_FLOAT(interp: Interpreter) {
    const str = interp.stack_pop();
    const result = parseFloat(str);
    interp.stack_push(result);
  }

  // ( num buckets -- bucket )
  // Each bucket has three elements: [low high value]. If num is >= low and < high, it's in the bucket
  // If a number isn't in any bucket, null is returned
  word_BUCKET(interp: Interpreter) {
    const buckets = interp.stack_pop();
    const num = interp.stack_pop();

    let result = null;
    for (let i = 0; i < buckets.length; i++) {
      const low = buckets[i][0];
      const high = buckets[i][1];
      const value = buckets[i][2];

      if (num >= low && num < high) {
        result = value;
        break;
      }
    }
    if (num == undefined) result = "";
    interp.stack_push(result);
  }

  // ( low high -- int )
  word_UNIFORM_RANDOM(interp: Interpreter) {
    const high = interp.stack_pop();
    const low = interp.stack_pop();

    // From: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
    function getRandomIntInclusive(min, max) {
      min = Math.ceil(min);
      max = Math.floor(max);
      return Math.floor(Math.random() * (max - min + 1) + min); //The maximum is inclusive and the minimum is inclusive
    }
    const result = getRandomIntInclusive(low, high);
    interp.stack_push(result);
  }

  // ( val start_ranges -- index )
  word_RANGE_INDEX(interp: Interpreter) {
    const start_ranges = interp.stack_pop();
    const val = interp.stack_pop();

    // Cap off the value ranges with infinity
    start_ranges.push(Infinity);

    if (val === null || !start_ranges) {
      interp.stack_push(null);
      return;
    }

    if (val < start_ranges[0]) {
      interp.stack_push(null);
      return;
    }

    let result = null;
    for (let i = 0; i < start_ranges.length - 1; i++) {
      if (val >= start_ranges[i] && val < start_ranges[i + 1]) {
        result = i;
        break;
      }
    }

    interp.stack_push(result);
  }

  // ( items start_ranges fvalue -- value_by_bucket )
  async word_RANGE_BUCKETS(interp: Interpreter) {
    const fvalue = interp.stack_pop();
    const string_location = interp.get_string_location();

    const start_ranges = interp.stack_pop();
    const items = interp.stack_pop();

    if (!items || !start_ranges) {
      interp.stack_push({});
      return;
    }

    const result = { BEFORE: [] };
    for (const range_start of start_ranges) {
      result[range_start] = [];
    }

    for (const item of items) {
      interp.stack_push(item);
      await interp.run(fvalue, string_location);
      const value = interp.stack_pop();
      let bucket = "BEFORE";
      for (const range_start of start_ranges) {
        if (value < range_start) {
          break;
        }

        bucket = range_start;

        if (value === range_start) {
          break;
        }
      }
      result[bucket].push(item);
    }

    interp.stack_push(result);
  }

  // ( -- Infinity )
  async word_INFINITY(interp: Interpreter) {
    interp.stack_push(Infinity);
  }


  // ( -- )
  word_bang_PUSH_ERROR(interp: Interpreter) {
    interp.modify_flags(this.module_id, { push_error: true });
  }

  // ( -- )
  word_bang_WITH_KEY(interp: Interpreter) {
    interp.modify_flags(this.module_id, { with_key: true });
  }

  // (comparator -- )
  //
  // `comparator` may be a Forthic string or a Python key function
  word_bang_COMPARATOR(interp: Interpreter) {
    const comparator = interp.stack_pop();
    interp.modify_flags(this.module_id, { comparator });
  }

  // ( -- )
  word_bang_PUSH_REST(interp: Interpreter) {
    interp.modify_flags(this.module_id, { push_rest: true });
  }

  // (depth -- )
  //
  // NOTE: `depth` of 0 is the same not having set depth
  word_bang_DEPTH(interp: Interpreter) {
    const depth = interp.stack_pop();
    interp.modify_flags(this.module_id, { depth });
  }

  // ( bool -- )
  word_bang_OVERWRITE(interp: Interpreter) {
    const overwrite = interp.stack_pop();
    interp.modify_flags(this.module_id, { overwrite });
  }

  // ( delay_ms -- )
  word_bang_DELAY(interp: Interpreter) {
    const delay_ms = interp.stack_pop();
    interp.modify_flags(this.module_id, { delay: delay_ms });
  }

  // ( num_interps -- )
  word_bang_INTERPS(interp: Interpreter) {
    const num_interps = interp.stack_pop();
    interp.modify_flags(this.module_id, { interps: num_interps });
  }

  // ( label -- )
  word_bang_NOTE_PROGRESS(interp: Interpreter) {
    const label = interp.stack_pop();
    interp.modify_flags(this.module_id, { note_progress: label });
  }

  // (str -- encoded)
  word_URL_ENCODE(interp: Interpreter) {
    const str = interp.stack_pop();
    let result = "";
    if (str) result = encodeURIComponent(str);
    interp.stack_push(result);
  }

  // (urlencoded -- decoded)
  word_URL_DECODE(interp: Interpreter) {
    const urlencoded = interp.stack_pop();
    let result = "";
    if (urlencoded) result = decodeURIComponent(urlencoded);
    interp.stack_push(result);
  }

  // ( -- char)
  word_QUOTE_CHAR(interp: Interpreter) {
    interp.stack_push(DLE);
  }

  // ( string -- quoted_string)
  word_QUOTED(interp: Interpreter) {
    const string = interp.stack_pop();
    let clean_string = "";
    for (let i = 0; i < string.length; i++) {
      let c = string[i];
      if (c == DLE) c = " ";
      clean_string += c;
    }
    const result = `${DLE}${clean_string}${DLE}`;
    interp.stack_push(result);
  }

  // ( object -- )
  word_CONSOLE_LOG(interp: Interpreter) {
    const object = interp.stack_pop();
    console.log(object);
    interp.stack_push(object);
  }

  // ( -- )
  word_PROFILE_START(interp: Interpreter) {
    interp.start_profiling();
  }

  // ( -- )
  word_PROFILE_END(interp: Interpreter) {
    interp.stop_profiling();
  }

  // ( label -- )
  word_PROFILE_TIMESTAMP(interp: Interpreter) {
    const label = interp.stack_pop();
    interp.add_timestamp(label);
  }

  // ( -- )
  word_PROFILE_DATA(interp: Interpreter) {
    const histogram = interp.word_histogram();
    const timestamps = interp.profile_timestamps();

    const result = {
      word_counts: [],
      timestamps: [],
    };
    histogram.forEach((val) => {
      const rec = { word: val["word"], count: val["count"] };
      result["word_counts"].push(rec);
    });

    let prev_time = 0.0;
    timestamps.forEach((t) => {
      const rec = {
        label: t["label"],
        time_ms: t["time_ms"],
        delta: t["time_ms"] - prev_time,
      };
      prev_time = t["time_ms"];
      result["timestamps"].push(rec);
    });

    interp.stack_push(result);
  }

  // ( -- null )
  word_NULL(interp: Interpreter) {
    interp.stack_push(null);
  }

  // ( value default_value -- value )
  word_DEFAULT(interp: Interpreter) {
    const default_value = interp.stack_pop();
    let value = interp.stack_pop();
    if (value === undefined || value === null || value === "")
      value = default_value;
    interp.stack_push(value);
  }

  // ( value default_forthic -- value )
  async word_star_DEFAULT(interp: Interpreter) {
    const default_forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    let value = interp.stack_pop();

    if (value === undefined || value === null || value === "") {
      await interp.run(default_forthic, string_location);
      value = interp.stack_pop();
    }
    interp.stack_push(value);
  }

  // ( Record default_key/vals -- Record )
  word_REC_DEFAULTS(interp: Interpreter) {
    const key_vals = interp.stack_pop();
    const record = interp.stack_pop();
    key_vals.forEach((key_val) => {
      const key = key_val[0];
      const value = record[key];
      if (value === undefined || value === null || value == "") {
        record[key] = key_val[1];
      }
    });

    interp.stack_push(record);
  }

  // ( item forthic num-times -- ? )
  async word_l_REPEAT(interp: Interpreter) {
    const num_times = interp.stack_pop();
    const forthic = interp.stack_pop();
    const string_location = interp.get_string_location();

    for (let i = 0; i < num_times; i++) {
      // Store item so we can push it back later
      const item = interp.stack_pop();
      interp.stack_push(item);

      await interp.run(forthic, string_location);
      const res = interp.stack_pop();

      // Push original item and result
      interp.stack_push(item);
      interp.stack_push(res);
    }
  }

  // ( a -- a )
  async word_IDENTITY(_interp: Interpreter) {}

  // ( value num_places -- str )
  word_to_FIXED(interp: Interpreter) {
    const num_places = interp.stack_pop();
    const value = interp.stack_pop();
    let result = value;
    if (value === undefined || value === null) result = "";
    else if (!isNaN(value)) result = value.toFixed(num_places);
    interp.stack_push(result);
  }

  // ( object -- json )
  word_to_JSON(interp: Interpreter) {
    const object = interp.stack_pop();
    const result = JSON.stringify(object);
    interp.stack_push(result);
  }

  // ( json -- json )
  word_PRETTIFY(interp: Interpreter) {
    const json = interp.stack_pop();
    const result = pretty_print(json);
    interp.stack_push(result);
  }

  // ( json -- object )
  word_JSON_to(interp: Interpreter) {
    const str = interp.stack_pop();
    let result = null;
    if (str) result = JSON.parse(str);
    interp.stack_push(result);
  }

  // ( name -- ? )
  async word_LOAD_SCREEN(interp: Interpreter) {
    const name = interp.stack_pop();
    const screen_forthic = interp.get_screen_forthic(name);
    const location = new CodeLocation({ screen_name: name });
    // await interp.run(screen_forthic, location);
    await interp.run(screen_forthic, location);
  }

  // ( -- )
  word_dot_s(interp: Interpreter) {
    const stack = interp.get_stack();
    if (stack.length > 0) {
      console.log(stack[stack.length - 1]);
    }
    else {
      console.log("<STACK EMPTY>");
    }
    throw new IntentionalStopError(".s");
  }

  // ( -- )
  word_dot_S(interp: Interpreter) {
    const stack = interp.get_stack().reverse();
    console.log(JSON.stringify(stack, null, 2));
    throw new IntentionalStopError(".S");
  }

  // ( a b -- a - b )
  word_minus(interp: Interpreter) {
    const b = interp.stack_pop();
    const a = interp.stack_pop();
    interp.stack_push(a - b);
  }
}

// Descends into record using an array of fields, returning final value or null
function drill_for_value(record: any, fields: string[]): any {
  let result = record;
  for (let i = 0; i < fields.length; i++) {
    const f = fields[i];
    if (result == null) return null;
    result = result[f];
  }
  if (result === undefined) result = null;
  return result;
}

async function execute_returning_error(
  interp: Interpreter,
  forthic: string,
  string_location: CodeLocation,
): Promise<any> {
  let result = null;
  try {
    await interp.run(forthic, string_location);
  } catch (e) {
    result = e;
  }
  return result;
}

function fully_flatten_array(items: any[], accum: any): any {
  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    if (item instanceof Array) fully_flatten_array(item, accum);
    else accum.push(item);
  }
  return accum;
}

function flatten_array(items: any[], depth: number, accum = []): any {
  if (depth === undefined) return fully_flatten_array(items, accum);
  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    if (depth > 0 && item instanceof Array)
      flatten_array(item, depth - 1, accum);
    else accum.push(item);
  }
  return accum;
}