import re
import os
import random
import math
import pytz
import pdb
import datetime
from dateutil import parser
import urllib
import json
import io
import markdown
import csv
from collections import defaultdict

from .module import Module, PushValueWord
from .profile import ProfileAnalyzer
from .interfaces import IInterpreter

from typing import Optional, Union, Any, List

DLE = chr(16)   # ASCII DLE char


class StackDump(RuntimeError):
    pass


class GlobalModuleError(RuntimeError):
    pass


class InvalidTimeError(GlobalModuleError):
    pass


# TODO: Ensure that None flows through all words properly

class GlobalModule(Module):
    """This implements the standard `global` module words

    The `GlobalModule` is a special module because it always the last one searched for Forthic words. Because
    of this, it is also responsible for handling "literal words" that push themselves onto the stack. These
    are words like "1", "2.5", "06-05-2021", etc.

    The `GlobalModule` also implements base words that might usually be built into the language, like
    `VARIABLES`, `!`, `@`, etc.

    See `docs/modules/global_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp, timezone):
        super().__init__('<GLOBAL>', interp)
        self.timezone = timezone

        # "Screens" of Forthic code can be loaded from disk/memory. Since screens can load other screens,
        # we need to be careful not to get into a loop. The `active_screens` keeps track of this.
        self.active_screens = set()

        # `literal_handlers` convert tokens into values when no other words can be found.
        # A Forthic interpreter can be customized here to recoginize domain-specific literals.
        self.literal_handlers = [
            self.to_bool,
            self.to_int,
            self.to_float,
            self.to_date,
            self.to_time,
        ]

        # Module Flags: These are all None but are settable for one-time use to change the behavior
        # of module words
        self.flags = {
            "with_key": None,
            "push_error": None,
            "comparator": None,
            "push_rest": None,
            "depth": None,
        }

        # ----------------
        # Base words
        self.add_module_word('VARIABLES', self.word_VARIABLES)
        self.add_module_word('!', self.word_bang)
        self.add_module_word('@', self.word_at)
        self.add_module_word('!@', self.word_bang_at)
        self.add_module_word('<!', self.word_l_bang)
        self.add_module_word('INTERPRET', self.word_INTERPRET)
        self.add_module_word('EXPORT', self.word_EXPORT)
        self.add_module_word('USE-MODULES', self.word_USE_MODULES)
        self.add_module_word('REC', self.word_REC)
        self.add_module_word('REC@', self.word_REC_at)
        self.add_module_word('<REC!', self.word_l_REC_bang)
        self.add_module_word('SCREEN!', self.word_SCREEN_bang)
        self.add_module_word('SCREEN', self.word_SCREEN)
        self.add_module_word('LOAD-SCREEN', self.word_LOAD_SCREEN)

        # ----------------
        # Array/Record words
        self.add_module_word('APPEND', self.word_APPEND)
        self.add_module_word('REVERSE', self.word_REVERSE)
        self.add_module_word('UNIQUE', self.word_UNIQUE)
        self.add_module_word('<DEL', self.word_L_DEL)
        self.add_module_word('RELABEL', self.word_RELABEL)
        self.add_module_word('BY-FIELD', self.word_BY_FIELD)
        self.add_module_word('GROUP-BY-FIELD', self.word_GROUP_BY_FIELD)
        self.add_module_word('GROUP-BY', self.word_GROUP_BY)

        self.add_module_word('GROUPS-OF', self.word_GROUPS_OF)
        self.add_module_word('INDEX', self.word_INDEX)
        self.add_module_word('MAP', self.word_MAP)
        self.add_module_word('FOREACH', self.word_FOREACH)
        self.add_module_word('INVERT-KEYS', self.word_INVERT_KEYS)
        self.add_module_word('ZIP', self.word_ZIP)
        self.add_module_word('ZIP-WITH', self.word_ZIP_WITH)
        self.add_module_word('KEYS', self.word_KEYS)
        self.add_module_word('VALUES', self.word_VALUES)
        self.add_module_word('LENGTH', self.word_LENGTH)
        self.add_module_word('RANGE', self.word_RANGE)
        self.add_module_word('SLICE', self.word_SLICE)
        self.add_module_word('DIFFERENCE', self.word_DIFFERENCE)
        self.add_module_word('INTERSECTION', self.word_INTERSECTION)
        self.add_module_word('UNION', self.word_UNION)
        self.add_module_word('SELECT', self.word_SELECT)
        self.add_module_word('TAKE', self.word_TAKE)
        self.add_module_word('DROP', self.word_DROP)
        self.add_module_word('ROTATE', self.word_ROTATE)
        self.add_module_word('SHUFFLE', self.word_SHUFFLE)
        self.add_module_word('SORT', self.word_SORT)
        self.add_module_word('FIELD-KEY-FUNC', self.word_FIELD_KEY_FUNC)
        self.add_module_word('NTH', self.word_NTH)
        self.add_module_word('LAST', self.word_LAST)
        self.add_module_word('UNPACK', self.word_UNPACK)
        self.add_module_word('FLATTEN', self.word_FLATTEN)
        self.add_module_word('KEY-OF', self.word_KEY_OF)
        self.add_module_word('REDUCE', self.word_REDUCE)
        self.add_module_word('CUMULATIVE-DIST', self.word_CUMULATIVE_DIST)

        # ----------------
        # Stack words
        self.add_module_word('POP', self.word_POP)
        self.add_module_word('DUP', self.word_DUP)
        self.add_module_word('SWAP', self.word_SWAP)

        # ----------------
        # String words
        self.add_module_word('CONCAT', self.word_CONCAT)
        self.add_module_word('SPLIT', self.word_SPLIT)
        self.add_module_word('JOIN', self.word_JOIN)
        self.add_module_word('/N', self.word_slash_N)
        self.add_module_word('/R', self.word_slash_R)
        self.add_module_word('/T', self.word_slash_T)
        self.add_module_word('LOWERCASE', self.word_LOWERCASE)
        self.add_module_word('UPPERCASE', self.word_UPPERCASE)
        self.add_module_word('ASCII', self.word_ASCII)
        self.add_module_word('STRIP', self.word_STRIP)
        self.add_module_word('REPLACE', self.word_REPLACE)
        self.add_module_word('RE-REPLACE', self.word_RE_REPLACE)
        self.add_module_word('RE-MATCH', self.word_RE_MATCH)
        self.add_module_word('RE-MATCH-ALL', self.word_RE_MATCH_ALL)
        self.add_module_word('RE-MATCH-GROUP', self.word_RE_MATCH_GROUP)
        self.add_module_word('>STR', self.word_to_STR)
        self.add_module_word('URL-ENCODE', self.word_URL_ENCODE)
        self.add_module_word('URL-DECODE', self.word_URL_DECODE)

        # ----------------
        # Tree words
        self.add_module_word('TRAVERSE-DEPTH-FIRST', self.word_TRAVERSE_DEPTH_FIRST)
        self.add_module_word('SUBTREES', self.word_SUBTREES)

        # ----------------
        # Misc words
        self.add_module_word('NULL', self.word_NULL)
        self.add_module_word('QUOTE-CHAR', self.word_QUOTE_CHAR)
        self.add_module_word('QUOTED', self.word_QUOTED)
        self.add_module_word('DEFAULT', self.word_DEFAULT)
        self.add_module_word('*DEFAULT', self.word_star_DEFAULT)
        self.add_module_word('<REPEAT', self.word_l_REPEAT)
        self.add_module_word('IDENTITY', self.word_IDENTITY)
        self.add_module_word('>FIXED', self.word_to_FIXED)

        # TODO: Add support for serializing dates and datetimes
        self.add_module_word('>JSON', self.word_to_JSON)
        self.add_module_word('JSON>', self.word_JSON_to)

        self.add_module_word('>TSV', self.word_to_TSV)
        self.add_module_word('TSV>', self.word_TSV_to)
        self.add_module_word('RECS>TSV', self.word_RECS_to_TSV)
        self.add_module_word('TSV>RECS', self.word_TSV_to_RECS)
        self.add_module_word('.s', self.word_dot_s)

        # ----------------
        # Date/time words
        self.add_module_word('AM', self.word_AM)
        self.add_module_word('PM', self.word_PM)
        self.add_module_word('NOW', self.word_NOW)
        self.add_module_word('>TIME', self.word_to_TIME)
        self.add_module_word('<TZ!', self.word_l_TZ_bang)
        self.add_module_word('TIME>STR', self.word_TIME_to_STR)
        self.add_module_word('>DATE', self.word_to_DATE)
        self.add_module_word('TODAY', self.word_TODAY)
        self.add_module_word('MONDAY', self.word_MONDAY)
        self.add_module_word('TUESDAY', self.word_TUESDAY)
        self.add_module_word('WEDNESDAY', self.word_WEDNESDAY)
        self.add_module_word('THURSDAY', self.word_THURSDAY)
        self.add_module_word('FRIDAY', self.word_FRIDAY)
        self.add_module_word('SATURDAY', self.word_SATURDAY)
        self.add_module_word('SUNDAY', self.word_SUNDAY)
        self.add_module_word('NEXT', self.word_NEXT)

        self.add_module_word('ADD-DAYS', self.word_ADD_DAYS)
        self.add_module_word('SUBTRACT-DATES', self.word_SUBTRACT_DATES)
        self.add_module_word('SUBTRACT-TIMES', self.word_SUBTRACT_TIMES)
        self.add_module_word('DATE>STR', self.word_DATE_to_STR)
        self.add_module_word('DATE-TIME>DATETIME', self.word_DATE_TIME_to_DATETIME)
        self.add_module_word('DATETIME>TIMESTAMP', self.word_DATETIME_to_TIMESTAMP)
        self.add_module_word('TIMESTAMP>DATETIME', self.word_TIMESTAMP_to_DATETIME)
        self.add_module_word('STR>DATETIME', self.word_STR_to_DATETIME)

        # ----------------
        # Math words
        self.add_module_word('+', self.word_plus)
        self.add_module_word('-', self.word_minus)
        self.add_module_word('*', self.word_times)
        self.add_module_word('/', self.word_divide_by)
        self.add_module_word('MOD', self.word_MOD)
        self.add_module_word('ROUND', self.word_ROUND)
        self.add_module_word('MAX', self.word_MAX)
        self.add_module_word('MIN', self.word_MIN)
        self.add_module_word('==', self.word_equal_equal)
        self.add_module_word('!=', self.word_not_equal)
        self.add_module_word('>', self.word_greater_than)
        self.add_module_word('>=', self.word_greater_than_or_equal)
        self.add_module_word('<', self.word_less_than)
        self.add_module_word('<=', self.word_less_than_or_equal)
        self.add_module_word('OR', self.word_OR)
        self.add_module_word('AND', self.word_AND)
        self.add_module_word('NOT', self.word_NOT)
        self.add_module_word('IN', self.word_IN)
        self.add_module_word('ANY', self.word_ANY)
        self.add_module_word('ALL', self.word_ALL)
        self.add_module_word('>BOOL', self.word_to_BOOL)
        self.add_module_word('>INT', self.word_to_INT)
        self.add_module_word('>FLOAT', self.word_to_FLOAT)
        self.add_module_word('UNIFORM-RANDOM', self.word_UNIFORM_RANDOM)
        self.add_module_word('RANGE-INDEX', self.word_RANGE_INDEX)

        # ----------------
        # Flag words
        self.add_module_word('!PUSH-ERROR', self.word_bang_PUSH_ERROR)
        self.add_module_word('!WITH-KEY', self.word_bang_WITH_KEY)
        self.add_module_word('!COMPARATOR', self.word_bang_COMPARATOR)
        self.add_module_word('!PUSH-REST', self.word_bang_PUSH_REST)
        self.add_module_word('!DEPTH', self.word_bang_DEPTH)

        # ----------------
        # Profiling words
        self.add_module_word('PROFILE-START', self.word_PROFILE_START)
        self.add_module_word('PROFILE-TIMESTAMP', self.word_PROFILE_TIMESTAMP)
        self.add_module_word('PROFILE-END', self.word_PROFILE_END)
        self.add_module_word('PROFILE-DATA', self.word_PROFILE_DATA)
        self.add_module_word('PROFILE-REPORT', self.word_PROFILE_REPORT)

        # ----------------
        # Python-only words
        self.add_module_word('CURRENT-USER', self.word_CURRENT_USER)
        self.add_module_word('MARKDOWN>HTML', self.word_MARKDOWN_to_HTML)

    def find_word(self, name: str):
        """Searches the global module for a word, trying literals if no word can be found"""
        result = super().find_word(name)
        if result is None:
            result = self.find_literal_word(name)
        return result

    def find_literal_word(self, string: str):
        """Converts a string into a literal using one of the registered converters"""
        for handler in self.literal_handlers:
            value = handler(string)
            if value is not None:
                return PushValueWord(string, value)
        return None

    # --------------------------------------------------------------------------
    # Literal handlers

    def to_bool(self, str_val: str) -> Optional[bool]:
        """If str_val can be converted to bool, return value; otherwise None"""
        result = None
        if str_val == 'TRUE':
            result = True
        elif str_val == 'FALSE':
            result = False
        return result

    def to_int(self, str_val: str) -> Optional[int]:
        """If str_val can be converted to int, return value; otherwise None"""
        try:
            result = int(str_val)
        except ValueError:
            return None
        return result

    def to_float(self, str_val: str) -> Optional[float]:
        """If str_val can be converted to float, return value; otherwise None"""
        try:
            result = float(str_val)
        except ValueError:
            return None
        return result

    def to_date(self, str_val: str) -> Optional[datetime.date]:
        """If str_val can be converted to date, return value; otherwise None"""
        match = re.match(r'(\d{4})-(\d{2})-(\d{2})', str_val)
        if not match:
            return None

        year = int(match.group(1))
        month = int(match.group(2))
        day = int(match.group(3))
        result = datetime.date(year, month, day)
        return result

    def to_time(self, str_val: str) -> Optional[datetime.time]:
        """If str_val can be converted to time, return value; otherwise None"""
        match = re.match(r'(\d{1,2}):(\d{2})', str_val)
        if not match:
            return None

        hour = int(match.group(1))
        minute = int(match.group(2))
        if hour > 23 or minute > 60:
            return None
        result = datetime.time(hour, minute, tzinfo=self.timezone)
        return result

    # --------------------------------------------------------------------------
    # Word handlers

    # ( varnames -- )
    def word_VARIABLES(self, interp: IInterpreter):
        """Creates a new variable in the current module"""
        varnames = interp.stack_pop()
        module = interp.cur_module()
        for v in varnames:
            module.add_variable(v)

    # ( value variable -- )
    def word_bang(self, interp: IInterpreter):
        """Sets the value of a variable"""
        variable = interp.stack_pop()
        value = interp.stack_pop()
        variable.value = value

    # ( variable -- value )
    def word_at(self, interp: IInterpreter):
        """Pushes variable's value onto the stack"""
        variable = interp.stack_pop()
        interp.stack_push(variable.value)

    # ( value variable -- value )
    def word_bang_at(self, interp: IInterpreter):
        """Set the value of a variable and then pushes variable's value onto the stack"""
        variable = interp.stack_pop()
        value = interp.stack_pop()
        variable.value = value
        interp.stack_push(variable.value)

    # ( value variable -- variable )
    def word_l_bang(self, interp: IInterpreter):
        """Set the value of a variable and then pushes variable onto the stack"""
        variable = interp.stack_pop()
        value = interp.stack_pop()
        variable.value = value
        interp.stack_push(variable)

    # ( object -- ? )
    def word_INTERPRET(self, interp: IInterpreter):
        """Pops a string/Lambda and interprets it"""
        obj = interp.stack_pop()

        if not obj:
            return

        execute(interp, obj)

    # ( names -- )
    def word_EXPORT(self, interp: IInterpreter):
        names = interp.stack_pop()
        interp.cur_module().add_exportable(names)

    # ( names -- )
    def word_USE_MODULES(self, interp: IInterpreter):
        names = interp.stack_pop()

        cur_module = interp.cur_module()
        if cur_module != interp.app_module:
            raise GlobalModuleError(
                'USE-MODULES can only be called within the app module'
            )

        for name in names:
            module_name = name
            prefix = name

            if isinstance(name, list):
                module_name = name[0]
                prefix = name[1]

            module = interp.find_module(module_name)
            interp.app_module.import_module(prefix, module, interp)

    # ( key_vals -- rec )
    # key_vals is an array of [key val] pairs
    def word_REC(self, interp: IInterpreter):
        key_vals = interp.stack_pop()

        if not key_vals:
            key_vals = []

        result = {}
        for pair in key_vals:
            key = None
            val = None
            if pair:
                if len(pair) >= 1:
                    key = pair[0]
                if len(pair) >= 2:
                    val = pair[1]
            result[key] = val
        interp.stack_push(result)

    # ( rec field -- value )
    # ( rec fields -- value )
    def word_REC_at(self, interp: IInterpreter):
        field = interp.stack_pop()
        rec = interp.stack_pop()

        if not rec:
            interp.stack_push(None)
            return

        if isinstance(field, list):
            fields = field
        else:
            fields = [field]

        result = drill_for_value(rec, fields)
        interp.stack_push(result)

    # ( rec value field -- rec )
    def word_l_REC_bang(self, interp: IInterpreter):
        field = interp.stack_pop()
        value = interp.stack_pop()
        rec = interp.stack_pop()

        if not rec:
            rec = {}

        if isinstance(field, list):
            fields = field
        else:
            fields = [field]

        def ensure_field(rec, field):
            res = rec.get(field)
            if not res:
                res = {}
                rec[field] = res
            return res

        cur_rec = rec
        for f in fields[:-1]:   # Drill down up until the last value
            cur_rec = ensure_field(cur_rec, f)

        # Set the value at the right depth within rec
        cur_rec[fields[-1]] = value

        interp.stack_push(rec)

    # ( content name -- )
    def word_SCREEN_bang(self, interp: IInterpreter):
        """Stores a screen in the application module"""
        name = interp.stack_pop()
        content = interp.stack_pop()
        interp.app_module.set_screen(name, content)

    # ( name -- content )
    def word_SCREEN(self, interp: IInterpreter):
        """Returns screen stored in application module"""
        name = interp.stack_pop()
        result = interp.app_module.get_screen(name)
        interp.stack_push(result)

    # ( name -- ? )
    def word_LOAD_SCREEN(self, interp: IInterpreter):
        """Runs screen"""
        name = interp.stack_pop()
        if name in self.active_screens:
            raise GlobalModuleError(
                f"Can't load screen '{name}' because it is currently being loaded"
            )

        screen = interp.app_module.get_screen(name)

        self.active_screens.add(name)
        interp.run_in_module(interp.app_module, screen)
        self.active_screens.remove(name)

    # ( array item -- array )
    # ( record key/val -- record )
    def word_APPEND(self, interp: IInterpreter):
        item = interp.stack_pop()
        result = interp.stack_pop()

        if not result:
            result = []

        if isinstance(result, list):
            result.append(item)
        else:   # If not a list, treat as record
            result[item[0]] = item[1]

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- record )
    def word_REVERSE(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            interp.stack_push(container)
            return

        def reverse_record(rec):
            res = {}
            for pair in reversed(rec.items()):
                res[pair[0]] = pair[1]
            return res

        if isinstance(container, list):
            result = list(reversed(container))
        else:   # If not a list, treat as record
            result = reverse_record(container)

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- record )
    # NOTE: If record, assuming its values are hashable
    def word_UNIQUE(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            interp.stack_push(container)
            return

        def invert_record(record):
            res = {}
            for k, v in record.items():
                res[v] = k
            return res

        if isinstance(container, list):
            result = list(set(container))
        else:   # If not a list, treat as record
            result = invert_record(invert_record(container))

        interp.stack_push(result)

    # ( array index -- array )
    # ( record key -- record )
    def word_L_DEL(self, interp: IInterpreter):
        key = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            interp.stack_push(container)
            return

        if isinstance(container, list):
            del container[key]
        else:
            if key in container:
                del container[key]
        interp.stack_push(container)

    # ( array old_keys new_keys -- array )
    # ( record old_keys new_keys -- record )
    def word_RELABEL(self, interp: IInterpreter):
        new_keys = interp.stack_pop()
        old_keys = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            interp.stack_push(container)
            return

        if len(old_keys) != len(new_keys):
            raise GlobalModuleError(
                'RELABEL: old_keys and new_keys must be same length'
            )

        new_to_old = {}
        for i in range(len(old_keys)):
            new_to_old[new_keys[i]] = old_keys[i]

        if isinstance(container, list):
            result: Any = []
            for key in sorted(new_to_old):
                result.append(container[new_to_old[key]])
        else:
            result = {}
            for key in new_to_old:
                result[key] = container.get(new_to_old[key])

        interp.stack_push(result)

    # ( array field -- field_to_item )
    # ( record field -- field_to_item )
    def word_BY_FIELD(self, interp: IInterpreter):
        field = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            values = container
        else:
            values = container.values()

        result = {}
        for v in values:
            if v is not None:
                result[v.get(field)] = v

        interp.stack_push(result)

    # ( array field -- field_to_items )
    # ( record field -- field_to_items )
    def word_GROUP_BY_FIELD(self, interp: IInterpreter):
        field = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            values = container
        else:
            values = container.values()

        result = defaultdict(list)
        for v in values:
            field_value = None
            if v is not None:
                field_value = v.get(field)

            if field_value is None:
                field_value = ""

            if isinstance(field_value, list):
                for fv in field_value:
                    result[fv].append(v)
            else:
                result[field_value].append(v)

        interp.stack_push(result)

    # ( array forthic -- group_to_items )
    # ( record forthic -- group_to_items )
    #
    # Flagged behavior:
    #    with_key: Pushes container key in addition to container value before executing Forthic
    def word_GROUP_BY(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        container = interp.stack_pop()

        flags = self.get_flags()

        if not container:
            container = []

        if isinstance(container, list):
            keys: Any = range(len(container))
            values = container
        else:
            keys = list(container.keys())
            values = list(container.values())

        result = defaultdict(list)
        for i in range(len(values)):
            key = keys[i]
            v = values[i]
            if flags.get('with_key'):
                interp.stack_push(key)
            interp.stack_push(v)
            execute(interp, forthic)
            group = interp.stack_pop()
            result[group].append(v)

        interp.stack_push(result)

    # ( array n -- arrays )
    # ( record n -- records )
    def word_GROUPS_OF(self, interp: IInterpreter):
        size = interp.stack_pop()
        container = interp.stack_pop()
        if size <= 0:
            raise GlobalModuleError('GROUPS-OF requires group size > 0')

        if not container:
            container = []

        def group_items(items, group_size):
            num_groups = math.ceil(len(items) / group_size)
            res = []
            remaining = items[:]
            for _ in range(num_groups):
                res.append(remaining[0:group_size])
                remaining = remaining[group_size:]
            return res

        def extract_rec(record, keys):
            res = {}
            for k in keys:
                res[k] = record[k]
            return res

        if isinstance(container, list):
            result = group_items(container, size)
        else:
            keys = list(container.keys())
            key_groups = group_items(keys, size)
            result = [extract_rec(container, ks) for ks in key_groups]

        interp.stack_push(result)

    # ( array forthic -- record )
    def word_INDEX(self, interp: IInterpreter):
        forthic = interp.stack_pop()  # Returns a list of string keys
        items = interp.stack_pop()

        if not items:
            interp.stack_push(items)
            return

        result = defaultdict(list)
        for item in items:
            interp.stack_push(item)
            execute(interp, forthic)
            keys = interp.stack_pop()
            for k in keys:
                result[k.lower()].append(item)

        interp.stack_push(result)

    # ( array forthic -- array )
    # ( record forthic -- record )
    #
    # Flagged behavior:
    #    * with_key: Pushes key in addition to value
    #    * push_error: If an error occurs while mapping over an element, push None onto the stack and gather the error.
    #                  At the end of the mapping, push the errors onto the stack
    def word_MAP(self, interp: IInterpreter):
        # Get the args
        forthic = interp.stack_pop()
        items = interp.stack_pop()

        # Get flags
        flags = self.get_flags()

        depth = flags.get('depth')
        if not depth:
            depth = 0

        # Early exit if no items
        if not items:
            interp.stack_push(items)
            return

        # This maps the forthic over an item, storing errors if needed
        def map_value(key, value, errors):
            if flags.get('with_key'):
                interp.stack_push(key)
            interp.stack_push(value)

            if flags.get('push_error'):
                error = None
                try:
                    execute(interp, forthic)
                except Exception as e:
                    interp.stack_push(None)
                    error = e
                errors.append(error)
            else:
                execute(interp, forthic)

            return interp.stack_pop()

        # This recursively descends a record structure
        def descend_record(record, depth, accum, errors):
            for k, item in record.items():
                if depth > 0:
                    if isinstance(item, list):
                        accum[k] = []
                        descend_list(item, depth - 1, accum[k], errors)
                    else:
                        accum[k] = {}
                        descend_record(item, depth - 1, accum[k], errors)
                else:
                    accum[k] = map_value(k, item, errors)
            return accum

        # This recursively descends a list
        def descend_list(items, depth, accum, errors):
            for i in range(len(items)):
                item = items[i]
                if depth > 0:
                    if isinstance(item, list):
                        accum.append([])
                        descend_list(item, depth - 1, accum[-1], errors)
                    else:
                        accum.append({})
                        descend_record(item, depth - 1, accum[-1], errors)
                else:
                    accum.append(map_value(i, item, errors))
            return accum

        errors: Any = []
        result: Any = []
        if isinstance(items, list):
            result = descend_list(items, depth, [], errors)
        else:
            result = descend_record(items, depth, {}, errors)

        # Return results
        interp.stack_push(result)
        if flags.get('push_error'):
            interp.stack_push(errors)

    # ( items forthic -- ? )
    # ( record forthic -- ? )
    #
    # Flagged behavior
    #    * with_key: Pushes key in addition to value when executing Forthic
    #    * push_error: After execution, push an array of errors onto stack corresponding to each element
    #                  in the specified container
    def word_FOREACH(self, interp: IInterpreter):
        flags = self.get_flags()
        foreach(interp, flags)

    # ( record -- record )
    # Swaps the order of nested keys in a record
    def word_INVERT_KEYS(self, interp: IInterpreter):
        record = interp.stack_pop()
        result: Any = defaultdict(dict)
        for first_key, sub_record in record.items():
            for second_key, value in sub_record.items():
                result[second_key][first_key] = value
        interp.stack_push(result)

    # ( array1 array2 -- array )
    # ( record1 record2 -- record )
    def word_ZIP(self, interp: IInterpreter):
        container2 = interp.stack_pop()
        container1 = interp.stack_pop()

        if not container1:
            container1 = []

        if not container2:
            container2 = []

        if isinstance(container2, list):
            result: Any = []
            for i in range(len(container1)):
                value2 = container2[i] if i < len(container2) else None
                result.append([container1[i], value2])
        else:
            result = {}
            for k, v in container1.items():
                result[k] = [v, container2.get(k)]

        interp.stack_push(result)

    # ( array1 array2 forthic -- array )
    # ( record1 record2 forthic -- record )
    def word_ZIP_WITH(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        container2 = interp.stack_pop()
        container1 = interp.stack_pop()

        if not container1:
            container1 = []

        if not container2:
            container2 = []

        if isinstance(container2, list):
            result: Any = []
            for i in range(len(container1)):
                value1 = container1[i]
                value2 = container2[i] if i < len(container2) else None
                interp.stack_push(value1)
                interp.stack_push(value2)
                execute(interp, forthic)
                res = interp.stack_pop()
                result.append(res)
        else:
            result = {}
            for k, v in container1.items():
                interp.stack_push(v)
                interp.stack_push(container2.get(k))
                execute(interp, forthic)
                res = interp.stack_pop()
                result[k] = res

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- array )
    def word_KEYS(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            result = list(range(len(container)))
        else:
            result = list(container.keys())

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- array )
    def word_VALUES(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            result = container
        else:
            result = list(container.values())

        interp.stack_push(result)

    # ( array -- length )
    # ( record -- length )
    def word_LENGTH(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            container = []

        result = len(container)

        interp.stack_push(result)

    # ( array fstart fend -- indices )
    # Returns start and end indices of a range bounded where fstart and fend are true
    def word_RANGE(self, interp: IInterpreter):
        fend = interp.stack_pop()
        fstart = interp.stack_pop()
        array = interp.stack_pop()

        if not array:
            array = []

        start_found = False
        end_found = False

        start_index = None
        end_index = None
        for index, item in enumerate(array):
            if not start_found:
                interp.stack_push(item)
                execute(interp, fstart)
                start_found = interp.stack_pop()
                if start_found:
                    start_index = index

            if start_found and not end_found:
                interp.stack_push(item)
                execute(interp, fend)
                end_found = interp.stack_pop()
                if end_found:
                    end_index = index
                    break

        interp.stack_push([start_index, end_index])

    # ( array start end -- array )
    # ( record start end -- record )
    def word_SLICE(self, interp: IInterpreter):
        end = int(interp.stack_pop())
        start = int(interp.stack_pop())
        container = interp.stack_pop()
        length = len(container)

        if not container:
            container = []

        def normalize_index(index):
            res = index
            if index < 0:
                res = index + length
            return res

        start = normalize_index(start)
        end = normalize_index(end)

        step = 1
        if start > end:
            step = -1

        indexes: List[Any] = [start]
        if start < 0 or start >= length:
            indexes = []

        while start != end:
            start = start + step
            if start < 0 or start >= length:
                indexes.append(None)
            else:
                indexes.append(start)

        if isinstance(container, list):
            result: Any = []
            for i in indexes:
                if i is None:
                    result.append(None)
                else:
                    result.append(container[i])
        else:
            keys = list(container.keys())
            result = {}
            for i in indexes:
                if i is not None:
                    k = keys[i]
                    result[k] = container.get(k)

        interp.stack_push(result)

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_DIFFERENCE(self, interp: IInterpreter):
        rcontainer = interp.stack_pop()
        lcontainer = interp.stack_pop()

        if lcontainer is None:
            lcontainer = []

        if rcontainer is None:
            rcontainer = []

        def difference(left, right):
            res = []
            for item in left:
                if item not in right:
                    res.append(item)
            return res

        if isinstance(rcontainer, list):
            result = difference(lcontainer, rcontainer)
        else:
            lkeys = lcontainer.keys()
            rkeys = rcontainer.keys()
            diff = difference(lkeys, rkeys)
            result = {}
            for k in diff:
                result[k] = lcontainer[k]

        interp.stack_push(result)

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_INTERSECTION(self, interp: IInterpreter):
        rcontainer = interp.stack_pop()
        lcontainer = interp.stack_pop()

        if lcontainer is None:
            lcontainer = []

        if rcontainer is None:
            rcontainer = []

        if isinstance(rcontainer, list):
            lset = set(lcontainer)
            rset = set(rcontainer)
            result: Any = list(lset.intersection(rset))
        else:
            lkeys = set(lcontainer.keys())
            rkeys = set(rcontainer.keys())
            intersection = lkeys.intersection(rkeys)
            result = {}
            for k in intersection:
                result[k] = lcontainer[k]

        interp.stack_push(result)

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_UNION(self, interp: IInterpreter):
        rcontainer = interp.stack_pop()
        lcontainer = interp.stack_pop()

        if lcontainer is None:
            lcontainer = []

        if rcontainer is None:
            rcontainer = []

        if isinstance(rcontainer, list):
            lset = set(lcontainer)
            rset = set(rcontainer)
            result: Any = list(lset.union(rset))
        else:
            lkeys = set(lcontainer.keys())
            rkeys = set(rcontainer.keys())
            union = lkeys.union(rkeys)
            result = {}
            for k in union:
                item = lcontainer.get(k)
                if not item:
                    item = rcontainer.get(k)
                result[k] = item

        interp.stack_push(result)

    # ( larray forthic -- array )
    # ( lrecord forthic -- record )
    #
    # Flagged behavior:
    #    with_key: Pushes key and value onto stack for evaluation
    def word_SELECT(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        container = interp.stack_pop()

        flags = self.get_flags()

        if not container:
            interp.stack_push(container)
            return

        if isinstance(container, list):
            result: Any = []
            for i in range(len(container)):
                item = container[i]
                if flags.get('with_key'):
                    interp.stack_push(i)
                interp.stack_push(item)
                execute(interp, forthic)
                should_select = interp.stack_pop()
                if should_select:
                    result.append(item)
        else:
            result = {}
            for k, v in container.items():
                if flags.get('with_key'):
                    interp.stack_push(k)
                interp.stack_push(v)
                execute(interp, forthic)
                should_select = interp.stack_pop()
                if should_select:
                    result[k] = v

        interp.stack_push(result)

    # ( array n -- array )
    # ( record n -- record )
    #
    # Flagged behavior:
    #   * push_rest: This pushes the rest of the take container onto the stack
    def word_TAKE(self, interp: IInterpreter):
        n = interp.stack_pop()
        container = interp.stack_pop()

        flags = self.get_flags()

        if not container:
            container = []

        if isinstance(container, list):
            taken = container[:n]
            rest = container[n:]
        else:
            keys = sorted(list(container.keys()))
            taken_keys = keys[:n]
            rest_keys = keys[n:]
            taken = [container[k] for k in taken_keys]
            rest = [container[k] for k in rest_keys]

        interp.stack_push(taken)
        if flags.get('push_rest'):
            interp.stack_push(rest)

    # ( array n -- rest )
    # ( record n -- rest )
    def word_DROP(self, interp: IInterpreter):
        n = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            rest = container[n:]
        else:
            keys = sorted(list(container.keys()))
            rest_keys = keys[n:]
            rest = [container[k] for k in rest_keys]

        interp.stack_push(rest)

    # ( array -- array )
    # ( record -- record )
    def word_ROTATE(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            result = container
        elif isinstance(container, list):
            result = container[:]
            last = result.pop()
            result.insert(0, last)
        else:
            result = {}
            keys = list(container.keys())
            last = keys.pop()
            keys.insert(0, last)
            for k in keys:
                result[k] = container[k]

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- record )
    def word_SHUFFLE(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            result = container[:]
            random.shuffle(result)
        else:
            result = container

        interp.stack_push(result)

    # ( array -- array )
    # ( record -- record )
    def word_SORT(self, interp: IInterpreter):
        container = interp.stack_pop()

        flags = self.get_flags()
        comparator = flags.get('comparator')

        if not container:
            container = []

        # Sort using default item comparision
        def sort_without_comparator():
            def sort_record(record):
                sorted_items = sorted(record.items(), key=lambda x: x[1])
                res = {}
                for pair in sorted_items:
                    res[pair[0]] = pair[1]
                return res

            if isinstance(container, list):
                non_nones = [item for item in container if item is not None]
                nones = [item for item in container if item is None]
                result = sorted(non_nones) + nones
            else:
                result = sort_record(container)
            return result

        # Sort using a forthic string
        def sort_with_forthic(forthic):
            def forthic_func(val):
                interp.stack_push(val)
                execute(interp, forthic)
                res = interp.stack_pop()
                return res

            def sort_record(record):
                sorted_items = sorted(record.items(), key=lambda x: forthic_func(x[1]))
                res = {}
                for pair in sorted_items:
                    res[pair[0]] = pair[1]
                return res

            if isinstance(container, list):
                result = sorted(container[:], key=forthic_func)
            else:
                result = sort_record(container)
            return result

        # Sort using a key func
        def sort_with_key_func(key_func):
            if isinstance(container, list):
                result = sorted(container[:], key=key_func)
            else:
                result = container
            return result

        if isinstance(comparator, str):
            result = sort_with_forthic(comparator)
        elif callable(comparator):
            result = sort_with_key_func(comparator)
        else:
            result = sort_without_comparator()
        interp.stack_push(result)

    # ( field -- key_func )
    def word_FIELD_KEY_FUNC(self, interp: IInterpreter):
        field = interp.stack_pop()

        def result(record):
            return record[field]

        interp.stack_push(result)

    # ( array n -- item )
    # ( record n -- value )
    def word_NTH(self, interp: IInterpreter):
        n = interp.stack_pop()
        container = interp.stack_pop()

        if n is None or not container:
            interp.stack_push(None)
            return

        if n < 0 or n >= len(container):
            interp.stack_push(None)
            return

        if isinstance(container, list):
            result = container[n]
        else:
            keys = list(container.keys())
            key = keys[n]
            result = container[key]

        interp.stack_push(result)

    # ( array -- item )
    # ( record -- value )
    def word_LAST(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            interp.stack_push(None)
            return

        if isinstance(container, list):
            result = container[-1]
        else:
            keys = sorted(list(container.keys()))
            key = keys[-1]
            result = container[key]

        interp.stack_push(result)

    # ( array -- a1 a2 .. an )
    def word_UNPACK(self, interp: IInterpreter):
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            for item in container:
                interp.stack_push(item)
        else:
            keys = sorted(list(container.keys()))
            for k in keys:
                interp.stack_push(container[k])

    # ( nested_arrays -- array )
    # ( nested_records -- record )
    def word_FLATTEN(self, interp: IInterpreter):
        nested = interp.stack_pop()
        flags = self.get_flags()

        if not nested:
            nested = []

        depth = flags.get('depth')

        def fully_flatten_array(items, accum):
            for item in items:
                if isinstance(item, list):
                    fully_flatten_array(item, accum)
                else:
                    accum.append(item)
            return accum

        def flatten_array(items, depth, accum=[]):
            if depth is None:
                return fully_flatten_array(items, accum)

            for item in items:
                if depth > 0 and isinstance(item, list):
                    flatten_array(item, depth - 1, accum)
                else:
                    accum.append(item)
            return accum

        def add_to_record_result(item, keys, key, result):
            new_key = '\t'.join(keys + [key])
            result[new_key] = item

        def fully_flatten_record(record, res, keys):
            for k, item in record.items():
                if isinstance(item, dict):
                    fully_flatten_record(item, res, keys + [k])
                else:
                    add_to_record_result(item, keys, k, res)
            return res

        def flatten_record(record, depth, res={}, keys=[]):
            if depth is None:
                return fully_flatten_record(record, res, keys)

            for k, item in record.items():
                if depth > 0 and isinstance(item, dict):
                    flatten_record(item, depth - 1, res, keys + [k])
                else:
                    add_to_record_result(item, keys, k, res)
            return res

        if isinstance(nested, list):
            result = flatten_array(nested, depth)
        else:
            result = flatten_record(nested, depth)

        interp.stack_push(result)
        return

    # ( list item -- index )
    # ( record item -- key )
    def word_KEY_OF(self, interp: IInterpreter):
        item = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            if item not in container:
                result = None
            else:
                result = container.index(item)
        else:
            result = None
            for k, v in container.items():
                if v == item:
                    result = k
                    break

        interp.stack_push(result)

    # ( list initial forthic -- value )
    # ( record initial forthic -- value )
    def word_REDUCE(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        initial = interp.stack_pop()
        container = interp.stack_pop()

        if not container:
            container = []

        if isinstance(container, list):
            interp.stack_push(initial)
            for item in container:
                interp.stack_push(item)
                execute(interp, forthic)
            result = interp.stack_pop()
        else:
            interp.stack_push(initial)
            for _, v in container.items():
                interp.stack_push(v)
                execute(interp, forthic)
            result = interp.stack_pop()

        interp.stack_push(result)

    # ( records field breakpoints -- cumulative_distribution )
    def word_CUMULATIVE_DIST(self, interp: IInterpreter):
        breakpoints = interp.stack_pop()
        field = interp.stack_pop()
        records = interp.stack_pop()

        sorted_breakpoints = sorted(breakpoints)

        def get_breakpoint_index(breakpoints, value):
            out_of_range_index = len(breakpoints) + 1000  # Adding 1000 so it doesn't look like an "off by one" error :-)
            if value is None:
                return out_of_range_index

            res = None
            for i, breakpoint_value in enumerate(breakpoints):
                if value <= breakpoint_value:
                    res = i
                    break

            if res is None:
                res = out_of_range_index
            return res

        # Compute breakpoint indexes
        record_breakpoint_indexes = []
        for r in records:
            record_breakpoint_indexes.append(get_breakpoint_index(sorted_breakpoints, r.get(field)))

        # Compute breakpoint counts
        breakpoint_counts = [0] * len(sorted_breakpoints)
        for breakpoint_index in record_breakpoint_indexes:
            for i in range(len(breakpoint_counts)):
                if breakpoint_index <= i:
                    breakpoint_counts[i] += 1

        # Compute breakpoint pcts
        breakpoint_pcts = [0.0] * len(sorted_breakpoints)
        num_records = len(records)
        if num_records > 0:
            for i, count in enumerate(breakpoint_counts):
                breakpoint_pcts[i] = count / num_records * 100.0

        result = {
            "records": records,
            "field": field,
            "breakpoints": breakpoints,
            "record_breakpoint_indexes": record_breakpoint_indexes,
            "breakpoint_counts": breakpoint_counts,
            "breakpoint_pcts": breakpoint_pcts,
        }
        interp.stack_push(result)
        return

    # ( item -- )
    def word_POP(self, interp: IInterpreter):
        interp.stack_pop()

    # ( a -- a a )
    def word_DUP(self, interp: IInterpreter):
        a = interp.stack_pop()
        interp.stack_push(a)
        interp.stack_push(a)

    # ( a b -- b a )
    def word_SWAP(self, interp: IInterpreter):
        b = interp.stack[-1]
        a = interp.stack[-2]
        interp.stack[-1] = a
        interp.stack[-2] = b

    # ( str1 str2 -- str )
    # ( array_of_str -- str )
    def word_CONCAT(self, interp: IInterpreter):
        """Concatenates two strings"""
        str2 = interp.stack_pop()
        array = None
        if isinstance(str2, list):
            array = str2
        else:
            str1 = interp.stack_pop()
            array = [str1, str2]

        str_array = [str(item) for item in array]
        result = ''.join(str_array)
        interp.stack_push(result)

    # ( string sep -- items )
    def word_SPLIT(self, interp: IInterpreter):
        sep = interp.stack_pop()
        string = interp.stack_pop()

        if not string:
            string = ''

        result = string.split(sep)
        interp.stack_push(result)

    # ( array sep -- string )
    def word_JOIN(self, interp: IInterpreter):
        sep = interp.stack_pop()
        array = interp.stack_pop()

        if not array:
            array = []

        string_array = [str(item) for item in array]
        result = sep.join(string_array)
        interp.stack_push(result)

    # ( -- char )
    def word_slash_N(self, interp: IInterpreter):
        interp.stack_push('\n')

    # ( -- char )
    def word_slash_R(self, interp: IInterpreter):
        interp.stack_push('\r')

    # ( -- char )
    def word_slash_T(self, interp: IInterpreter):
        interp.stack_push('\t')

    # ( string -- string )
    def word_LOWERCASE(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        result = string.lower()
        interp.stack_push(result)

    # ( string -- string )
    def word_UPPERCASE(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        result = string.upper()
        interp.stack_push(result)

    # ( string -- string )
    def word_ASCII(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        result = ''
        for c in string:
            if ord(c) < 256:
                result += c
        interp.stack_push(result)

    # ( string -- string )
    def word_STRIP(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        interp.stack_push(string.strip())

    # ( string s r -- string )
    def word_REPLACE(self, interp: IInterpreter):
        replacement = interp.stack_pop()
        search_string = interp.stack_pop()
        string = interp.stack_pop()

        if not string:
            string = ''

        result = string.replace(search_string, replacement)
        interp.stack_push(result)

    # ( string regex replace -- string )
    def word_RE_REPLACE(self, interp: IInterpreter):
        replace = interp.stack_pop()
        regex = interp.stack_pop()
        string = interp.stack_pop()

        if not string:
            string = ''

        result = re.sub(regex, replace, string, re.MULTILINE | re.DOTALL)
        interp.stack_push(result)

    # ( string regex -- match )
    def word_RE_MATCH(self, interp: IInterpreter):
        regex = interp.stack_pop()
        string = interp.stack_pop()

        if not string:
            string = ''

        result = re.match(regex, string, re.MULTILINE | re.DOTALL)
        interp.stack_push(result)

    # ( string regex -- matches )
    def word_RE_MATCH_ALL(self, interp: IInterpreter):
        regex = interp.stack_pop()
        string = interp.stack_pop()

        if not string:
            string = ''

        result = re.findall(regex, string, re.MULTILINE | re.DOTALL)
        interp.stack_push(result)

    # ( match num -- string )
    def word_RE_MATCH_GROUP(self, interp: IInterpreter):
        num = interp.stack_pop()
        match = interp.stack_pop()
        result = None
        if match:
            result = match.group(num)
        interp.stack_push(result)

    # ( object -- string )
    def word_to_STR(self, interp: IInterpreter):
        obj = interp.stack_pop()
        result = str(obj)
        interp.stack_push(result)

    # ( str -- url_encoded_str )
    def word_URL_ENCODE(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        result = urllib.parse.quote_plus(string)
        interp.stack_push(result)

    # ( url_encoded -- str )
    def word_URL_DECODE(self, interp: IInterpreter):
        encoded = interp.stack_pop()

        if not encoded:
            encoded = ''

        result = urllib.parse.unquote(encoded)
        interp.stack_push(result)

    # ( root child_items_forthic -- node_items )
    # This starts from a root and applies `child_items_forthic` to get a set of child items.
    # This repeats, depth first, until all elements have been traversed.
    # If an item has already been traversed, it is not further traversed.
    #
    # Each node_item is a record with the following fields:
    #    * depth: Depth in tree (0 for root)
    #    * value: The child item
    def word_TRAVERSE_DEPTH_FIRST(self, interp: IInterpreter):
        child_items_forthic = interp.stack_pop()
        root = interp.stack_pop()

        result = []

        def traverse(item, depth):
            if item in result:
                return
            node_item = {
                'depth': depth,
                'value': item,
            }
            result.append(node_item)
            interp.stack_push(item)
            execute(interp, child_items_forthic)
            children = interp.stack_pop()
            for c in children:
                traverse(c, depth + 1)

        traverse(root, 0)
        interp.stack_push(result)

    # ( tree subroots -- subtrees )
    # `tree` is an array of `node_items` (see word_TRAVERSE_DEPTH_FIRST)
    # `subroots` is an array of `node_items` in the tree
    # `subtrees` is an array of trees rooted at the subroots
    #
    # If a subroot is not in the tree, then the value of its subtree is []
    def word_SUBTREES(self, interp: IInterpreter):
        subroots = interp.stack_pop()
        tree = interp.stack_pop()

        def get_subtree(subroot):
            try:
                index = tree.index(subroot)
            except ValueError:
                index = None

            # If subroot is not in tree
            if index is None:
                return []

            # Return node items from subroot to next node item at the subroot depth or higher
            res = [subroot]
            subroot_depth = subroot['depth']
            for node_item in tree[index + 1:]:
                if node_item['depth'] <= subroot_depth:
                    break
                res.append(node_item)
            return res

        result = [get_subtree(s) for s in subroots]
        interp.stack_push(result)

    # ( -- None )
    def word_NULL(self, interp: IInterpreter):
        interp.stack_push(None)

    # ( -- quote_char )
    def word_QUOTE_CHAR(self, interp: IInterpreter):
        result = DLE
        interp.stack_push(result)

    # ( string -- quoted_string )
    def word_QUOTED(self, interp: IInterpreter):
        string = interp.stack_pop()

        if not string:
            string = ''

        chars = []
        for c in string:
            if c == DLE:
                c = ' '
            chars.append(c)
        clean_string = ''.join(chars)
        result = f'{DLE}{clean_string}{DLE}'
        interp.stack_push(result)

    # ( value default_value -- val )
    def word_DEFAULT(self, interp: IInterpreter):
        default_value = interp.stack_pop()
        value = interp.stack_pop()
        if value is None or value == '':
            value = default_value
        interp.stack_push(value)

    # ( value default_forthic -- val )
    def word_star_DEFAULT(self, interp: IInterpreter):
        default_forthic = interp.stack_pop()
        value = interp.stack_pop()
        if value is None or value == '':
            execute(interp, default_forthic)
            value = interp.stack_pop()
        interp.stack_push(value)

    # ( item forthic num-times -- ? )
    def word_l_REPEAT(self, interp: IInterpreter):
        num_times = interp.stack_pop()
        forthic = interp.stack_pop()
        for _ in range(num_times):
            # Store item so we can push it back later
            item = interp.stack_pop()
            interp.stack_push(item)

            execute(interp, forthic)
            res = interp.stack_pop()

            # Push original item and result
            interp.stack_push(item)
            interp.stack_push(res)

    # ( a -- a )
    def word_IDENTITY(self, interp: IInterpreter):
        pass

    # ( num digits -- str )
    def word_to_FIXED(self, interp: IInterpreter):
        digits = interp.stack_pop()
        num = interp.stack_pop()

        if num is None:
            interp.stack_push(None)
            return

        result = f'%.{digits}f' % num
        interp.stack_push(result)

    # ( item -- json )
    def word_to_JSON(self, interp: IInterpreter):
        item = interp.stack_pop()
        result = json.dumps(item)
        interp.stack_push(result)

    # ( json -- item )
    def word_JSON_to(self, interp: IInterpreter):
        string = interp.stack_pop()
        result = json.loads(string)
        interp.stack_push(result)

    # ( items -- tsv )
    def word_to_TSV(self, interp: IInterpreter):
        items = interp.stack_pop()

        if not items:
            items = []

        buf = io.StringIO()
        writer = csv.writer(buf, delimiter='\t')
        writer.writerows(items)
        result = buf.getvalue()
        interp.stack_push(result)

    # ( tsv -- items )
    def word_TSV_to(self, interp: IInterpreter):
        tsv = interp.stack_pop()

        buf = io.StringIO(tsv)
        reader = csv.reader(buf, delimiter='\t')
        result = [row for row in reader]
        interp.stack_push(result)

    # ( records header -- tsv )
    def word_RECS_to_TSV(self, interp: IInterpreter):
        header = interp.stack_pop()
        records = interp.stack_pop()

        if not records:
            records = []

        vals_array = []
        for rec in records:
            vals_array.append([rec[h] for h in header])

        buf = io.StringIO()
        writer = csv.writer(buf, delimiter='\t')

        writer.writerow(header)
        writer.writerows(vals_array)
        result = buf.getvalue()
        interp.stack_push(result)

    # ( tsv -- records )
    def word_TSV_to_RECS(self, interp: IInterpreter):
        tsv = interp.stack_pop()

        buf = io.StringIO(tsv)
        reader = csv.reader(buf, delimiter='\t')
        rows = [row for row in reader]
        header = rows[0]
        result = []
        for row in rows[1:]:
            rec = {}
            for i in range(len(header)):
                rec[header[i]] = row[i]
            result.append(rec)
        interp.stack_push(result)

    # ( -- )
    def word_dot_s(self, interp: IInterpreter):
        top_of_stack = None
        if len(interp.stack) > 0:
            top_of_stack = interp.stack[-1]

        if interp.dev_mode:
            print(top_of_stack)
            pdb.set_trace()
        else:
            # Raising an exception to show stack to user
            items = ['Forthic Stack:']
            indices = reversed(range(len(interp.stack)))
            for i in indices:
                items.append(
                    f'[{i}]: {str(interp.stack[i])}'
                )
            stack_string = '\n'.join(items)
            raise StackDump(stack_string)

    # ( time -- time )
    def word_AM(self, interp: IInterpreter):
        a_time = interp.stack_pop()
        if not isinstance(a_time, datetime.time):
            raise InvalidTimeError(f'AM expecting a time, not {a_time}')

        result = a_time
        if a_time.hour >= 12:
            result = datetime.time(a_time.hour - 12, a_time.minute)

        interp.stack_push(result)

    # ( time -- time )
    def word_PM(self, interp: IInterpreter):
        a_time = interp.stack_pop()
        if not isinstance(a_time, datetime.time):
            raise InvalidTimeError(f'PM expecting a time, not {a_time}')

        result = a_time
        if a_time.hour < 12:
            result = datetime.time(a_time.hour + 12, a_time.minute)

        interp.stack_push(result)

    # ( -- time )
    def word_NOW(self, interp: IInterpreter):
        result = datetime.datetime.now(tz=self.timezone)
        interp.stack_push(result)

    # ( str -- time )
    # ( time -- time )
    def word_to_TIME(self, interp: IInterpreter):
        item = interp.stack_pop()
        result: Union[datetime.time, datetime.datetime, None] = None
        if isinstance(item, datetime.datetime):
            result = item
        else:
            t = parser.parse(item)
            tz = self.timezone
            if t.tzinfo:
                tz = t.tzinfo
            result = datetime.time(t.hour, t.minute, tzinfo=tz)

        interp.stack_push(result)

    # ( time tzstr -- time )
    def word_l_TZ_bang(self, interp: IInterpreter):
        tzstr = interp.stack_pop()
        t = interp.stack_pop()
        tz = pytz.timezone(tzstr)
        if isinstance(t, datetime.datetime):
            result: Union[datetime.datetime, datetime.time] = t.replace(tzinfo=tz)
        else:
            result = datetime.time(t.hour, t.minute, tzinfo=tz)

        interp.stack_push(result)

    # ( time -- string )
    def word_TIME_to_STR(self, interp: IInterpreter):
        t = interp.stack_pop()
        dt = t.tzinfo.localize(datetime.datetime(2000, 1, 1, t.hour, t.minute))
        interp_dt = dt.astimezone(self.timezone)
        result = interp_dt.strftime('%H:%M')
        interp.stack_push(result)

    # ( item -- date )
    def word_to_DATE(self, interp: IInterpreter):
        item = interp.stack_pop()

        result = None
        if not item:
            result = None
        elif isinstance(item, datetime.datetime):
            result = item.date()
        elif isinstance(item, datetime.date):
            result = item
        else:
            result = parser.parse(item).date()
        interp.stack_push(result)

    # ( -- date )
    def word_TODAY(self, interp: IInterpreter):
        result = datetime.date.today()
        interp.stack_push(result)

    # ( -- date )
    def word_MONDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(0))

    # ( -- date )
    def word_TUESDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(1))

    # ( -- date )
    def word_WEDNESDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(2))

    # ( -- date )
    def word_THURSDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(3))

    # ( -- date )
    def word_FRIDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(4))

    # ( -- date )
    def word_SATURDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(5))

    # ( -- date )
    def word_SUNDAY(self, interp: IInterpreter):
        interp.stack_push(self.day_this_week(6))

    # ( date -- date )
    def word_NEXT(self, interp: IInterpreter):
        # If date is in the past, return date + 7 days
        a_date = interp.stack_pop()
        today = datetime.date.today()
        result = a_date
        if a_date < today:
            result = a_date + datetime.timedelta(7)
        interp.stack_push(result)

    def day_this_week(self, day_of_week):
        # NOTE: Monday is start of week
        today = datetime.date.today()
        delta_days = (day_of_week - today.weekday()) % 7
        if day_of_week < today.weekday():
            delta_days -= 7
        result = today + datetime.timedelta(delta_days)
        return result

    # ( date num_days -- date )
    def word_ADD_DAYS(self, interp: IInterpreter):
        num_days = interp.stack_pop()
        date = interp.stack_pop()
        result = date + datetime.timedelta(num_days)
        interp.stack_push(result)

    # ( ldate rdate -- num_days )
    def word_SUBTRACT_DATES(self, interp: IInterpreter):
        rdate = interp.stack_pop()
        ldate = interp.stack_pop()
        delta = ldate - rdate
        result = round(delta.total_seconds() / 60 / 60 / 24)
        interp.stack_push(result)

    # ( ldate rdate -- num_secs )
    def word_SUBTRACT_TIMES(self, interp: IInterpreter):
        rdate = interp.stack_pop()
        ldate = interp.stack_pop()
        delta = ldate - rdate
        result = delta.total_seconds()
        interp.stack_push(result)

    # ( date -- str )
    def word_DATE_to_STR(self, interp: IInterpreter):
        date = interp.stack_pop()
        if not date:
            interp.stack_push('')
            return

        result = f'{date.year}-{date.month:02d}-{date.day:02d}'
        interp.stack_push(result)

    # ( date time -- datetime )
    def word_DATE_TIME_to_DATETIME(self, interp: IInterpreter):
        a_time = interp.stack_pop()
        a_date = interp.stack_pop()
        result = datetime.datetime(
            a_date.year, a_date.month, a_date.day, a_time.hour, a_time.minute
        )
        tz = self.timezone
        if a_time.tzinfo:
            tz = a_time.tzinfo
        result = tz.localize(result)
        interp.stack_push(result)

    # ( datetime -- timestamp )
    def word_DATETIME_to_TIMESTAMP(self, interp: IInterpreter):
        dt = interp.stack_pop()
        result = int(datetime.datetime.timestamp(dt))
        interp.stack_push(result)

    # ( timestamp -- datetime )
    def word_TIMESTAMP_to_DATETIME(self, interp: IInterpreter):
        ts = interp.stack_pop()
        result = datetime.datetime.fromtimestamp(ts)
        interp.stack_push(result)

    # ( str -- datetime )
    def word_STR_to_DATETIME(self, interp: IInterpreter):
        string = interp.stack_pop()

        if string is None:
            interp.stack_push(None)
            return

        result = parser.parse(string)
        interp.stack_push(result)

    # ( a b -- a+b )
    # ( [a1 a2...] -- sum )
    def word_plus(self, interp: IInterpreter):
        """Adds two numbers or an array of numbers"""
        b = interp.stack_pop()
        result = 0
        if isinstance(b, list):
            for num in b:
                if b is not None:
                    result += num
        else:
            a = interp.stack_pop()
            if a is None:
                a = 0
            if b is None:
                b = 0
            result = a + b
        interp.stack_push(result)

    # ( a b -- a-b )
    def word_minus(self, interp: IInterpreter):
        b = interp.stack_pop()
        a = interp.stack_pop()

        if a is None or b is None:
            interp.stack_push(None)
            return

        # Return seconds for datetime
        if isinstance(a, datetime.datetime):
            delta = a - b
            result = delta.total_seconds()
        # Return days for date
        elif isinstance(a, datetime.date):
            delta = a - b
            result = delta.total_seconds() / 60 / 60 / 24
        else:
            result = a - b

        interp.stack_push(result)

    # ( a b -- a*b )
    # ( [a1 a2...] -- product )
    def word_times(self, interp: IInterpreter):
        b = interp.stack_pop()
        result = 1
        numbers = []
        if isinstance(b, list):
            numbers = b
        else:
            a = interp.stack_pop()
            numbers = [a, b]

        for num in numbers:
            if num is None:
                interp.stack_push(None)
                return
            result *= num
        interp.stack_push(result)

    # ( a b -- a/b )
    def word_divide_by(self, interp: IInterpreter):
        b = interp.stack_pop()
        a = interp.stack_pop()

        if a is None or b is None:
            interp.stack_push(None)
            return

        if b == 0:
            result = None
        else:
            result = a / b
        interp.stack_push(result)

    # ( m n -- m%n )
    def word_MOD(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()

        if m is None or n is None:
            interp.stack_push(None)
            return

        interp.stack_push(m % n)

    # ( num -- int )
    def word_ROUND(self, interp: IInterpreter):
        num = interp.stack_pop()

        if num is None:
            interp.stack_push(None)
            return

        interp.stack_push(round(num))

    # ( items -- item )
    def word_MAX(self, interp: IInterpreter):
        items = interp.stack_pop()
        if not items:
            interp.stack_push(None)
            return
        interp.stack_push(max(items))

    # ( items -- item )
    def word_MIN(self, interp: IInterpreter):
        items = interp.stack_pop()
        if not items:
            interp.stack_push(None)
            return
        interp.stack_push(min(items))

    # ( m n -- bool )
    def word_equal_equal(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()
        interp.stack_push(m == n)

    # ( m n -- bool )
    def word_not_equal(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()
        interp.stack_push(m != n)

    # ( m n -- bool )
    def word_greater_than(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()

        if m is None or n is None:
            interp.stack_push(None)
            return

        interp.stack_push(m > n)

    # ( m n -- bool )
    def word_greater_than_or_equal(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()

        if m is None or n is None:
            interp.stack_push(None)
            return

        interp.stack_push(m >= n)

    # ( m n -- bool )
    def word_less_than(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()

        if m is None or n is None:
            interp.stack_push(None)
            return

        interp.stack_push(m < n)

    # ( m n -- bool )
    def word_less_than_or_equal(self, interp: IInterpreter):
        n = interp.stack_pop()
        m = interp.stack_pop()

        if m is None or n is None:
            interp.stack_push(None)
            return

        interp.stack_push(m <= n)

    # ( a b -- bool )
    # ( [a1 a2...] -- bool )
    def word_OR(self, interp: IInterpreter):
        b = interp.stack_pop()
        if isinstance(b, list):
            result = any(b)
        else:
            a = interp.stack_pop()
            result = a or b
        interp.stack_push(result)

    # ( a b -- bool )
    # ( [a1 a2...] -- bool )
    def word_AND(self, interp: IInterpreter):
        b = interp.stack_pop()
        if isinstance(b, list):
            result = all(b)
        else:
            a = interp.stack_pop()
            result = a and b
        interp.stack_push(result)

    # ( a -- bool )
    def word_NOT(self, interp: IInterpreter):
        a = interp.stack_pop()
        interp.stack_push(not a)

    # ( item items -- bool )
    def word_IN(self, interp: IInterpreter):
        items = interp.stack_pop()
        item = interp.stack_pop()
        if not items:
            items = []
        result = item in items
        interp.stack_push(result)

    # ( vals required_vals -- bool )
    def word_ANY(self, interp: IInterpreter):
        required_vals = interp.stack_pop()
        vals = interp.stack_pop()

        if not vals:
            vals = []
        if not required_vals:
            required_vals = []

        result = False

        for rv in required_vals:
            if rv in vals:
                result = True
                break

        # If nothing is required, then all values are true
        if len(required_vals) == 0:
            result = True
        interp.stack_push(result)

    # ( vals required_vals -- bool )
    def word_ALL(self, interp: IInterpreter):
        required_vals = interp.stack_pop()
        vals = interp.stack_pop()

        if not vals:
            vals = []
        if not required_vals:
            required_vals = []

        required_set = set(required_vals)
        vals_set = set(vals)
        intersection_set = required_set.intersection(vals_set)

        result = intersection_set == required_set
        interp.stack_push(result)

    # ( item -- bool )
    def word_to_BOOL(self, interp: IInterpreter):
        item = interp.stack_pop()
        result = False
        if item:
            result = True
        interp.stack_push(result)

    # ( a -- a_int )
    def word_to_INT(self, interp: IInterpreter):
        a = interp.stack_pop()

        if a is None:
            interp.stack_push(0)
            return

        if isinstance(a, list) or isinstance(a, dict):
            interp.stack_push(len(a))
            return

        result: Union[int, None]
        try:
            result = int(float(a))
        except ValueError:
            result = None

        interp.stack_push(result)

    # ( a -- a_int )
    def word_to_FLOAT(self, interp: IInterpreter):
        a = interp.stack_pop()

        if a is None:
            interp.stack_push(0.0)
            return

        result: Union[float, None]
        try:
            result = float(a)
        except ValueError:
            result = None

        interp.stack_push(result)

    # ( low high -- int )
    def word_UNIFORM_RANDOM(self, interp: IInterpreter):
        high = interp.stack_pop()
        low = interp.stack_pop()
        result = random.uniform(low, high)
        interp.stack_push(result)

    # ( val start_ranges -- index )
    def word_RANGE_INDEX(self, interp: IInterpreter):
        """Returns index of range that value falls into"""
        start_ranges = interp.stack_pop()
        val = interp.stack_pop()

        # Cap off the value ranges with infinity
        start_ranges.append(math.inf)

        if val is None or not start_ranges:
            interp.stack_push(None)
            return

        if val < start_ranges[0]:
            interp.stack_push(None)
            return

        result = None
        for i in range(len(start_ranges) - 1):
            if val >= start_ranges[i] and val < start_ranges[i + 1]:
                result = i
                break

        interp.stack_push(result)

    # ( -- )
    def word_bang_PUSH_ERROR(self, interp: IInterpreter):
        self.flags["push_error"] = True

    # ( -- )
    def word_bang_WITH_KEY(self, interp: IInterpreter):
        self.flags["with_key"] = True

    # (comparator -- )
    #
    # `comparator` may be a Forthic string or a Python key function
    def word_bang_COMPARATOR(self, interp: IInterpreter):
        comparator = interp.stack_pop()
        self.flags["comparator"] = comparator

    # ( -- )
    def word_bang_PUSH_REST(self, interp: IInterpreter):
        self.flags["push_rest"] = True

    # (depth -- )
    #
    # `depth` of 0 is the same as a regular MAP
    def word_bang_DEPTH(self, interp: IInterpreter):
        depth = interp.stack_pop()
        self.flags["depth"] = depth

    # ( -- )
    def word_PROFILE_START(self, interp: IInterpreter):
        interp.start_profiling()

    # ( label -- )
    def word_PROFILE_TIMESTAMP(self, interp: IInterpreter):
        label = interp.stack_pop()
        interp.add_timestamp(label)

    # ( -- ProfileAnalyzer )
    def word_PROFILE_END(self, interp: IInterpreter):
        interp.stop_profiling()
        result = None
        if interp.cur_word_profile:
            interp.cur_word_profile = interp.cur_word_profile.get_parent()
            result = ProfileAnalyzer(interp.cur_word_profile)
        interp.stack_push(result)

    # ( -- data )
    def word_PROFILE_DATA(self, interp: IInterpreter):
        histogram = interp.word_histogram()
        timestamps = interp.profile_timestamps()

        result = defaultdict(list)
        for val in histogram:
            rec = {'word': val['word'], 'count': val['count']}
            result['word_counts'].append(rec)

        prev_time = 0.0
        for t in timestamps:
            rec = {
                'label': t['label'],
                'time': t['time'],
                'delta': t['time'] - prev_time,
            }
            prev_time = t['time']
            result['timestamps'].append(rec)

        interp.stack_push(result)

    # ( -- profile_report )
    def word_PROFILE_REPORT(self, interp: IInterpreter):
        histogram = interp.word_histogram()
        result = '\nWord counts:\n'
        result += '\n'.join(
            [
                '%30s: %d' % (val['word'], val['count'])
                for val in histogram
                if val['count'] > 1
            ]
        )

        result += '\n\nTimestamps (sec):\n'
        timestamps = interp.profile_timestamps()

        def timestamp_strings(timestamps):
            res = []
            prev_time = 0.0
            for t in timestamps:
                string = '%30s: %.3f (%.3f)' % (
                    t['label'],
                    t['time'],
                    t['time'] - prev_time,
                )
                prev_time = t['time']
                res.append(string)
            return res

        result += '\n'.join(timestamp_strings(timestamps))
        result += '\n'

        interp.stack_push(result)

    # ( -- username )
    def word_CURRENT_USER(self, interp: IInterpreter):
        result = os.getlogin()
        interp.stack_push(result)

    # ( markdown -- html)
    def word_MARKDOWN_to_HTML(self, interp: IInterpreter):
        markdown_content = interp.stack_pop()
        result = markdown.markdown(markdown_content)
        interp.stack_push(result)

    def get_flags(self):
        flags = self.flags.copy()
        self.flags = {}
        return flags


def drill_for_value(record, fields):
    """Descends into record using an array of fields, returning final value or None"""
    result = record
    try:
        for f in fields:
            if result is None:
                return result
            if isinstance(result, list):
                result = result[f]
            else:
                result = result.get(f)
    except Exception:
        result = None
    return result


def run_returning_error(interp, forthic):
    result = None
    try:
        execute(interp, forthic)
    except Exception as e:
        result = e
    return result


def foreach(interp, flags):
    forthic = interp.stack_pop()
    container = interp.stack_pop()

    errors = []
    if not container:
        container = []

    if isinstance(container, list):
        items = container
        for i in range(len(items)):
            item = items[i]
            if flags.get('with_key'):
                interp.stack_push(i)
            interp.stack_push(item)
            if flags.get('push_error'):
                errors.append(run_returning_error(interp, forthic))
            else:
                execute(interp, forthic)

    else:
        for k, item in container.items():
            if flags.get('with_key'):
                interp.stack_push(k)
            interp.stack_push(item)
            if flags.get('push_error'):
                errors.append(run_returning_error(interp, forthic))
            else:
                execute(interp, forthic)

    if flags.get('push_error'):
        interp.stack_push(errors)
    return


def execute(interp: IInterpreter, object: str):
    if isinstance(object, str):
        interp.run(object)
    else:
        object.execute(interp)
    return


def execute_returning_error(interp: IInterpreter, object: str) -> Optional[Exception]:
    result = None
    try:
        execute(interp, object)
    except Exception as e:
        result = e
    return result
