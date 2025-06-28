# frozen_string_literal: true

require "time"
require "json"

require_relative "forthic_module"
require_relative "forthic_error"
require_relative "words/word"
require_relative "words/push_value_word"
require_relative "words/map_word"
require_relative "tokenizer"
require_relative "code_location"

module Forthic
  class GlobalModule < ForthicModule
    attr_accessor :module_id, :literal_handlers

    def initialize(interp)
      super("<GLOBAL>", interp)
      @module_id = "<GLOBAL>-#{rand(1_000_000)}"

      # Set default flags
      interp.set_flags(@module_id, {
        with_key: nil,
        push_error: nil,
        comparator: nil,
        push_rest: nil,
        depth: nil,
        interps: 1
      })

      @literal_handlers = [
        method(:to_bool),
        method(:to_float),
        method(:to_literal_date),
        method(:to_time),
        method(:to_int)
      ]

      # --------------------------------------------------
      # Base words
      add_module_word("VARIABLES", method(:word_VARIABLES))
      add_module_word("!", method(:word_bang))
      add_module_word("@", method(:word_at))
      add_module_word("!@", method(:word_bang_at))
      add_module_word("INTERPRET", method(:word_INTERPRET))
      add_module_word("EXPORT", method(:word_EXPORT))
      add_module_word("USE-MODULES", method(:word_USE_MODULES))
      add_module_word("REC", method(:word_REC))
      add_module_word("REC@", method(:word_REC_at))
      add_module_word("<REC!", method(:word_l_REC_bang))

      # --------------------------------------------------
      # Array/Record words
      add_module_word("APPEND", method(:word_APPEND))
      add_module_word("REVERSE", method(:word_REVERSE))
      add_module_word("UNIQUE", method(:word_UNIQUE))
      add_module_word("<DEL", method(:word_L_DEL))
      add_module_word("RELABEL", method(:word_RELABEL))
      add_module_word("BY-FIELD", method(:word_BY_FIELD))
      add_module_word("GROUP-BY-FIELD", method(:word_GROUP_BY_FIELD))
      add_module_word("GROUP-BY", method(:word_GROUP_BY))
      add_module_word("GROUPS-OF", method(:word_GROUPS_OF))
      add_module_word("INDEX", method(:word_INDEX))
      add_module_word("MAP", method(:word_MAP))
      add_module_word("FOREACH", method(:word_FOREACH))
      add_module_word("INVERT-KEYS", method(:word_INVERT_KEYS))
      add_module_word("ZIP", method(:word_ZIP))
      add_module_word("ZIP-WITH", method(:word_ZIP_WITH))
      add_module_word("KEYS", method(:word_KEYS))
      add_module_word("VALUES", method(:word_VALUES))
      add_module_word("LENGTH", method(:word_LENGTH))
      add_module_word("RANGE", method(:word_RANGE))
      add_module_word("SLICE", method(:word_SLICE))
      add_module_word("DIFFERENCE", method(:word_DIFFERENCE))
      add_module_word("INTERSECTION", method(:word_INTERSECTION))
      add_module_word("UNION", method(:word_UNION))
      add_module_word("SELECT", method(:word_SELECT))
      add_module_word("TAKE", method(:word_TAKE))
      add_module_word("DROP", method(:word_DROP))
      add_module_word("ROTATE", method(:word_ROTATE))
      add_module_word("ARRAY?", method(:word_ARRAY_q))
      add_module_word("SHUFFLE", method(:word_SHUFFLE))
      add_module_word("SORT", method(:word_SORT))
      add_module_word("FIELD-KEY-FUNC", method(:word_FIELD_KEY_FUNC))
      add_module_word("NTH", method(:word_NTH))
      add_module_word("LAST", method(:word_LAST))
      add_module_word("UNPACK", method(:word_UNPACK))
      add_module_word("FLATTEN", method(:word_FLATTEN))
      add_module_word("KEY-OF", method(:word_KEY_OF))
      add_module_word("REDUCE", method(:word_REDUCE))

      # --------------------------------------------------
      # Stack words
      add_module_word("POP", method(:word_POP))
      add_module_word("DUP", method(:word_DUP))
      add_module_word("SWAP", method(:word_SWAP))

      # --------------------------------------------------
      # String words
      add_module_word(">STR", method(:word_to_STR))
      add_module_word("CONCAT", method(:word_CONCAT))
      add_module_word("SPLIT", method(:word_SPLIT))
      add_module_word("JOIN", method(:word_JOIN))
      add_module_word("/N", method(:word_slash_N))
      add_module_word("/R", method(:word_slash_R))
      add_module_word("/T", method(:word_slash_T))
      add_module_word("LOWERCASE", method(:word_LOWERCASE))
      add_module_word("UPPERCASE", method(:word_UPPERCASE))
      add_module_word("ASCII", method(:word_ASCII))
      add_module_word("STRIP", method(:word_STRIP))
      add_module_word("REPLACE", method(:word_REPLACE))
      add_module_word("RE-MATCH", method(:word_RE_MATCH))
      add_module_word("RE-MATCH-GROUP", method(:word_RE_MATCH_GROUP))
      add_module_word("RE-MATCH-ALL", method(:word_RE_MATCH_ALL))

      # --------------------------------------------------
      # Misc words
      add_module_word("NULL", method(:word_NULL))
      add_module_word("DEFAULT", method(:word_DEFAULT))
      add_module_word("*DEFAULT", method(:word_star_DEFAULT))
      add_module_word("<REPEAT", method(:word_l_REPEAT))
      add_module_word(">FIXED", method(:word_to_FIXED))
      add_module_word(">JSON", method(:word_to_JSON))
      add_module_word("JSON>", method(:word_JSON_to))
      add_module_word(".s", method(:word_dot_s))
      add_module_word(".S", method(:word_dot_S))

      # --------------------------------------------------
      # Date/time words
      add_module_word("AM", method(:word_AM))
      add_module_word("PM", method(:word_PM))
      add_module_word("NOW", method(:word_NOW))
      add_module_word(">TIME", method(:word_to_TIME))
      add_module_word(">DATE", method(:word_to_DATE))
      add_module_word("TODAY", method(:word_TODAY))
      add_module_word("MONDAY", method(:word_MONDAY))
      add_module_word("TUESDAY", method(:word_TUESDAY))
      add_module_word("WEDNESDAY", method(:word_WEDNESDAY))
      add_module_word("THURSDAY", method(:word_THURSDAY))
      add_module_word("FRIDAY", method(:word_FRIDAY))
      add_module_word("SATURDAY", method(:word_SATURDAY))
      add_module_word("SUNDAY", method(:word_SUNDAY))
      add_module_word("ADD-DAYS", method(:word_ADD_DAYS))
      add_module_word("SUBTRACT-DATES", method(:word_SUBTRACT_DATES))
      add_module_word("DATE>STR", method(:word_DATE_to_STR))
      add_module_word("TIME>STR", method(:word_TIME_to_STR))
      add_module_word("DATE-TIME>DATETIME", method(:word_DATE_TIME_to_DATETIME))
      add_module_word("DATETIME>TIMESTAMP", method(:word_DATETIME_to_TIMESTAMP))
      add_module_word("TIMESTAMP>DATETIME", method(:word_TIMESTAMP_to_DATETIME))
      add_module_word("STR>DATETIME", method(:word_STR_to_DATETIME))
      add_module_word("STR>TIMESTAMP", method(:word_STR_to_TIMESTAMP))

      # --------------------------------------------------
      # Math words
      add_module_word("+", method(:word_plus))
      add_module_word("-", method(:word_minus))
      add_module_word("*", method(:word_times))
      add_module_word("/", method(:word_divide_by))
      add_module_word("ADD", method(:word_plus))
      add_module_word("SUBTRACT", method(:word_minus))
      add_module_word("MULTIPLY", method(:word_times))
      add_module_word("DIVIDE", method(:word_divide_by))
      add_module_word("MOD", method(:word_MOD))
      add_module_word("MEAN", method(:word_MEAN))
      add_module_word("MAX", method(:word_MAX))
      add_module_word("MIN", method(:word_MIN))
      add_module_word("ROUND", method(:word_ROUND))
      add_module_word("==", method(:word_equal_equal))
      add_module_word("!=", method(:word_not_equal))
      add_module_word(">", method(:word_greater_than))
      add_module_word(">=", method(:word_greater_than_or_equal))
      add_module_word("<", method(:word_less_than))
      add_module_word("<=", method(:word_less_than_or_equal))
      add_module_word("OR", method(:word_OR))
      add_module_word("AND", method(:word_AND))
      add_module_word("NOT", method(:word_NOT))
      add_module_word("IN", method(:word_IN))
      add_module_word("ANY", method(:word_ANY))
      add_module_word("ALL", method(:word_ALL))
      add_module_word(">BOOL", method(:word_to_BOOL))
      add_module_word(">INT", method(:word_to_INT))
      add_module_word(">FLOAT", method(:word_to_FLOAT))
      add_module_word("RANGE-INDEX", method(:word_RANGE_INDEX))
      add_module_word("INFINITY", method(:word_INFINITY))

      # --------------------------------------------------
      # Flag words
      add_module_word("!PUSH-ERROR", method(:word_bang_PUSH_ERROR))
      add_module_word("!WITH-KEY", method(:word_bang_WITH_KEY))
      add_module_word("!COMPARATOR", method(:word_bang_COMPARATOR))
      add_module_word("!PUSH-REST", method(:word_bang_PUSH_REST))
      add_module_word("!DEPTH", method(:word_bang_DEPTH))
      add_module_word("!INTERPS", method(:word_bang_INTERPS))

      # --------------------------------------------------
      # Profiling words
      add_module_word("PROFILE-START", method(:word_PROFILE_START))
      add_module_word("PROFILE-TIMESTAMP", method(:word_PROFILE_TIMESTAMP))
      add_module_word("PROFILE-END", method(:word_PROFILE_END))
      add_module_word("PROFILE-DATA", method(:word_PROFILE_DATA))

      # --------------------------------------------------
      # Ruby-specific words
      add_module_word(">SYM", method(:word_to_SYM))
    end

    def find_word(name)
      result = super
      result ||= find_literal_word(name)
      result
    end

    def find_literal_word(string)
      value = nil
      @literal_handlers.each do |handler|
        value = handler.call(string)
        return PushValueWord.new(string, value) unless value.nil?
      end
      nil
    end

    # Literal handlers
    def to_bool(str_val)
      return true if str_val == "TRUE"
      return false if str_val == "FALSE"
      nil
    end

    def to_int(str_val)
      Integer(str_val)
    rescue
      nil
    end

    def to_float(str_val)
      return nil unless str_val.include?(".")
      begin
        Float(str_val)
      rescue
        nil
      end
    end

    def to_literal_date(str_val)
      match = str_val.match(/(\d{4})-(\d{2})-(\d{2})/)
      return nil unless match

      year = match[1].to_i
      month = match[2].to_i
      day = match[3].to_i
      Date.new(year, month, day)
    end

    def to_time(str_val)
      match = str_val.match(/(\d{1,2}):(\d{2})/)
      return nil unless match

      hours = match[1].to_i
      minutes = match[2].to_i
      return nil if hours > 23 || minutes >= 60

      result = Time.now
      Time.new(result.year, result.month, result.day, hours, minutes, 0)
    end

    # Convenience function to create element word handlers
    def make_element_word(element_name)
      proc do |interp|
        interp.stack_push(element_name)
        interp.run("Element")
      end
    end

    def add_element_word(element_name)
      add_module_word(element_name, make_element_word(element_name))
    end

    # Words

    # ( varnames -- )
    def word_VARIABLES(interp)
      varnames = interp.stack_pop
      module_ = interp.cur_module
      varnames.each do |v|
        if /__.*/.match?(v)
          raise ForthicError.new(
            "global_module-696",
            "word_VARIABLES: variable names cannot begin with '__': '#{v}'",
            "This is a reserved variable naming convention"
          )
        end
        module_.add_variable(v)
      end
    end

    # ( value variable -- )
    def word_bang(interp)
      variable = interp.stack_pop
      value = interp.stack_pop
      variable.value = value
    end

    # ( variable -- value )
    def word_at(interp)
      variable = interp.stack_pop
      interp.stack_push(variable.value)
    end

    # ( value variable -- value )
    def word_bang_at(interp)
      variable = interp.stack_pop
      value = interp.stack_pop
      variable.value = value
      interp.stack_push(variable.value)
    end

    # ( string -- )
    def word_INTERPRET(interp)
      string = interp.stack_pop
      string_location = interp.get_string_location
      interp.run(string, string_location) if string
    end

    # ( names -- )
    def word_EXPORT(interp)
      names = interp.stack_pop
      interp.cur_module.add_exportable(names)
    end

    # ( names -- )
    def word_USE_MODULES(interp)
      names = interp.stack_pop

      names.each do |name|
        module_name, prefix = name.is_a?(Array) ? name : [name, name]
        mod = interp.find_module(module_name)
        interp.get_app_module.import_module(prefix, mod, interp)
      end
    end

    # ( key_vals -- rec )
    def word_REC(interp)
      key_vals = interp.stack_pop || []
      result = {}
      key_vals.each do |pair|
        key, val = pair
        result[key] = val
      end
      interp.stack_push(result)
    end

    # ( rec field -- value )
    # ( rec fields -- value )
    def word_REC_at(interp)
      field = interp.stack_pop
      rec = interp.stack_pop

      if rec.nil?
        interp.stack_push(nil)
        return
      end

      fields = field.is_a?(Array) ? field : [field]
      result = drill_for_value(rec, fields)
      interp.stack_push(result)
    end

    # ( rec value field -- rec )
    def word_l_REC_bang(interp)
      field = interp.stack_pop
      value = interp.stack_pop
      rec = interp.stack_pop || {}

      fields = field.is_a?(Array) ? field : [field]

      cur_rec = rec
      fields[0...-1].each do |f|
        cur_rec[f] ||= {}
        cur_rec = cur_rec[f]
      end

      cur_rec[fields[-1]] = value
      interp.stack_push(rec)
    end

    # ( array item -- array )
    # ( record key/val -- record )
    def word_APPEND(interp)
      item = interp.stack_pop
      result = interp.stack_pop

      result ||= []

      if result.is_a?(Array)
        result.push(item)
      else
        result[item[0]] = item[1]
      end

      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- record )
    def word_REVERSE(interp)
      result = interp.stack_pop

      if result.nil?
        interp.stack_push(result)
        return
      end

      result = result.reverse if result.is_a?(Array)

      interp.stack_push(result)
    end

    # ( array -- array )
    def word_UNIQUE(interp)
      container = interp.stack_pop

      if container.nil?
        interp.stack_push(container)
        return
      end

      result = container
      result = container.uniq if container.is_a?(Array)

      interp.stack_push(result)
    end

    # ( array index -- array )
    # ( record key -- record )
    def word_L_DEL(interp)
      key = interp.stack_pop
      container = interp.stack_pop

      if container.nil?
        interp.stack_push(container)
        return
      end

      if container.is_a?(Array)
        container.delete_at(key)
      else
        container.delete(key)
      end

      interp.stack_push(container)
    end

    # ( array old_keys new_keys -- array )
    # ( record old_keys new_keys -- record )
    def word_RELABEL(interp)
      new_keys = interp.stack_pop
      old_keys = interp.stack_pop
      container = interp.stack_pop

      if container.nil?
        interp.stack_push(container)
        return
      end

      raise "RELABEL: old_keys and new_keys must be same length" if old_keys.length != new_keys.length

      new_to_old = {}
      old_keys.each_with_index do |old_key, i|
        new_to_old[new_keys[i]] = old_key
      end

      result = if container.is_a?(Array)
        new_to_old.keys.sort.map { |k| container[new_to_old[k]] }
      else
        new_to_old.each_with_object({}) { |(new_key, old_key), res| res[new_key] = container[old_key] }
      end

      interp.stack_push(result)
    end

    # ( array field -- field_to_item )
    # ( record field -- field_to_item )
    def word_BY_FIELD(interp)
      field = interp.stack_pop
      container = interp.stack_pop

      container ||= []

      values = if container.is_a?(Array)
        container
      else
        container.values
      end

      result = {}
      values.each do |v|
        result[v[field]] = v if v
      end

      interp.stack_push(result)
    end

    # ( array field -- field_to_items )
    # ( record field -- field_to_items )
    def word_GROUP_BY_FIELD(interp)
      field = interp.stack_pop
      container = interp.stack_pop

      container ||= []

      values = if container.is_a?(Array)
        container
      else
        container.values
      end

      result = {}
      values.each do |v|
        field_val = v[field]
        if field_val.is_a?(Array)
          field_val.each do |fv|
            result[fv] ||= []
            result[fv].push(v)
          end
        else
          result[field_val] ||= []
          result[field_val].push(v)
        end
      end

      interp.stack_push(result)
    end

    # ( array forthic -- group_to_items )
    # ( record forthic -- group_to_items )
    def word_GROUP_BY(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      container = interp.stack_pop

      flags = interp.get_flags(module_id)

      container ||= []

      keys, values = if container.is_a?(Array)
        [Array.new(container.size) { |i| i }, container]
      else
        [container.keys, container.values]
      end

      result = {}
      values.each_with_index do |value, i|
        key = keys[i]
        interp.stack_push(key) if flags[:with_key]
        interp.stack_push(value)
        interp.run(forthic, string_location)
        group = interp.stack_pop
        result[group] ||= []
        result[group].push(value)
      end

      interp.stack_push(result)
    end

    # ( array n -- arrays )
    # ( record n -- records )
    def word_GROUPS_OF(interp)
      size = interp.stack_pop
      container = interp.stack_pop
      raise "GROUPS-OF requires group size > 0" if size <= 0

      container ||= []
      result = if container.is_a?(Array)
        group_items(container, size)
      else
        keys = container.keys
        key_groups = group_items(keys, size)
        key_groups.map { |ks| extract_rec(container, ks) }
      end

      interp.stack_push(result)
    end

    # ( array forthic -- record )
    def word_INDEX(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location
      items = interp.stack_pop

      if !items
        interp.stack_push(items)
        return
      end

      result = {}
      items.each do |item|
        interp.stack_push(item)
        interp.run(forthic, string_location)
        keys = interp.stack_pop
        keys.each do |k|
          lowercased_key = k.downcase
          if result[lowercased_key]
            result[lowercased_key].push(item)
          else
            result[lowercased_key] = [item]
          end
        end
      end
      interp.stack_push(result)
    end

    # ( items forthic -- [ ? ] )
    def word_MAP(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location
      items = interp.stack_pop
      flags = interp.get_flags(module_id)

      map_word = MapWord.new(items, forthic, string_location, flags)
      map_word.execute(interp)
    end

    # ( items forthic -- )
    def word_FOREACH(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      items = interp.stack_pop
      flags = interp.get_flags(module_id)

      if !items
        interp.stack_push(items)
        return
      end

      errors = []
      if items.is_a?(Array)
        items.each_with_index do |item, i|
          if flags[:with_key]
            interp.stack_push(i)
          end
          interp.stack_push(item)
          if flags[:push_error]
            errors.push(execute_returning_error(interp, forthic, string_location))
          else
            interp.run(forthic, string_location)
          end
        end
      else
        items.each do |k, item|
          if flags[:with_key]
            interp.stack_push(k)
          end
          interp.stack_push(item)
          if flags[:push_error]
            errors.push(execute_returning_error(interp, forthic, string_location))
          else
            interp.run(forthic, string_location)
          end
        end
      end

      if flags[:push_error]
        interp.stack_push(errors)
      end
    end

    # ( record -- record )
    def word_INVERT_KEYS(interp)
      record = interp.stack_pop
      result = {}
      record.each do |first_key, sub_record|
        sub_record.each do |second_key, value|
          result[second_key] ||= {}
          result[second_key][first_key] = value
        end
      end
      interp.stack_push(result)
    end

    # ( array1 array2 -- array )
    # ( record1 record2 -- record )
    def word_ZIP(interp)
      container2 = interp.stack_pop
      container1 = interp.stack_pop

      container1 ||= []
      container2 ||= []

      result = if container2.is_a?(Array)
        container1.map.with_index { |v, i| [v, container2[i]] }
      else
        container1.each_with_object({}) { |(k, v), res| res[k] = [v, container2[k]] }
      end

      interp.stack_push(result)
    end

    # ( array1 array2 forthic -- array )
    # ( record1 record2 forthic -- record )
    def word_ZIP_WITH(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      container2 = interp.stack_pop
      container1 = interp.stack_pop

      container1 ||= []
      container2 ||= []

      result = if container2.is_a?(Array)
        container1.map.with_index do |v, i|
          interp.stack_push(v)
          interp.stack_push(container2[i])
          interp.run(forthic, string_location)
          interp.stack_pop
        end
      else
        container1.each_with_object({}) do |(k, v), res|
          interp.stack_push(v)
          interp.stack_push(container2[k])
          interp.run(forthic, string_location)
          res[k] = interp.stack_pop
        end
      end

      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- array )
    def word_KEYS(interp)
      container = interp.stack_pop

      container ||= []

      result = if container.is_a?(Array)
        container.each_index.to_a
      else
        container.keys
      end

      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- array )
    def word_VALUES(interp)
      container = interp.stack_pop

      container ||= []

      result = if container.is_a?(Array)
        container
      else
        container.values
      end

      interp.stack_push(result)
    end

    # ( array -- length )
    # ( record -- length )
    def word_LENGTH(interp)
      container = interp.stack_pop

      if container.nil?
        interp.stack_push(container)
        return
      end

      result = container.length
      interp.stack_push(result)
    end

    # ( array start end -- array )
    # ( record start end -- record )
    def word_RANGE(interp)
      fend = interp.stack_pop
      fend_string_location = interp.get_string_location

      fstart = interp.stack_pop
      fstart_string_location = interp.get_string_location

      array = interp.stack_pop

      array ||= []

      start_found = false
      end_found = false

      start_index = nil
      end_index = nil

      array.each_with_index do |item, index|
        unless start_found
          interp.stack_push(item)
          interp.run(fstart, fstart_string_location)
          start_found = interp.stack_pop
          start_index = index if start_found
        end

        if start_found && !end_found
          interp.stack_push(item)
          interp.run(fend, fend_string_location)
          end_found = interp.stack_pop
          end_index = index if end_found
          break if end_found
        end
      end

      interp.stack_push([start_index, end_index])
    end

    # ( array start end -- array )
    # ( record start end -- record )
    def word_SLICE(interp)
      fend = interp.stack_pop.to_i
      fstart = interp.stack_pop.to_i
      container = interp.stack_pop

      container ||= []

      length = container.length

      start = normalize_index(fstart, length)
      fend = normalize_index(fend, length)

      step = 1
      step = -1 if start > fend

      indexes = [start]
      indexes = [] if start < 0 || start >= length

      while start != fend
        start += step
        if start < 0 || start >= length
          indexes.push(nil)
        else
          indexes.push(start)
        end
      end

      result = if container.is_a?(Array)
        indexes.map { |i| i.nil? ? nil : container[i] }
      else
        keys = container.keys.sort
        indexes.each_with_object({}) do |i, res|
          next if i.nil?

          k = keys[i]
          res[k] = container[k]
        end
      end

      interp.stack_push(result)
    end

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_DIFFERENCE(interp)
      rcontainer = interp.stack_pop
      lcontainer = interp.stack_pop

      lcontainer ||= []
      rcontainer ||= []

      result = if rcontainer.is_a?(Array)
        lcontainer - rcontainer
      else
        diff = lcontainer.keys - rcontainer.keys
        diff.each_with_object({}) { |k, res| res[k] = lcontainer[k] }
      end

      interp.stack_push(result)
    end

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_INTERSECTION(interp)
      rcontainer = interp.stack_pop
      lcontainer = interp.stack_pop

      lcontainer ||= []
      rcontainer ||= []

      result = if rcontainer.is_a?(Array)
        lcontainer & rcontainer
      else
        lkeys = lcontainer.keys
        rkeys = rcontainer.keys

        intersect = lkeys & rkeys
        intersect.each_with_object({}) { |k, res| res[k] = lcontainer[k] }
      end

      interp.stack_push(result)
    end

    # ( larray rarray -- array )
    # ( lrecord rrecord -- record )
    def word_UNION(interp)
      rcontainer = interp.stack_pop
      lcontainer = interp.stack_pop

      lcontainer ||= []
      rcontainer ||= []

      result = if rcontainer.is_a?(Array)
        lcontainer | rcontainer
      else
        lkeys = lcontainer.keys
        rkeys = rcontainer.keys

        keys = lkeys | rkeys
        keys.each_with_object({}) do |k, res|
          val = lcontainer[k]
          val = rcontainer[k] if val.nil?
          res[k] = val
        end
      end

      interp.stack_push(result)
    end

    # ( larray forthic -- array )
    # ( lrecord forthic -- record )
    def word_SELECT(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      container = interp.stack_pop
      flags = interp.get_flags(module_id)

      if !container
        interp.stack_push(container)
        return
      end

      result = nil
      if container.is_a?(Array)
        result = []
        container.each_with_index do |item, i|
          interp.stack_push(i) if flags[:with_key]
          interp.stack_push(item)
          interp.run(forthic, string_location)
          should_select = interp.stack_pop
          result.push(item) if should_select
        end
      else
        result = {}
        container.each do |k, v|
          interp.stack_push(k) if flags[:with_key]
          interp.stack_push(v)
          interp.run(forthic, string_location)
          should_select = interp.stack_pop
          result[k] = v if should_select
        end
      end
      interp.stack_push(result)
    end

    # ( array n -- taken rest )
    # ( record n -- taken rest )
    def word_TAKE(interp)
      n = interp.stack_pop
      container = interp.stack_pop
      flags = interp.get_flags(module_id)

      container ||= []

      taken = nil
      rest = nil

      if container.is_a?(Array)
        taken = container[0...n]
        rest = container[n..]
      else
        keys = container.keys.sort
        taken_keys = keys[0...n]
        rest_keys = keys[n..]
        taken = taken_keys.each_with_object({}) { |k, res| res[k] = container[k] }
        rest = rest_keys.each_with_object({}) { |k, res| res[k] = container[k] }
      end

      interp.stack_push(taken)
      interp.stack_push(rest) if flags[:push_rest]
    end

    # ( array n -- array )
    # ( record n -- record )
    def word_DROP(interp)
      n = interp.stack_pop
      container = interp.stack_pop

      if !container
        interp.stack_push(container)
        return
      end

      result = nil
      if container.is_a?(Array)
        result = container[n..]
      else
        keys = container.keys.sort
        rest_keys = keys[n..]
        result = rest_keys.each_with_object({}) { |k, res| res[k] = container[k] }
      end

      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- record )
    def word_ROTATE(interp)
      container = interp.stack_pop

      result = container
      if !container
        result = container
      elsif container.is_a?(Array)
        result = container
        if container.length > 0
          val = result.pop
          result.unshift(val)
        end
      else
        result = container
      end

      interp.stack_push(result)
    end

    # ( val -- bool )
    def word_ARRAY_q(interp)
      val = interp.stack_pop
      result = val.is_a?(Array)
      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- record )
    def word_SHUFFLE(interp)
      container = interp.stack_pop

      if !container
        interp.stack_push(container)
        return
      end
      result = if container.is_a?(Array)
        container.shuffle
      else
        container
      end

      interp.stack_push(result)
    end

    # ( array -- array )
    # ( record -- record )
    def word_SORT(interp)
      flag_string_position = interp.get_string_location
      container = interp.stack_pop

      flags = interp.get_flags(module_id)
      comparator = flags[:comparator]

      container ||= []
      unless container.is_a?(Array)
        interp.stack_push(container)
        return
      end

      # Figure out what to do
      result = if comparator.is_a?(String)
        sort_with_forthic(interp, comparator, flag_string_position, container)
      elsif comparator.nil?
        sort_without_comparator(container)
      else
        sort_with_key_func(container, comparator)
      end

      interp.stack_push(result)
    end

    # ( field -- key_func )
    def word_FIELD_KEY_FUNC(interp)
      field = interp.stack_pop

      result = lambda do |record|
        record[field]
      end

      interp.stack_push(result)
    end

    # ( array n -- item )
    # ( record n -- value )
    def word_NTH(interp)
      n = interp.stack_pop
      container = interp.stack_pop

      if n.nil? || !container
        interp.stack_push(nil)
        return
      end

      result = nil
      if container.is_a?(Array)
        if n < 0 || n >= container.length
          interp.stack_push(nil)
          return
        end
        result = container[n]
      else
        if n < 0 || n >= container.keys.length
          interp.stack_push(nil)
          return
        end
        keys = container.keys.sort
        key = keys[n]
        result = container[key]
      end

      interp.stack_push(result)
    end

    # ( array -- item )
    # ( record -- value )
    def word_LAST(interp)
      container = interp.stack_pop

      if !container
        interp.stack_push(nil)
        return
      end

      result = nil
      if container.is_a?(Array)
        result = if container.length == 0
          nil
        else
          container[container.length - 1]
        end
      else
        keys = container.keys.sort
        result = if keys.length == 0
          nil
        else
          container[keys[keys.length - 1]]
        end
      end

      interp.stack_push(result)
    end

    # ( array -- a1 a2 .. an )
    # ( record -- v1 v2 .. vn )
    def word_UNPACK(interp)
      container = interp.stack_pop

      container ||= []

      if container.is_a?(Array)
        container.each do |item|
          interp.stack_push(item)
        end
      else
        keys = container.keys.sort
        keys.each do |k|
          interp.stack_push(container[k])
        end
      end
    end

    # ( array -- array )
    # ( record -- record )
    def word_FLATTEN(interp)
      nested = interp.stack_pop
      flags = interp.get_flags(module_id)

      nested ||= []
      depth = flags[:depth]

      result = if nested.is_a?(Array)
        flatten_array(nested, depth)
      else
        flatten_record(nested, depth, {}, [])
      end

      interp.stack_push(result)
    end

    # ( array item -- index )
    # ( record item -- key )
    def word_KEY_OF(interp)
      item = interp.stack_pop
      container = interp.stack_pop

      container ||= []

      result = nil
      if container.is_a?(Array)
        index = container.index(item)
        # If index is nil or < 0, return nil
        result = (index.nil? || index < 0) ? nil : index
      else
        keys = container.keys
        keys.each do |k|
          v = container[k]
          if v == item
            result = k
            break
          end
        end
      end

      interp.stack_push(result)
    end

    # ( array init forthic -- value )
    # ( record init forthic -- value )
    def word_REDUCE(interp)
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      initial = interp.stack_pop
      container = interp.stack_pop

      container ||= []

      interp.stack_push(initial)

      if container.is_a?(Array)
        container.each do |item|
          interp.stack_push(item)
          interp.run(forthic, string_location)
        end
      else
        container.each do |k, v|
          interp.stack_push(v)
          interp.run(forthic, string_location)
        end
      end

      result = interp.stack_pop

      interp.stack_push(result)
    end

    # ( a -- )
    def word_POP(interp)
      interp.stack_pop
    end

    # ( a -- a a )
    def word_DUP(interp)
      a = interp.stack_pop
      interp.stack_push(a)
      interp.stack_push(a)
    end

    # ( a b -- b a )
    def word_SWAP(interp)
      b = interp.stack_pop
      a = interp.stack_pop
      interp.stack_push(b)
      interp.stack_push(a)
    end

    # ( item -- str )
    def word_to_STR(interp)
      item = interp.stack_pop
      interp.stack_push(item.to_s)
    end

    # ( str1 str2 -- str )
    # ( array_of_str -- str )
    def word_CONCAT(interp)
      str2 = interp.stack_pop
      array = str2.is_a?(Array) ? str2 : [interp.stack_pop, str2]
      result = array.join("")
      interp.stack_push(result)
    end

    # ( string sep -- items )
    def word_SPLIT(interp)
      sep = interp.stack_pop
      string = interp.stack_pop

      if !string
        string = ""
      end

      result = string.split(sep)
      interp.stack_push(result)
    end

    # ( strings sep -- string )
    def word_JOIN(interp)
      sep = interp.stack_pop
      strings = interp.stack_pop

      if !strings
        strings = []
      end

      result = strings.join(sep)
      interp.stack_push(result)
    end

    # ( -- char )
    def word_slash_N(interp)
      interp.stack_push("\n")
    end

    # ( -- char )
    def word_slash_R(interp)
      interp.stack_push("\r")
    end

    # ( -- char )
    def word_slash_T(interp)
      interp.stack_push("\t")
    end

    # ( A -- a )
    def word_LOWERCASE(interp)
      string = interp.stack_pop
      result = string ? string.downcase : ""
      interp.stack_push(result)
    end

    # ( a -- A )
    def word_UPPERCASE(interp)
      string = interp.stack_pop
      result = string ? string.upcase : ""
      interp.stack_push(result)
    end

    # ( string -- string )
    def word_ASCII(interp)
      string = interp.stack_pop

      if !string
        string = ""
      end

      result = ""
      string.each_char do |ch|
        result += ch if ch.ord < 256
      end
      interp.stack_push(result)
    end

    # ( string -- string )
    def word_STRIP(interp)
      string = interp.stack_pop
      result = string ? string.strip : ""
      interp.stack_push(result)
    end

    # ( string text replace -- string )
    def word_REPLACE(interp)
      replace = interp.stack_pop
      text = interp.stack_pop
      string = interp.stack_pop

      result = string
      if string
        pattern = Regexp.new(text)
        result = string.gsub(pattern, replace)
      end
      interp.stack_push(result)
    end

    # ( string pattern -- match )
    def word_RE_MATCH(interp)
      pattern = interp.stack_pop
      string = interp.stack_pop

      re_pattern = Regexp.new(pattern)
      result = false
      if string
        result = string.match(re_pattern)
      end
      interp.stack_push(result)
    end

    # ( match num -- string )
    def word_RE_MATCH_GROUP(interp)
      num = interp.stack_pop
      match = interp.stack_pop
      result = nil
      if match
        result = match[num]
      end
      interp.stack_push(result)
    end

    # ( string pattern -- matches )
    def word_RE_MATCH_ALL(interp)
      pattern = interp.stack_pop
      string = interp.stack_pop

      re_pattern = Regexp.new(pattern)
      matches = []
      if string
        matches = string.scan(re_pattern)
      end
      result = matches.map { |m| m[0] }
      interp.stack_push(result)
    end

    # ( -- nil )
    def word_NULL(interp)
      interp.stack_push(nil)
    end

    # ( value default -- value )
    def word_DEFAULT(interp)
      default_value = interp.stack_pop
      value = interp.stack_pop
      result = (value.nil? || value == "") ? default_value : value
      interp.stack_push(result)
    end

    # ( value default_forthic -- value )
    def word_star_DEFAULT(interp)
      default_forthic = interp.stack_pop
      value = interp.stack_pop

      if value.nil? || value == ""
        interp.run(default_forthic)
        value = interp.stack_pop
      end
      interp.stack_push(value)
    end

    # ( item forthic num-times -- ? )
    def word_l_REPEAT(interp)
      num_times = interp.stack_pop
      forthic = interp.stack_pop
      string_location = interp.get_string_location

      num_times.times do
        item = interp.stack_pop
        interp.stack_push(item)

        interp.run(forthic, string_location)
        res = interp.stack_pop

        interp.stack_push(item)
        interp.stack_push(res)
      end
    end

    # ( value num_places -- str )
    def word_to_FIXED(interp)
      num_places = interp.stack_pop
      value = interp.stack_pop
      result = if value.nil?
        ""
      elsif !value.is_a?(Numeric)
        value
      else
        # Round value to num_places
        value.round(num_places)
      end
      interp.stack_push(result.to_s)
    end

    # ( object -- json )
    def word_to_JSON(interp)
      object = interp.stack_pop
      result = object.to_json
      interp.stack_push(result)
    end

    # ( json -- object )
    def word_JSON_to(interp)
      json = interp.stack_pop
      result = JSON.parse(json)
      interp.stack_push(result)
    end

    # ( -- )
    def word_dot_s(interp)
      stack = interp.stack
      puts "===> Stack <==="
      if stack.length > 0
        p stack[stack.length - 1]
      else
        puts "<STACK EMPTY>"
      end
      interp.halt
    end

    # ( -- )
    def word_dot_S(interp)
      # Print full stack in reverse with index, pretty printing stack value
      puts "===> Stack <==="
      # Get reversed stack
      reversed_stack = interp.stack.reverse
      reversed_stack.each_with_index do |item, i|
        puts "#{i}: #{item.inspect}"
      end
    end

    # ( time -- time )
    def word_AM(interp)
      time = interp.stack_pop
      raise "AM expecting a time" unless time.is_a?(Time)

      result = time
      if time.hour >= 12
        result = Time.new(time.year, time.month, time.day, time.hour - 12, time.min, time.sec)
      end
      interp.stack_push(result)
    end

    # ( time -- time )
    def word_PM(interp)
      time = interp.stack_pop
      raise "PM expecting a time" unless time.is_a?(Time)

      result = time
      if time.hour < 12
        result = Time.new(time.year, time.month, time.day, time.hour + 12, time.min, time.sec)
      end
      interp.stack_push(result)
    end

    # ( -- time )
    def word_NOW(interp)
      result = Time.now
      interp.stack_push(result)
    end

    # ( obj -- time )
    def word_to_TIME(interp)
      obj = interp.stack_pop
      result = if obj.is_a?(Time)
        obj
      else
        Time.parse(obj.to_s)
      end
      interp.stack_push(result)
    end

    # ( obj -- date )
    def word_to_DATE(interp)
      obj = interp.stack_pop
      result = if obj.is_a?(Date)
        obj
      else
        Date.parse(obj.to_s)
      end
      interp.stack_push(result)
    end

    # ( -- date )
    def word_TODAY(interp)
      result = Date.today
      interp.stack_push(result)
    end

    # ( -- date )
    def word_MONDAY(interp)
      result = get_day_this_week(0)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_TUESDAY(interp)
      result = get_day_this_week(1)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_WEDNESDAY(interp)
      result = get_day_this_week(2)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_THURSDAY(interp)
      result = get_day_this_week(3)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_FRIDAY(interp)
      result = get_day_this_week(4)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_SATURDAY(interp)
      result = get_day_this_week(5)
      interp.stack_push(result)
    end

    # ( -- date )
    def word_SUNDAY(interp)
      result = get_day_this_week(6)
      interp.stack_push(result)
    end

    # ( date num-days -- date )
    def word_ADD_DAYS(interp)
      num_days = interp.stack_pop
      date = interp.stack_pop

      result = date + num_days
      interp.stack_push(result)
    end

    # ( l_date r_date -- num_days )
    def word_SUBTRACT_DATES(interp)
      r_date = interp.stack_pop
      l_date = interp.stack_pop
      result = (l_date - r_date).to_i
      interp.stack_push(result)
    end

    # ( a b -- a+b )
    # ( items -- sum )
    def word_plus(interp)
      items = interp.stack_pop

      if items.is_a?(Array)
        sum = 0
        items.each do |item|
          sum += item
        end
        interp.stack_push(sum)
      else
        b = items
        a = interp.stack_pop
        interp.stack_push(a + b)
      end
    end

    # ( a b -- a - b )
    def word_minus(interp)
      b = interp.stack_pop
      a = interp.stack_pop
      interp.stack_push(a - b)
    end

    # ( a b -- a*b )
    def word_times(interp)
      b = interp.stack_pop
      result = 1
      numbers = []

      if b.is_a?(Array)
        numbers = b
      else
        a = interp.stack_pop
        numbers = [a, b]
      end

      nil_found = numbers.any?(&:nil?)
      if nil_found
        interp.stack_push(nil)
        return
      end

      numbers.each do |num|
        result *= num
      end

      interp.stack_push(result)
    end

    # ( a b -- a/b )
    def word_divide_by(interp)
      b = interp.stack_pop
      a = interp.stack_pop
      result = a / b
      interp.stack_push(result)
    end

    # ( value mod -- remainder )
    def word_MOD(interp)
      mod = interp.stack_pop
      value = interp.stack_pop
      result = value % mod
      interp.stack_push(result)
    end

    # ( numbers -- mean )
    # ( records -- mean_record )
    def word_MEAN(interp)
      values = interp.stack_pop

      if !values || values.empty?
        interp.stack_push(0)
        return
      end

      result = compute_mean(values)
      interp.stack_push(result)
    end

    # ( num1 num2 -- num )
    # ( numbers -- num )
    def word_MAX(interp)
      num2 = interp.stack_pop

      numbers = []
      if num2.is_a?(Array)
        numbers = num2
      else
        num1 = interp.stack_pop
        numbers = [num1, num2]
      end

      interp.stack_push(numbers.max)
    end

    # ( num1 num2 -- num )
    # ( numbers -- num )
    def word_MIN(interp)
      num2 = interp.stack_pop

      numbers = []
      if num2.is_a?(Array)
        numbers = num2
      else
        num1 = interp.stack_pop
        numbers = [num1, num2]
      end

      interp.stack_push(numbers.min)
    end

    # ( a -- a )
    def word_ROUND(interp)
      a = interp.stack_pop
      result = a.round
      interp.stack_push(result)
    end

    # ( a b -- a == b )
    # ( items -- all_equal )
    def word_equal_equal(interp)
      items = interp.stack_pop

      if items.is_a?(Array)
        all_equal = items.all? { |item| item == items[0] }
        interp.stack_push(all_equal)
      else
        b = items
        a = interp.stack_pop
        interp.stack_push(a == b)
      end
    end

    # ( a b -- a != b )
    # ( items -- all_not_equal )
    def word_not_equal(interp)
      items = interp.stack_pop

      if items.is_a?(Array)
        all_not_equal = items.all? { |item| item != items[0] }
        interp.stack_push(all_not_equal)
      else
        b = items
        a = interp.stack_pop
        interp.stack_push(a != b)
      end
    end

    # ( l r -- bool )
    def word_greater_than(interp)
      r = interp.stack_pop
      l = interp.stack_pop

      if l.nil? || r.nil?
        interp.stack_push(nil)
        return
      end

      result = l > r
      interp.stack_push(result)
    end

    # ( l r -- bool )
    def word_greater_than_or_equal(interp)
      r = interp.stack_pop
      l = interp.stack_pop

      if l.nil? || r.nil?
        interp.stack_push(nil)
        return
      end

      result = l >= r
      interp.stack_push(result)
    end

    # ( l r -- bool )
    def word_less_than(interp)
      r = interp.stack_pop
      l = interp.stack_pop

      if l.nil? || r.nil?
        interp.stack_push(nil)
        return
      end

      result = l < r
      interp.stack_push(result)
    end

    # ( l r -- bool )
    # ( items -- bool )
    def word_OR(interp)
      r = interp.stack_pop

      items = r.is_a?(Array) ? r : [interp.stack_pop, r]
      result = items.any? { |item| item }
      interp.stack_push(result)
    end

    # ( l r -- bool )
    # ( items -- bool )
    def word_AND(interp)
      r = interp.stack_pop

      items = r.is_a?(Array) ? r : [interp.stack_pop, r]
      result = items.all? { |item| item }
      interp.stack_push(result)
    end

    # ( bool -- bool )
    def word_NOT(interp)
      value = interp.stack_pop
      interp.stack_push(!value)
    end

    # ( item items -- bool )
    def word_IN(interp)
      items = interp.stack_pop
      item = interp.stack_pop

      if !items
        items = []
      end
      result = items.include?(item)
      interp.stack_push(result)
    end

    # ( vals required_vals -- bool )
    def word_ANY(interp)
      required_vals = interp.stack_pop
      vals = interp.stack_pop

      result = false
      required_vals.each do |rv|
        if vals.include?(rv)
          result = true
          break
        end
      end

      # If nothing is required, then all values are true
      result = true if required_vals.empty?

      interp.stack_push(result)
    end

    # ( vals required_vals -- bool )
    def word_ALL(interp)
      required_vals = interp.stack_pop
      vals = interp.stack_pop

      vals ||= []
      required_vals ||= []

      result = required_vals.all? { |val| vals.include?(val) }
      interp.stack_push(result)
    end

    # ( item -- bool )
    def word_to_BOOL(interp)
      item = interp.stack_pop
      result = if item.nil? || item == "" || item == 0
        false
      else
        !!item
      end
      interp.stack_push(result)
    end

    # ( item -- int )
    def word_to_INT(interp)
      str = interp.stack_pop
      result = str.to_i
      interp.stack_push(result)
    end

    # ( item -- float )
    def word_to_FLOAT(interp)
      str = interp.stack_pop
      result = str.to_f
      interp.stack_push(result)
    end

    # ( val start_ranges -- index )
    def word_RANGE_INDEX(interp)
      start_ranges = interp.stack_pop
      val = interp.stack_pop

      # Cap off the value ranges with infinity
      start_ranges.push(Float::INFINITY)

      if val.nil? || start_ranges.nil?
        interp.stack_push(nil)
        return
      end

      if val < start_ranges[0]
        interp.stack_push(nil)
        return
      end

      result = nil
      (0...start_ranges.length - 1).each do |i|
        if val >= start_ranges[i] && val < start_ranges[i + 1]
          result = i
          break
        end
      end

      interp.stack_push(result)
    end

    # ( -- Infinity )
    def word_INFINITY(interp)
      interp.stack_push(Float::INFINITY)
    end

    # ( l r -- bool )
    def word_less_than_or_equal(interp)
      r = interp.stack_pop
      l = interp.stack_pop

      if l.nil? || r.nil?
        interp.stack_push(nil)
        return
      end

      result = l <= r
      interp.stack_push(result)
    end

    # ( date -- str )
    def word_DATE_to_STR(interp)
      date = interp.stack_pop
      result = date_to_string(date)
      interp.stack_push(result)
    end

    # ( time -- str )
    def word_TIME_to_STR(interp)
      time = interp.stack_pop
      result = time.strftime("%H:%M")
      interp.stack_push(result)
    end

    # ( date time -- datetime )
    def word_DATE_TIME_to_DATETIME(interp)
      time = interp.stack_pop
      date = interp.stack_pop
      dt_string = "#{date.year}-#{date.month}-#{date.day} #{time.hour}:#{time.min}"
      result = Time.parse(dt_string)
      interp.stack_push(result)
    end

    # ( datetime -- timestamp )
    def word_DATETIME_to_TIMESTAMP(interp)
      datetime = interp.stack_pop
      result = datetime.to_i
      interp.stack_push(result)
    end

    # ( timestamp -- datetime )
    def word_TIMESTAMP_to_DATETIME(interp)
      timestamp = interp.stack_pop
      result = Time.at(timestamp)
      interp.stack_push(result)
    end

    # ( str -- datetime )
    def word_STR_to_DATETIME(interp)
      s = interp.stack_pop
      result = Time.parse(s)
      interp.stack_push(result)
    end

    # ( str -- timestamp )
    def word_STR_to_TIMESTAMP(interp)
      s = interp.stack_pop
      datetime = Time.parse(s)
      result = datetime.to_i
      interp.stack_push(result)
    end

    # ( -- )
    def word_bang_PUSH_ERROR(interp)
      interp.modify_flags(module_id, {push_error: true})
    end

    # ( -- )
    def word_bang_WITH_KEY(interp)
      interp.modify_flags(module_id, {with_key: true})
    end

    # ( comparator -- )
    def word_bang_COMPARATOR(interp)
      comparator = interp.stack_pop
      interp.modify_flags(module_id, {comparator: comparator})
    end

    # ( -- )
    def word_bang_PUSH_REST(interp)
      interp.modify_flags(module_id, {push_rest: true})
    end

    # ( depth -- )
    #
    # NOTE: `depth` of 0 is the same not having set depth
    def word_bang_DEPTH(interp)
      depth = interp.stack_pop
      interp.modify_flags(module_id, {depth: depth})
    end

    # ( num_inteprs -- )
    def word_bang_INTERPS(interp)
      num_interps = interp.stack_pop
      interp.modify_flags(module_id, {interps: num_interps})
    end

    # ( -- )
    def word_PROFILE_START(interp)
      interp.start_profiling
    end

    # ( -- )
    def word_PROFILE_END(interp)
      interp.stop_profiling
    end

    # ( label -- )
    def word_PROFILE_TIMESTAMP(interp)
      label = interp.stack_pop
      interp.add_timestamp(label)
    end

    # ( -- )
    def word_PROFILE_DATA(interp)
      histogram = interp.word_histogram
      timestamps = interp.profile_timestamps

      result = {
        word_counts: [],
        timestamps: []
      }

      histogram.each do |val|
        rec = {word: val[:word], count: val[:count]}
        result[:word_counts].push(rec)
      end

      prev_time = 0.0
      timestamps.each do |t|
        rec = {
          label: t[:label],
          time_ms: t[:time_ms],
          delta: t[:time_ms] - prev_time
        }
        prev_time = t[:time_ms]
        result[:timestamps].push(rec)
      end

      interp.stack_push(result)
    end

    # ( string -- symbol )
    def word_to_SYM(interp)
      string = interp.stack_pop
      result = string.to_sym
      interp.stack_push(result)
    end

    # --------------------------------------------------
    # Helpers
    private

    def drill_for_value(rec, fields)
      cur_rec = rec
      fields.each do |f|
        if cur_rec.is_a?(Array) || cur_rec.is_a?(Hash)
          cur_rec = cur_rec[f]
        else
          return nil
        end
      end
      cur_rec
    end

    def date_to_string(date)
      date.strftime("%Y-%m-%d")
    end

    def group_items(items, group_size)
      num_groups = (items.length / group_size.to_f).ceil
      res = []
      remaining = items.dup
      num_groups.times do
        res.push(remaining.slice!(0, group_size))
      end
      res
    end

    def extract_rec(record, keys)
      res = {}
      keys.each { |k| res[k] = record[k] }
      res
    end

    def execute_returning_error(interp, forthic, string_location)
      result = nil
      begin
        interp.run(forthic, string_location)
      rescue => e
        result = e
      end
      result
    end

    def normalize_index(index, length)
      res = index
      res += length if index < 0
      res
    end

    # Default sort
    def sort_without_comparator(container)
      # Separate nil values from non-nil values
      nils, non_nils = container.partition(&:nil?)

      # Sort non_nils and append nils
      non_nils.sort + nils
    end

    # Sort using a forthic string
    def sort_with_forthic(interp, forthic, flag_string_position, container)
      aug_array = make_aug_array(interp, forthic, flag_string_position, container)
      aug_array.sort! { |l, r| cmp_items(l, r) }
      de_aug_array(aug_array)
    end

    def make_aug_array(interp, forthic, flag_string_position, vals)
      res = []
      vals.each do |val|
        interp.stack_push(val)
        interp.run(forthic, flag_string_position)
        aug_val = interp.stack_pop
        res.push([val, aug_val])
      end
      res
    end

    def cmp_items(l, r)
      l_val = l[1]
      r_val = r[1]

      if l_val < r_val
        -1
      elsif l_val > r_val
        1
      else
        0
      end
    end

    def de_aug_array(aug_vals)
      aug_vals.map { |aug_val| aug_val[0] }
    end

    # Sort with key func
    def sort_with_key_func(container, key_func)
      container.sort do |l, r|
        l_val = key_func.call(l)
        r_val = key_func.call(r)
        if l_val < r_val
          -1
        elsif l_val > r_val
          1
        else
          0
        end
      end
    end

    def add_to_record_result(item, key, keys, result)
      new_key = (keys + [key]).join("\t")
      result[new_key] = item
    end

    def fully_flatten_record(record, res, keys)
      record.each do |k, item|
        if is_record(item)
          fully_flatten_record(item, res, keys + [k])
        else
          add_to_record_result(item, k, keys, res)
        end
      end
      res
    end

    def flatten_record(record, depth, res, keys)
      return fully_flatten_record(record, res, keys) if depth.nil?

      record.each do |k, item|
        if depth > 0 && is_record(item)
          flatten_record(item, depth - 1, res, keys + [k])
        else
          add_to_record_result(item, k, keys, res)
        end
      end
      res
    end

    def flatten_array(array, depth)
      return array.flatten(depth) if depth
      array.flatten
    end

    def is_record(obj)
      obj.is_a?(Hash) && !obj.empty?
    end

    def get_day_this_week(day_of_week)
      # Assume the start of the week is Monday and a day_of_week of 0 means a Monday
      # Get the current day of the week
      today = Date.today
      current_day_of_week = today.wday

      # Return the date of the day_of_week
      today - (current_day_of_week - day_of_week)
    end

    # NOTE: Monday is the start of the week
    def normalize_day(day)
      day
    end

    def compute_number_mean(numbers)
      sum = numbers.reduce(0) { |acc, num| acc + num }
      sum.to_f / numbers.length
    end

    def compute_non_number_mean(objects)
      non_null_objects = objects.reject { |obj| obj.nil? }
      res = Hash.new(0)

      non_null_objects.each do |obj|
        obj_str = obj.is_a?(String) ? obj : obj.to_json
        res[obj_str] += 1
      end

      res.each do |key, value|
        res[key] = value.to_f / non_null_objects.length
      end
      res
    end

    def compute_object_mean(records)
      res = {}
      records.each do |record|
        record.each do |key, value|
          res[key] ||= []
          res[key] << value
        end
      end

      res.each do |key, values|
        res[key] = compute_mean(values)
      end
      res
    end

    def is_all_numbers(values)
      values.all? { |val| val.is_a?(Numeric) }
    end

    def is_all_records(values)
      values.all? { |val| val.is_a?(Hash) }
    end

    def select_non_null_values(values)
      values.reject { |val| val.nil? }
    end

    def compute_mean(values)
      result = nil
      if values.is_a?(Array)
        non_null_values = select_non_null_values(values)
        result = if is_all_numbers(non_null_values)
          compute_number_mean(non_null_values)
        elsif is_all_records(non_null_values)
          compute_object_mean(non_null_values)
        else
          compute_non_number_mean(non_null_values)
        end
      end
      result
    end
  end
end
