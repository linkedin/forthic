# frozen_string_literal: true

require_relative "forthic/version"

module Forthic
  autoload :Tokenizer, "forthic/tokenizer"
  autoload :CodeLocation, "forthic/code_location"
  autoload :Token, "forthic/token"
  autoload :PositionedString, "forthic/positioned_string"
  autoload :ForthicError, "forthic/forthic_error"

  module Errors
    autoload :UnknownWordError, "forthic/errors/unknown_word_error"
  end
  autoload :Word, "forthic/words/word"
  autoload :PushValueWord, "forthic/words/push_value_word"
  autoload :DefinitionWord, "forthic/words/definition_word"
  autoload :ModuleMemoWord, "forthic/words/module_memo_word"
  autoload :ModuleMemoBangAtWord, "forthic/words/module_memo_bang_at_word"
  autoload :ModuleMemoBangWord, "forthic/words/module_memo_bang_word"
  autoload :ModuleMemoBangAtWord, "forthic/words/module_memo_bang_at_word"
  autoload :EndArrayWord, "forthic/words/end_array_word"
  autoload :StartModuleWord, "forthic/words/start_module_word"
  autoload :EndModuleWord, "forthic/words/end_module_word"
  autoload :MapWord, "forthic/words/map_word"
  autoload :ForthicModule, "forthic/forthic_module"
  autoload :GlobalModule, "forthic/global_module"
  autoload :Interpreter, "forthic/interpreter"
end
