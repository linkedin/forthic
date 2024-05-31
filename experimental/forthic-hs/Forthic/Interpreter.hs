-- Forthic.Interpreter: Implements the Forthic interpreter
-- This uses the Tokenizer to parse, interpret, and execute Forthic code.

module Forthic.Interpreter (
-- TODO: Have new_interp take a list of modules
new_interp,           -- Creates a blank interpreter
require_app_modules,  -- Adds the Modules required by the application
run                   -- Run a Forthic string through a configured Interpreter
) where

import Data.Maybe
import Data.Map
import Data.List
import Data.Time
import Data.Time.Format
import Debug.Trace

import Text.Regex.Posix

import Forthic.StackItem
import Forthic.Types
import Forthic.Tokenizer
import Forthic.Module
import Forthic.GlobalModule

-- Executes a Forthic string with a given Interpreter
run_string :: Interpreter -> String -> Interpreter
run_string interp s = handle_tokens interp $ tokens s

-- Starting point for creating Forthic definitions
empty_definition = ForthicDefinition "" []

-- TODO: Change this to take an array of modules to add to the application module
-- Starting point for configuring an Interpreter
--
-- There are two special Modules: the global module and the app module. The global
-- module is separate from the other modules and cannot be pushed onto the module stack.
-- If a word/variable/literal cannot be handled by any other Module, the global module
-- will be searched.
--
-- The app module is named with an empty string and is referred to by an empty string.
-- It is stored in the Interpreter modules and is treated the same way as any other Module.
new_interp :: Interpreter
new_interp = interp'
    where interp = Interpreter{
                     interp_stack = [],
                     interp_module_stack = [],
                     interp_modules = Data.Map.insert "" app_mod Data.Map.empty,
                     interp_global_module = global_mod,
                     interp_is_compiling = False,
                     interp_cur_definition = empty_definition,
                     interp_run_string = run_string
                     }
          global_mod = global_module
          app_mod = empty_module
          interp' = (mod_init global_mod) interp global_mod


-- Requires modules needed for the application
require_app_modules :: Interpreter -> [Module] -> Interpreter
require_app_modules interp modules = require_modules interp' app_mod modules 
    where interp' = interp{interp_module_stack=[]}
          app_mod = fromJust $ Data.Map.lookup "" (interp_modules interp)


-- Helper function to gather elements of array as an SArray
gather_array :: [StackItem] -> [StackItem] -> StackItem
gather_array accum []  = error "Couldn't find SStartArray"
gather_array accum (SStartArray:rest) = SArray accum
gather_array accum (item:rest) = gather_array (item:accum) rest


-- Helper function used for EndArrayToken
pop_vals_push_array :: [StackItem] -> [StackItem]
pop_vals_push_array stack = stack''
    where a@(SArray values) = gather_array [] stack 
          stack' = drop (1 + length values) stack
          stack'' = a:stack'

to_literal :: String -> Maybe StackItem
to_literal str
    | is_date = Just (SDate $ fromJust maybe_date)
    | is_time = Just (STime $ fromJust maybe_time)
    | is_float = Just (SFloat float_num)
    | is_int  = Just (SInt int_num)
    | otherwise = Nothing
    where is_date = (str =~ "[0-9]{4}-[0-1][0-9]-[0-3][0-9]")
          maybe_date = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day
          is_time = (str =~ "[0-2][0-9]:[0-5][0-9]")
          maybe_time = parseTimeM True defaultTimeLocale "%H:%M" str :: Maybe TimeOfDay
          is_int =  (str =~ "-?[0-9]+")
          int_num = read str :: Int
          is_float = (str =~ "-?[0-9]+\\.[0-9]*")
          float_num = read str :: Float

-- Searches a list of Modules for a word
find_word' :: [Module] -> String -> Maybe ForthicWord
find_word' [] _ = Nothing
find_word' (mod:mods) name
    | res == Nothing  = find_word' mods name
    | otherwise       = res
        where res = find_mod_word mod name

-- Searches Interpreter for a word
-- This could be a ForthicWord, Variable, or the result of a literal handler
find_word :: Interpreter -> String -> Maybe ForthicWord
find_word interp name
    | isJust maybe_word = maybe_word 
    | member name vars = Just (PushVariableWord name)
    | isJust maybe_literal = Just (PushLiteralWord (fromJust maybe_literal))
    | otherwise = Nothing
    where cur_mod = cur_module interp
          global_mod = interp_global_module interp
          maybe_word = find_word' [cur_mod, global_mod] name
          vars = mod_vars cur_mod
          maybe_literal = to_literal name

execute_memo :: Interpreter -> String -> String -> Interpreter
execute_memo interp name var_name
    | val == SNone  = stack_push interp'' val''
    | otherwise     = stack_push interp' val
    where run_str = flip (interp_run_string interp)
          get_val = stack_pop . run_str (var_name ++ " @")
          (val, interp') = get_val interp
          (val'', interp'') = get_val . run_str (name ++ "!") $ interp'


-- Executes a list of Forthic words
execute_words :: Interpreter -> [ForthicWord] -> Interpreter
execute_words interp [] = interp
execute_words interp (word:rest) = execute_words interp' rest
    where interp' = execute_word interp word

-- Execute a single ForthicWord
execute_word :: Interpreter -> ForthicWord -> Interpreter
execute_word interp (PushStringWord s) = stack_push interp (SString s)
execute_word interp (PushLiteralWord stack_item) = stack_push interp stack_item
execute_word interp (ForthicDefinition name words) = execute_words interp words
execute_word interp (ImplementedWord _ func) = func interp
execute_word interp (MemoWord name var_name) = execute_memo interp name var_name 
execute_word interp (ImportedWord _ mod_ref word) = interp3'
    where interp' = module_stack_push interp mod_ref
          interp2' = execute_word interp' word
          interp3' = module_stack_pop interp2'
execute_word interp (PushVariableWord name)
    | isJust maybe_var = stack_push interp (SVariable $ fromJust maybe_var)
    | otherwise        = error ("Can't find variable: " ++ name)
    where vars = mod_vars (cur_module interp)
          maybe_var = Data.Map.lookup name vars

-- Helper to execute a WordToken
execute :: Interpreter -> Token -> Interpreter
execute interp token@Token{tok_text=s}
    | res /= Nothing  = execute_word interp word
    | otherwise = error ("Unable to execute: " ++ show token)
    where res = find_word interp s
          word = fromJust res

-- Adds a word to the current definition of an Interpreter
add_word_to_def :: Interpreter -> ForthicWord -> Interpreter
add_word_to_def interp word = interp{interp_cur_definition=def'}
    where ForthicDefinition name words = interp_cur_definition interp
          def' = ForthicDefinition name (words ++ [word])


-- Complete a definition and add it to the cur module
end_cur_definition :: Interpreter -> Interpreter
end_cur_definition interp = interp'{interp_is_compiling=False}
    where cur_mod = cur_module interp
          cur_definition = interp_cur_definition interp
          cur_mod' = add_mod_word cur_mod cur_definition
          interp' = update_cur_module interp cur_mod'


-- ===== handle_token
handle_token :: Interpreter -> Token -> Interpreter

-- Handle token in normal mode
handle_token interp@Interpreter{interp_is_compiling=False} token
    | ttype == StringToken = interp{interp_stack=SString ttext:s}
    | ttype == StartArrayToken = interp{interp_stack=SStartArray:s}
    | ttype == EndArrayToken = interp{interp_stack=pop_vals_push_array s}
    | ttype == StartModuleToken && ttext == "" = module_stack_push interp ""
    | ttype == StartModuleToken = module_stack_push interp mod_ref
    | ttype == EndModuleToken = module_stack_pop interp
    | ttype == StartDefToken =
          interp{interp_is_compiling=True, interp_cur_definition=ForthicDefinition ttext []}
    | ttype == EndDefToken = error ("Unmatched end definition: " ++ show token)
    | ttype == WordToken = execute interp token
    | otherwise = trace ttext interp
    where ttype = tok_type token
          ttext = tok_text token
          s = interp_stack interp
          cur_mod = cur_module interp
          mod_parents = (mod_name cur_mod):(mod_parentage cur_mod)
          mod_ref = parentage_to_module_ref $ ttext:mod_parents

-- Compile token into current definition
handle_token interp@Interpreter{interp_is_compiling=True} token
    | ttype == StringToken = add_word_to_def interp $ PushStringWord ttext
    | ttype == StartDefToken = error ("Can't have nested definitions: " ++ show token)
    | ttype == EndDefToken = end_cur_definition interp
    | ttype == WordToken && isNothing maybe_word = error ("[Compiling] Unknown word: " ++ show token)
    | ttype == WordToken = add_word_to_def interp (fromJust maybe_word)
    | ttype == StartModuleToken = module_stack_push interp mod_ref
    | ttype == EndModuleToken = module_stack_pop interp

    | otherwise = trace ttext interp
    where ttype = tok_type token
          ttext = tok_text token
          s = interp_stack interp
          maybe_word = find_word interp ttext
          cur_mod = cur_module interp
          mod_parents = (mod_name cur_mod):(mod_parentage cur_mod)
          mod_ref = parentage_to_module_ref $ ttext:mod_parents


-- Runs a list of Tokens through an Interpreter
handle_tokens :: Interpreter -> [Token] -> Interpreter
handle_tokens interp [] = interp
handle_tokens interp (t:ts) = handle_tokens interp' ts
    where interp' = handle_token interp t


-- Runs a Forthic string through an Interpreter
run :: Interpreter -> String -> Interpreter
run interp string = run_string interp'' string
    where interp'' = module_stack_push interp' ""  -- Push app module
          interp' = interp{interp_module_stack=[]}
