-- Forthic.Module: Contains words that can be reused across applications
--

module Forthic.Module (
empty_module,        -- Returns an empty Module to use as a base
require_modules,     -- Requires module for a Module and also adds it to the Interpreter
find_mod_word,       -- Searches module for a word, most recently checked first
add_mod_word,        -- Adds word to a Module
add_mod_words        -- Adds multiple words to a Module
) where

import Data.Map
import Forthic.Types
import Debug.Trace

-- Returns base empty Module
empty_module :: Module
empty_module = Module{
    mod_name = "",
    mod_parentage = [],
    mod_words = [],
    mod_exportable = [],
    mod_modules = [],
    mod_vars = empty,
    mod_init = \interp _ -> interp
}


-- Requires a Module by another Module, storing them in the Interpreter as well
require_module :: Interpreter -> Module -> Module -> Interpreter
require_module _ _ Module{mod_name=""} = error "Modules must not have an empty name"
require_module interp parent mod = module_stack_pop interp6'
    -- BUG? Oldest parents should be first
    where mod' = mod{mod_parentage=(mod_name parent):(mod_parentage parent)}
          interp' = module_stack_push interp (module_reference parent)
          interp2' = add_module interp' mod'
          interp3' = module_stack_push interp2' (module_reference mod')
          interp4' = (mod_init mod')  interp3' mod'
          interp5' = module_stack_pop interp4'
          parent_modules = mod_modules parent
          parent' = parent{mod_modules=(mod_name mod):parent_modules}
          -- BUG? When does required module get added to interp?
          interp6' = update_cur_module interp5' parent'


-- Requires multiple Modules by another Module, storing them in the Interpreter as well.
require_modules :: Interpreter -> Module -> [Module] -> Interpreter
require_modules interp parent [] = interp
require_modules interp parent (mod:rest) = require_modules interp' parent rest
    where interp' = require_module interp parent mod
          

-- Helper to search a Module for a word
find_mod_word' :: [ForthicWord] -> String -> Maybe ForthicWord
find_mod_word' [] _ = Nothing
find_mod_word' (w:ws) name
    | word_name w == name = Just w
    | otherwise           = find_mod_word' ws name


-- Searches Module for a word
find_mod_word :: Module -> String -> Maybe ForthicWord
find_mod_word mod name = find_mod_word' (mod_words mod) name

-- Adds word to a Module
add_mod_word :: Module -> ForthicWord -> Module
add_mod_word m@Module{mod_words=words} w = m{mod_words=w:words}

-- Adds multiple words to a Module
add_mod_words :: Module -> [ForthicWord] -> Module
add_mod_words mod [] = mod
add_mod_words mod (w:ws) = add_mod_words mod' ws
    where mod' = add_mod_word mod w
