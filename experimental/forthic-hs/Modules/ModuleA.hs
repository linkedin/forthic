module Modules.ModuleA (
module_a,
module_b,
module_c
) where

import Forthic.Types
import Forthic.StackItem
import Forthic.Module
import Forthic.Interpreter


word_MESSAGE :: ForthicWord
word_MESSAGE = ImplementedWord "MESSAGE" func
    where func = \interp -> stack_push interp (SString "Howdy")

module_a :: Module
module_a = empty_module{
               mod_name="module_a",
               mod_words=words,
               mod_exportable=["MESSAGE", "A1@", "A1!"],
               mod_init=minit
           }
    where words = [word_MESSAGE]
          minit = init_module_a
          forthic = module_a_forthic

module_a_forthic = "\
\ [ 'a_1' ] VARIABLES \n\
\ : TEST-STRING   '**Test**' ; \n\
\ : A1!   TEST-STRING a_1 ! ; \n\
\ : A1@   a_1 @ ;"

init_module_a :: Interpreter -> Module -> Interpreter
init_module_a interp mod_a = run_str interp' module_a_forthic
    where interp' = require_modules interp mod_a [module_b]
          run_str = interp_run_string interp


module_b :: Module
module_b = empty_module{mod_name="module_b", mod_words=words, mod_init=minit}
    where words = []
          minit = init_module_b

module_c :: Module
module_c = empty_module{mod_name="module_c", mod_words=words}
    where words = []

init_module_b :: Interpreter -> Module -> Interpreter
init_module_b interp mod_b = interp'
    where interp' = require_modules interp mod_b [module_c]
