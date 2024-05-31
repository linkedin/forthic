-- Forthic.Types: Defines all types used within the Forthic Interpreter
--
-- All types are defined in one file to prevent import cycles since the types
-- depend on each other.
--
-- Simple functions that operate on the types at a basic level are defined here
-- as well.
--
-- Tokenization types: TokenType, Token, Tokenizer
-- Interpreter types:  StackItem, Variable, ForthicWord, Module, Interpreter
--

module Forthic.Types (
Tokenizer(..),       -- Maintains current input location during tokenization
TokenType(..),       -- Differentiates parsed tokens
Token(..),           -- Contains type, matched text, and location of token
ForthicWord(..),     -- Can be stored in Forthic definitions and executed
Module(..),          -- Stores predefined Forthic words for re-use across applications
Interpreter(..),     -- Maintains interpreter state (primarily stack and modules)

as_SString,          -- If StackItem is an SString, Just SString; else Nothing
word_name,           -- Returns name of a ForthicWord
cur_module,          -- Returns Module at top of Interpreter's module stack
update_cur_module,   -- Writes cur module to Interpreter's modules store
stack_push,          -- Pushes StackItem onto Interpreter's stack
stack_pop,           -- Pops StackItem from Interpreter's stack
stack_pop_n,         -- Pops multiple StackItems from Interpreter's stack (in push order)
add_module,          -- Adds module to Interpreter store and registers it with the cur module
update_variable,     -- Writes Variable to Module
parentage_to_module_ref,  -- Converts a list of parent names to a module ref String
module_reference,    -- Returns module ref for a Module
find_module,         -- Searches Interpreter for Module given a module ref String
module_stack_push,   -- Pushes a module ref String onto Interpreter module stack
module_stack_pop     -- Pops module ref String from Interpreter module stack
) where

import Data.List
import Data.Map
import Data.Maybe
import Debug.Trace
import Forthic.StackItem

data TokenType =
    StringToken |
    CommentToken |
    StartArrayToken |
    EndArrayToken |
    StartModuleToken |
    EndModuleToken |
    StartDefToken |
    EndDefToken |
    WordToken |
    EOSToken
    deriving (Eq, Enum, Show)

-- Stores type, text, and location of a token
data Token = Token {
    tok_type :: TokenType,
    tok_text :: String,
    tok_line :: Int,
    tok_col :: Int
    } deriving(Show)

-- Maintains state of tokenization
data Tokenizer = Tokenizer {
    tokzr_input :: String,
    tokzr_line :: Int,
    tokzr_col :: Int
    } deriving(Show)



-- Used to check if StackItem is an SString
as_SString :: StackItem -> Maybe StackItem
as_SString item@(SString _) = Just item
as_SString _ = Nothing


-- Objects that can be executed by the Interpreter
data ForthicWord =
    ForthicDefinition String [ForthicWord] |
    PushStringWord String |
    PushVariableWord String |
    PushLiteralWord StackItem |
    ImplementedWord String (Interpreter -> Interpreter) |
    MemoWord String String |  -- name, var_name
    ImportedWord String String ForthicWord

-- Returns name of a ForthicWord
word_name :: ForthicWord -> String
word_name (ForthicDefinition name _) = name
word_name (PushStringWord _ )        = ""
word_name (ImplementedWord name _ )  = name
word_name (ImportedWord name _ _)  = name
word_name (MemoWord name _)  = name

instance Show ForthicWord where
    show (ForthicDefinition n ws) = "ForthicDefinition " ++ (show n) ++ " " ++ (show ws)
    show (PushStringWord s) = "PushStringWord " ++ s
    show (PushVariableWord s) = "PushVariableWord " ++ s
    show (ImplementedWord name func) = "<ImplementedWord:" ++ name ++ ">"
    show (ImportedWord name mod_ref word) = "<ImportedWord:" ++ name ++ ">"


instance Eq ForthicWord where
    ForthicDefinition l lws == ForthicDefinition r rws = l == r && lws == rws
    (PushStringWord l) == (PushStringWord r) = l == r
    ImplementedWord lname _ == ImplementedWord rname _ = lname == rname
    ImportedWord lname _ _ == ImportedWord rname _ _ = lname == rname
    _ == _ = False


-- Stores predefined words/variables for re-use across applications
data Module = Module{
    -- When a Module requires other Modules, it becomes a "parent" of those modules.
    -- The mod_parentage indicates the path by which a Module was required. The mod_modules
    -- field lists the names of the modules this module directly requires.
    mod_name :: String,
    mod_parentage :: [String],
    mod_modules :: [String],

    -- The primary responsibility of a Module is to store words and variables to implement
    -- reusable functionality. The mod_exportable field has names of words that are
    -- imported to a parent module via USE-MODULES
    mod_words :: [ForthicWord],
    mod_exportable :: [String],
    mod_vars :: (Map String Variable),

    -- The mod_init function is used to initialze the module. This function can do things
    -- like require other modules and execute arbitrary Forthic code
    mod_init :: Interpreter -> Module -> Interpreter
    } |
    NoModule

instance Show Module where
    show NoModule = "NoModule"
    show mod = "<Module:" ++ show (mod_name mod) ++ ">" ++
        " mod_parentage:" ++ show (mod_parentage mod) ++
        " mod_words:" ++ show (mod_words mod) ++
        " mod_modules:" ++ show (mod_modules mod) ++
        " mod_vars:" ++ show (keys $ mod_vars mod)

instance Eq Module where
    Module{mod_name=l} == Module{mod_name=r} = l == r
    NoModule == NoModule = True
    _ == NoModule = False
    NoModule == _ = False


-- Maintains Interpreter state, primarily stack and modules
data Interpreter = Interpreter{
    -- Stores arguments and results during execution
    interp_stack :: [StackItem],

    -- Stores module references as they are referred to in Forthic code
    interp_module_stack :: [String],

    -- All modules known to the interpreter are stored here. Modules are
    -- keyed by module reference Strings
    interp_modules :: (Map String Module),

    -- True if is compiling a definition. The definition being compiled is
    -- a ForthicDefinition in interp_cur_definition
    interp_is_compiling :: Bool,
    interp_cur_definition :: ForthicWord,

    -- The global_module implements words that are used in all applications and
    -- which require special access to the Interpreter
    interp_global_module :: Module,

    -- The interp_run_string is specified in the Interpreter module. It is in a
    -- separate file to avoid Haskell import cycles
    interp_run_string :: Interpreter -> String -> Interpreter
}

instance Show Interpreter where
    show interp = "Interpreter:" ++
                  " stack: " ++ show (interp_stack interp) ++
                  " is_compiling: " ++ show (interp_is_compiling interp) ++
                  " module_stack: " ++ show (interp_module_stack interp)


-- Returns the Module referred to by the top element of the Interpreter's module_stack
cur_module :: Interpreter -> Module
cur_module Interpreter{interp_module_stack=[]} = NoModule
cur_module interp@Interpreter{interp_module_stack=mod_ref:_}
    | isNothing maybe_mod = error ("Can't find module: " ++ mod_ref)
    | otherwise           = fromJust maybe_mod
    where maybe_mod = find_module interp mod_ref


-- Writes cur module back to the Interpreter's module store
update_cur_module :: Interpreter -> Module -> Interpreter
update_cur_module Interpreter{interp_module_stack=[]} mod = error "No module to replace"
update_cur_module interp mod = interp{interp_modules=modules'}
    where mod_ref:_ = interp_module_stack interp
          modules = interp_modules interp
          modules' = Data.Map.insert mod_ref mod modules


-- Pushes a StackItem onto the Intepreter's stack
stack_push :: Interpreter -> StackItem -> Interpreter
stack_push interp@Interpreter{interp_stack=s} item = interp{interp_stack=item:s}


-- Pops a StackItem from the Intepreter's stack
stack_pop :: Interpreter -> (StackItem, Interpreter)
stack_pop interp@Interpreter{interp_stack=s:ss} = (s, interp{interp_stack=ss})


-- Pops multiple Stackitems from the Interpreter's stack. The results are returned
-- in the order that they were pushed.
stack_pop_n :: Interpreter -> Int -> ([StackItem], Interpreter)
stack_pop_n interp n
    | n > stack_len = error "stack_pop_n: Stack underflow"
    | otherwise     = (reverse result, interp{interp_stack=rest})
    where stack = interp_stack interp
          stack_len = length stack
          (result, rest) = splitAt n stack

-- Returns module ref String for a given parentage list
parentage_to_module_ref :: [String] -> String
parentage_to_module_ref parentage = intercalate " " parentage

-- Returns mod reference for a Module
module_reference :: Module -> String
module_reference mod =  parentage_to_module_ref $ (mod_name mod):(mod_parentage mod)

-- Adds name of a Module required by a parent Module to that parent Module
add_module_name :: Module -> String -> Module
add_module_name mod name = mod{mod_modules=name:modules}
    where modules = mod_modules mod

-- Adds module to cur_module (and to Interpreter modules store)
add_module :: Interpreter -> Module -> Interpreter
add_module interp mod = interp'
    where modules' = Data.Map.insert mod_ref mod (interp_modules interp)
          mod_ref = module_reference mod
          cur_mod' = add_module_name (cur_module interp) (mod_name mod)
          interp' = (update_cur_module interp cur_mod'){interp_modules=modules'}

-- Updates value of Variable in a Module
update_variable :: Module -> Variable -> Module
update_variable mod var@(Variable name val)= mod{mod_vars=vars'}
    where vars = mod_vars mod
          vars' = Data.Map.insert name var vars

-- Searches Interpreter for Module given a module ref String
find_module :: Interpreter -> String -> Maybe Module
find_module interp mod_ref = Data.Map.lookup mod_ref modules
    where modules = interp_modules interp

-- Pushes a module reference onto the Interpreter's module stack
module_stack_push :: Interpreter -> String -> Interpreter
module_stack_push interp mod_ref = interp{interp_module_stack=mod_ref:stack}
        where stack = interp_module_stack interp


-- Pops a module from the Interpreter's module stack
module_stack_pop :: Interpreter -> Interpreter
module_stack_pop interp = interp{interp_module_stack=rest}
    where (mod:rest) = interp_module_stack interp
