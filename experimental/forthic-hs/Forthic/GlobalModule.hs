-- Forthic.GlobalModule: Contains words that are used across applications. These words also
-- make use of the internals of Interpreter in ways that most Modules should not.

module Forthic.GlobalModule (
global_module        -- Returns global module
) where

import Forthic.Types
import Forthic.StackItem
import Forthic.Module
import Text.Regex
import Text.Printf
import Data.Maybe
import Data.Map
import Data.List
import Data.Set
import Data.Char
import Data.Time
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as Text
import Data.List.Split
import Debug.Trace

-- Returns global module
global_module = empty_module{mod_words=global_mod_words}

-------------------------------------------------------------------------------
-- Words

-- Defines words in the global module
global_mod_words :: [ForthicWord]
global_mod_words = [ 
    -- Base words
    ImplementedWord "VARIABLES" word_VARIABLES,
    ImplementedWord "!" word_bang,
    ImplementedWord "@" word_at,
    ImplementedWord "USE-MODULES" word_USE_MODULES,
    ImplementedWord "AS" (\interp -> interp),
    ImplementedWord "INTERPRET" word_INTERPRET,
    ImplementedWord "MEMO" word_MEMO,

    -- Stack words
    ImplementedWord "DUP" word_DUP,
    ImplementedWord "POP" word_POP,
    ImplementedWord "SWAP" word_SWAP,
    ImplementedWord "NONE" word_NONE,
    ImplementedWord "IDENTITY" word_IDENTITY,
    ImplementedWord "TRUE" word_TRUE,
    ImplementedWord "FALSE" word_FALSE,
    ImplementedWord ">STR" word_to_STR,
    ImplementedWord ">INT" word_to_INT,
    ImplementedWord ">BOOL" word_to_BOOL,
    ImplementedWord "DEFAULT" word_DEFAULT,

    -- Records
    ImplementedWord "EMPTY-REC" word_EMPTY_REC,
    ImplementedWord "REC" word_REC,
    ImplementedWord "REC@" word_REC_at,
    ImplementedWord "<REC!" word_keep_REC_bang,
    ImplementedWord "MERGE" word_MERGE,
    ImplementedWord "RELABEL" word_RELABEL,

    -- Lists
    ImplementedWord "REVERSE" word_REVERSE,
    ImplementedWord "ZIP" word_ZIP,
    ImplementedWord "NTH" word_NTH,
    ImplementedWord "LAST" word_LAST,
    ImplementedWord "TAKE" word_TAKE,
    ImplementedWord "DROP" word_DROP,
    ImplementedWord "UNPACK" word_UNPACK,
    ImplementedWord "FLATTEN" word_FLATTEN,
    ImplementedWord "EXTEND" word_EXTEND,
    ImplementedWord "GROUPS-OF" word_GROUPS_OF,
    ImplementedWord "APPEND" word_APPEND,
    ImplementedWord "SLICE" word_SLICE,
    ImplementedWord "UNIQUE" word_UNIQUE,
    ImplementedWord "GROUP-BY-KEY" word_GROUP_BY_KEY,
    ImplementedWord "GROUP-BY-RULE" word_GROUP_BY_RULE,
    ImplementedWord "SELECT" word_SELECT,
    ImplementedWord "REDUCE" word_REDUCE,

    -- Lists: Sorting
    ImplementedWord "SORT" word_SORT,
    ImplementedWord "SORT-WITH" word_SORT_WITH,

    --  Lists and Records
    ImplementedWord "REMOVE" word_REMOVE,
    ImplementedWord "KEYS" word_KEYS,
    ImplementedWord "VALUES" word_VALUES,
    ImplementedWord "LENGTH" word_LENGTH,
    ImplementedWord "MAP" word_MAP,
    ImplementedWord "MAP-w/KEY" word_MAP_w_KEY,

    -- String
    ImplementedWord "CONCAT" word_CONCAT,
    ImplementedWord ">LOWER" word_to_LOWER,
    ImplementedWord ">FIXED" word_to_FIXED,
    ImplementedWord "STRIP" word_STRIP,
    ImplementedWord "REPLACE" word_REPLACE,
    ImplementedWord "SPLIT" word_SPLIT,
    ImplementedWord "JOIN" word_JOIN,

    -- Math
    ImplementedWord "*" word_star,
    ImplementedWord "+" word_plus,
    ImplementedWord "-" word_minus,
    ImplementedWord "/" word_slash,
    ImplementedWord "MOD" word_MOD,
    ImplementedWord "NEGATE" word_NEGATE,
    ImplementedWord "CLAMP" word_CLAMP,

    -- Date
    ImplementedWord ">DATE" word_to_DATE,

    -- Special
    ImplementedWord "REPEAT" word_REPEAT,
    ImplementedWord "==" word_equal,
    ImplementedWord "!=" word_not_equal,
    ImplementedWord ">=" word_greater_equal,
    ImplementedWord "<=" word_less_equal,
    ImplementedWord ".s" word_dot_s
    ]


-- ( varnames -- )
-- This ensures the specified variables are in the current module
word_VARIABLES :: Interpreter -> Interpreter
word_VARIABLES interp = update_cur_module interp' cur_mod'
    where cur_mod = cur_module interp
          cur_mod' = ensure_variables cur_mod var_names
          (var_names, interp') = stack_pop interp


-- ( val variable -- )
-- This sets the value of a variable and updates the current module
word_bang :: Interpreter -> Interpreter
word_bang interp = interp''
    where ([val, (SVariable (Variable var_name _))], interp') = stack_pop_n interp 2
          cur_mod = cur_module interp'
          new_var = Variable var_name val
          cur_mod' = update_variable cur_mod new_var
          interp'' = update_cur_module interp' cur_mod'


-- ( var -- val )
-- This gets the value of a variable in the current module
word_at :: Interpreter -> Interpreter
word_at interp = interp''
    where (SVariable (Variable _ val), interp') = stack_pop interp
          stack' = interp_stack interp'
          interp'' = interp'{interp_stack=val:stack'}


-- ( mod_specifiers -- )
-- Each module specifier can be a registered module name like "cache" or an array of strings
-- where the first element is the module name and the second is the prefix.
--
-- This adds aliases for all exportable words from the specified module to the current module.
word_USE_MODULES :: Interpreter -> Interpreter
word_USE_MODULES interp = update_cur_module interp' cur_mod'
    where cur_mod = cur_module interp
          cur_mod' = use_modules interp cur_mod mod_specifiers
          (mod_specifiers, interp') = stack_pop interp


-- ( forthic -- ? )
-- Interprets to given forthic string
word_INTERPRET :: Interpreter -> Interpreter
word_INTERPRET interp
    | isNothing maybe_string = error ("Can't INTERPRET: " ++ show item)
    | otherwise              = run_string interp' forthic
    where run_string = interp_run_string interp
          (item, interp') = stack_pop interp
          maybe_string = as_SString item
          SString forthic = fromJust maybe_string


-- ( name forthic -- )
-- Memo-izes a Forthic string
-- This creates three words:
--    name   -- Pushes memo-ized value onto the stack, executing the Forthic if null
--    name!  -- Updates memo-ized value by running the forthic string
--    name!! -- Updates memo-ized word with a value from the stack
word_MEMO :: Interpreter -> Interpreter
word_MEMO interp = interp3'
    where ([SString name, SString forthic], interp') = stack_pop_n interp 2
          run_str = flip (interp_run_string interp)
          var_name = "_memo_var_" ++ name
          fmake_var = "[ '" ++ var_name ++ "' ] VARIABLES" 
          fname_bang = ": " ++ name ++ "!   " ++ forthic ++ " " ++ var_name ++ " ! ;"
          fname_bang_bang = ": " ++ name ++ "!!   " ++ var_name ++ " ! ;"
          interp'' = run_str fname_bang_bang . run_str fname_bang . run_str fmake_var $ interp'
          cur_mod = cur_module interp''
          cur_mod' = add_mod_word cur_mod (MemoWord name var_name)
          interp3' = update_cur_module interp'' cur_mod'


-- ( a -- a a )
-- Duplicates top of stack
word_DUP :: Interpreter -> Interpreter
word_DUP interp = flip_push a . flip_push a $ interp'
    where (a, interp') = stack_pop interp


-- ( a -- )
-- Pops value from stack
word_POP :: Interpreter -> Interpreter
word_POP interp = interp'
    where (_, interp') = stack_pop interp


-- ( a b -- b a )
-- Swaps top two elements of stack
word_SWAP :: Interpreter -> Interpreter
word_SWAP interp = flip_push a . flip_push b $ interp'
    where ([a, b], interp') = stack_pop_n interp 2


-- ( -- SNone )
word_NONE :: Interpreter -> Interpreter
word_NONE interp = stack_push interp SNone


-- ( -- )
word_IDENTITY :: Interpreter -> Interpreter
word_IDENTITY interp = interp


-- ( -- SBool True )
word_TRUE :: Interpreter -> Interpreter
word_TRUE interp = stack_push interp $ SBool True


-- ( -- SBool False )
word_FALSE :: Interpreter -> Interpreter
word_FALSE interp = stack_push interp $ SBool False

-- ( StackItem -- SString )
word_to_STR :: Interpreter -> Interpreter
word_to_STR interp = stack_push interp' res
    where (item, interp') = stack_pop interp
          item_str = show item
          bare_item = last $ splitOn " " item_str
          res = SString bare_item


-- ( StackItem -- SInt )
word_to_INT :: Interpreter -> Interpreter
word_to_INT interp = stack_push interp' res
    where (item, interp') = stack_pop interp
          res = to_INT item


-- ( StackItem -- SBool )
word_to_BOOL :: Interpreter -> Interpreter
word_to_BOOL interp
    | res_int == SInt 0  = stack_push interp' $ SBool False
    | otherwise     = stack_push interp' $ SBool True
    where (item, interp') = stack_pop interp
          res_int = to_INT item


-- ( val default_val -- value )
-- If value is SNone, then default_val; otherwise val
word_DEFAULT :: Interpreter -> Interpreter
word_DEFAULT interp
    | val == SNone  = stack_push interp' default_val
    | otherwise  = stack_push interp' val
    where ([val, default_val], interp') = stack_pop_n interp 2


-- ( -- rec )
word_EMPTY_REC :: Interpreter -> Interpreter
word_EMPTY_REC interp = stack_push interp empty_rec

-- ( [ key/vals ] -- rec )
-- Convert a list of key val pairs into a record
word_REC :: Interpreter -> Interpreter
word_REC interp = stack_push interp' res
    where (SArray key_vals, interp') = stack_pop interp
          res = add_rec_fields empty_rec key_vals


-- ( rec field -- value )
-- Gets value of a record's field
word_REC_at :: Interpreter -> Interpreter
word_REC_at interp
    | isNothing maybe_res  = stack_push interp' SNone
    | otherwise            = stack_push interp' (fromJust maybe_res)
    where ([SRecord rec, SString field], interp') = stack_pop_n interp 2
          maybe_res = Data.Map.lookup field rec

-- ( rec value field -- rec )
-- Sets value of field
word_keep_REC_bang :: Interpreter -> Interpreter
word_keep_REC_bang interp = stack_push interp' res
    where ([SRecord rec, value, SString field], interp') = stack_pop_n interp 3
          res = SRecord (Data.Map.insert field value rec)

-- ( rec1 rec2 -- rec )
-- Merges two records
word_MERGE :: Interpreter -> Interpreter
word_MERGE interp = stack_push interp' res
    where ([SRecord rec1, SRecord rec2], interp') = stack_pop_n interp 2
          res = SRecord (Data.Map.union rec2 rec1)

-- ( old_rec old_fields new_fields -- new_rec )
-- Relabels/projects record
word_RELABEL :: Interpreter -> Interpreter
word_RELABEL interp = word_REC $ stack_push interp' key_vals
    where ([SRecord rec1, SArray old_fields, SArray new_fields], interp') = stack_pop_n interp 3
          values = Data.List.map (maybe_to_val . get_val) old_fields
          maybe_to_val = \maybe -> if isNothing maybe then SNone else fromJust maybe
          get_val = \(SString key) -> Data.Map.lookup key rec1
          key_val_tuples = zip new_fields values
          key_vals = SArray $ Data.List.map (\(key, val) -> SArray [key, val]) key_val_tuples


-- ( list -- list )
word_REVERSE :: Interpreter -> Interpreter
word_REVERSE interp = stack_push interp' res
    where (SArray list, interp') = stack_pop interp
          res = SArray (reverse list)

-- ( a_list b_list -- a_b_list )
word_ZIP :: Interpreter -> Interpreter
word_ZIP interp = stack_push interp' a_b_list
    where ([SArray a_list, SArray b_list], interp') = stack_pop_n interp 2
          a_b_tuples = zip a_list b_list
          a_b_list = SArray $ Data.List.map (\(a, b) -> SArray [a, b]) a_b_tuples

-- ( list n -- nth )
word_NTH :: Interpreter -> Interpreter
word_NTH interp = stack_push interp' res
    where ([SArray items, SInt n], interp') = stack_pop_n interp 2
          out_of_range = n < 0 || n >= length items
          res = if out_of_range then SNone else items !! n

-- ( list -- item )
word_LAST :: Interpreter -> Interpreter
word_LAST interp = stack_push interp' res
    where (SArray items, interp') = stack_pop interp
          res = if items == [] then SNone else last items

-- ( list n -- rest taken )
word_TAKE :: Interpreter -> Interpreter
word_TAKE interp = flip_push res2 . flip_push res1 $ interp'
    where ([SArray items, SInt n], interp') = stack_pop_n interp 2
          res1 = SArray (drop n items)
          res2 = SArray (take n items)

-- ( list n -- items )
word_DROP :: Interpreter -> Interpreter
word_DROP interp = stack_push interp' res
    where ([SArray items, SInt n], interp') = stack_pop_n interp 2
          res = SArray (drop n items)

-- ( [a b c ...] -- a b c ... )
word_UNPACK :: Interpreter -> Interpreter
word_UNPACK interp = push_items interp' items
    where (SArray items, interp') = stack_pop interp

-- ( nested_lists -- list )
word_FLATTEN :: Interpreter -> Interpreter
word_FLATTEN interp = stack_push interp' res
    where (SArray nested, interp') = stack_pop interp
          res = SArray (reverse $ flatten nested [])

-- ( list1 list2 -- list )
word_EXTEND :: Interpreter -> Interpreter
word_EXTEND interp = stack_push interp' res
    where ([SArray list1, SArray list2], interp') = stack_pop_n interp 2
          res = SArray (list1 ++ list2)

-- ( list size -- groups )
word_GROUPS_OF :: Interpreter -> Interpreter
word_GROUPS_OF interp = stack_push interp' res
    where ([SArray list, SInt size], interp') = stack_pop_n interp 2
          res = SArray (groups_of list size [])

-- ( list item -- list )
word_APPEND :: Interpreter -> Interpreter
word_APPEND interp = stack_push interp' res
    where ([SArray list, item], interp') = stack_pop_n interp 2
          res = SArray (list ++ [item])

-- ( list indices -- list )
-- The behavior depends on the number of indices and if they are negative
--    Two indices: Values are returned from start index to end, inclusive, in specified order
--    One index positive: Values are returned from start index to end of list
--    One index negative: Values returned from index to start of list where -1 is end of list
word_SLICE :: Interpreter -> Interpreter
word_SLICE interp = stack_push interp' res
    where ([SArray list, SArray indices], interp') = stack_pop_n interp 2
          index_to_int = \(SInt int) -> int
          int_indices = Data.List.map index_to_int indices
          res = SArray (slice_list list int_indices)

-- ( list -- list )
word_UNIQUE :: Interpreter -> Interpreter
word_UNIQUE interp = stack_push interp' res
    where (SArray list, interp') = stack_pop interp
          item_set = Data.Set.fromList list
          get_elem = (flip Data.Set.elemAt) item_set
          res = SArray $ Data.List.map get_elem [0..(Data.Set.size item_set - 1)]

-- ( list key -- rec )
word_GROUP_BY_KEY :: Interpreter -> Interpreter
word_GROUP_BY_KEY interp = stack_push interp' res
    where ([SArray list, SString key], interp') = stack_pop_n interp 2
          add_rec = \accum rec -> append_group_rec (get_rec_val_str key rec) rec accum
          groups = Data.List.foldl' add_rec Data.Map.empty list
          res = SRecord (Data.Map.map reverse_SArray groups)

-- ( list forthic -- rec )
word_GROUP_BY_RULE :: Interpreter -> Interpreter
word_GROUP_BY_RULE interp = stack_push interp' res
    where ([SArray list, SString forthic], interp') = stack_pop_n interp 2
          add_rec = \accum rec -> append_group_rec (get_rec_rule_str interp' forthic rec) rec accum
          groups = Data.List.foldl' add_rec Data.Map.empty list
          res = SRecord (Data.Map.map reverse_SArray groups)


-- ( list forthic -- list )
-- The forthic string should evaluate an element of the list and return a bool
word_SELECT :: Interpreter -> Interpreter
word_SELECT interp = stack_push interp' res
    where ([SArray list, SString forthic], interp') = stack_pop_n interp 2
          res = SArray $ select_items interp' list forthic []

-- ( list accum forthic -- accum )
-- The forthic stack notation is ( item accum -- accum )
word_REDUCE :: Interpreter -> Interpreter
word_REDUCE interp = stack_push interp' res
    where ([SArray list, accum, SString forthic], interp') = stack_pop_n interp 3
          res = reduce_list interp list accum forthic


-- ( forthic n -- ? )
word_REPEAT :: Interpreter -> Interpreter
word_REPEAT interp
    | n < 0      = interp
    | otherwise  = repeat_run_string interp' forthic n
    where ([SString forthic, SInt n], interp') = stack_pop_n interp 2


-- ( a b -- Bool )
word_equal :: Interpreter -> Interpreter
word_equal interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = SBool (a == b)


-- ( a b -- Bool )
word_greater_equal :: Interpreter -> Interpreter
word_greater_equal interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = SBool (a >= b)


-- ( a b -- Bool )
word_less_equal :: Interpreter -> Interpreter
word_less_equal interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = SBool (a <= b)


-- ( -- )
word_dot_s :: Interpreter -> Interpreter
word_dot_s interp = error (show $ interp_stack interp)


-- ( a b -- Bool )
word_not_equal :: Interpreter -> Interpreter
word_not_equal interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = SBool (a /= b)


-- ( a num -- a*num )
word_star :: Interpreter -> Interpreter
word_star interp = stack_push interp' res
    where ([a, num], interp') = stack_pop_n interp 2
          res = multiply_items a num

-- ( a b -- a+b )
word_plus :: Interpreter -> Interpreter
word_plus interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = add_items a b

-- ( a b -- a+b )
word_minus :: Interpreter -> Interpreter
word_minus interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = subtract_items a b

-- ( a b -- a/b )
word_slash :: Interpreter -> Interpreter
word_slash interp = stack_push interp' res
    where ([a, b], interp') = stack_pop_n interp 2
          res = divide_items a b

-- ( a b -- a mod b )
word_MOD :: Interpreter -> Interpreter
word_MOD interp = stack_push interp' res
    where ([SInt a, SInt b], interp') = stack_pop_n interp 2
          res = SInt $ a `mod` b

-- ( a -- -a )
word_NEGATE :: Interpreter -> Interpreter
word_NEGATE interp = stack_push interp' res
    where (item, interp') = stack_pop interp
          res = negate_item item

-- ( val low high -- value )
word_CLAMP :: Interpreter -> Interpreter
word_CLAMP interp = stack_push interp' res
    where ([val, low, high], interp') = stack_pop_n interp 3
          res = if val < low then low else if val > high then high else val

-- ( string -- date )
word_to_DATE :: Interpreter -> Interpreter
word_to_DATE interp = stack_push interp' res
    where (item, interp') = stack_pop interp
          res = to_DATE item


-- ( list -- list )
word_SORT :: Interpreter -> Interpreter
word_SORT interp = stack_push interp' res
    where (SArray list, interp') = stack_pop interp
          res = SArray $ sort list

-- ( list forthic -- list )
word_SORT_WITH :: Interpreter -> Interpreter
word_SORT_WITH interp = stack_push interp' res
    where ([SArray list, SString forthic], interp') = stack_pop_n interp 2
          flip_run = flip $ interp_run_string interp
          get_val = \a -> pop_val . flip_run forthic . flip_push a $ interp'
          res = SArray $ sortOn get_val list

-- ( list index -- list )
-- ( record key -- record )
word_REMOVE :: Interpreter -> Interpreter
word_REMOVE interp = stack_push interp' res
    where ([object, key], interp') = stack_pop_n interp 2
          res = remove_key object key


-- ( list -- indexes )
-- ( record -- keys )
word_KEYS :: Interpreter -> Interpreter
word_KEYS interp = stack_push interp' res
    where (object, interp') = stack_pop interp
          res = SArray $ object_keys object


-- ( list -- list )
-- ( record -- values )
word_VALUES :: Interpreter -> Interpreter
word_VALUES interp = stack_push interp' res
    where (object, interp') = stack_pop interp
          res = SArray $ object_values object


-- ( list -- length )
-- ( record -- length )
word_LENGTH :: Interpreter -> Interpreter
word_LENGTH interp = stack_push interp' res
    where (object, interp') = stack_pop interp
          res = SInt $ object_length object


-- ( list forthic -- list )
-- ( record forthic -- record )
-- Maps forthic over each element of a list or each value of a record
word_MAP :: Interpreter -> Interpreter
word_MAP interp = stack_push interp' res
    where ([object, SString forthic], interp') = stack_pop_n interp 2
          res = map_forthic interp' object forthic


-- ( list forthic -- list )
-- ( record forthic -- record )
-- Maps forthic over each element of a list or each value of a record
-- The forthic must expect a key and a value on the stack
word_MAP_w_KEY :: Interpreter -> Interpreter
word_MAP_w_KEY interp = stack_push interp' res
    where ([object, SString forthic], interp') = stack_pop_n interp 2
          res = map_forthic_w_key interp' object forthic


-- ( str1 str2 -- str1str2 )
-- ( [ s1 s2 ... ] -- s1s2... )
word_CONCAT :: Interpreter -> Interpreter
word_CONCAT interp
    | is_SString object = concat_s1_s2 interp
    | is_SArray object = concat_string_array interp
    | otherwise = error("Can't CONCAT " ++ (show object))
    where (object, interp') = stack_pop interp


-- ( str -- str )
word_to_LOWER :: Interpreter -> Interpreter
word_to_LOWER interp = stack_push interp' res
    where (SString s, interp') = stack_pop interp
          res = SString $ [toLower c | c <- s]


-- ( num places -- str )
word_to_FIXED :: Interpreter -> Interpreter
word_to_FIXED interp = stack_push interp' res
    where ([SFloat num, SInt places], interp') = stack_pop_n interp 2
          format = "%." ++ (show places) ++ "f"
          res = SString $ printf format num


-- ( str -- str )
word_STRIP :: Interpreter -> Interpreter
word_STRIP interp = stack_push interp' res
    where (SString str, interp') = stack_pop interp
          res = SString (Text.unpack . Text.strip . Text.pack $ str)

-- ( str old new -- str )
word_REPLACE :: Interpreter -> Interpreter
word_REPLACE interp = stack_push interp' res
    where ([SString str, SString old, SString new], interp') = stack_pop_n interp 3
          res = SString $ Text.unpack (Text.replace (Text.pack old) (Text.pack new) ( Text.pack str))


-- ( str sep -- [s1 s2 ..] )
word_SPLIT :: Interpreter -> Interpreter
word_SPLIT interp = stack_push interp' res
    where ([SString str, SString sep], interp') = stack_pop_n interp 2
          strings = Data.List.map (SString . Text.unpack) (Text.splitOn (Text.pack sep) (Text.pack str))
          res = SArray strings


-- ( [s1 s2 ..] sep -- str )
word_JOIN :: Interpreter -> Interpreter
word_JOIN interp = stack_push interp' res
    where ([SArray items, SString sep], interp') = stack_pop_n interp 2
          get_string = \(SString s) -> s      
          strings = Data.List.map get_string items
          res = SString $ Data.List.intercalate sep strings


-------------------------------------------------------------------------------
-- Helpers

flip_push = flip stack_push

pop_val :: Interpreter -> StackItem
pop_val interp = res
   where (res, _ ) = stack_pop interp


concat_s1_s2 :: Interpreter -> Interpreter
concat_s1_s2 interp = stack_push interp' res
    where ([SString s1, SString s2], interp') = stack_pop_n interp 2
          res = SString (s1 ++ s2)

concat_string_array :: Interpreter -> Interpreter
concat_string_array interp = stack_push interp' res
    where (SArray items, interp') = stack_pop interp
          as_string = \(SString s) -> s
          strings = Data.List.map as_string items
          res = SString $ Data.List.foldl' (++) "" strings


map_forthic :: Interpreter -> StackItem -> String -> StackItem
map_forthic interp (SArray list) forthic = res
    where flip_run = flip $ interp_run_string interp
          compute_val = \item -> pop_val . flip_run forthic . flip_push item $ interp
          res = SArray $ Data.List.map compute_val list

map_forthic interp (SRecord rec) forthic = res
    where flip_run = flip $ interp_run_string interp
          compute_val = \item -> pop_val . flip_run forthic . flip_push item $ interp
          res = SRecord $ Data.Map.map compute_val rec

map_forthic interp object forthic = error("Can't map over " ++ (show object))


map_forthic_w_key :: Interpreter -> StackItem -> String -> StackItem
map_forthic_w_key interp (SArray list) forthic = res
    where flip_run = flip $ interp_run_string interp
          pairs = zip (Data.List.map SInt [0..]) list
          compute_val = \(i, obj) -> pop_val . flip_run forthic . flip_push obj . flip_push i $ interp
          res = SArray $ Data.List.map compute_val pairs

map_forthic_w_key interp (SRecord rec) forthic = res
    where flip_run = flip $ interp_run_string interp
          compute_val = \k obj -> pop_val . flip_run forthic .
                                      flip_push obj . flip_push (SString k) $ interp
          res = SRecord $ Data.Map.mapWithKey compute_val rec

map_forthic_w_key interp object forthic = error("Can't map over " ++ (show object))


object_length :: StackItem -> Int
object_length (SArray list) = length list
object_length (SRecord rec) = Data.Map.size rec
object_length (SString str) = length str
object_length object = error("Can't get length for " ++ (show object))

object_values :: StackItem -> [StackItem]
object_values (SArray list) = list
object_values (SRecord rec) = Data.Map.elems rec
object_values object = error("Can't get values from " ++ (show object))

object_keys :: StackItem -> [StackItem]
object_keys (SArray list) = Data.List.map SInt [0..(length list - 1)]
object_keys (SRecord rec) = Data.List.map SString (Data.Map.keys rec)
object_keys object = error("Can't get keys from " ++ (show object))


remove_at_index :: [StackItem] -> Int -> Int -> [StackItem] -> [StackItem]
remove_at_index list@(item:rest) index cur_index accum
    | index < 0 || index >= length list = accum
    | cur_index == index                = (reverse accum) ++ rest
    | otherwise                         = remove_at_index rest index (cur_index+1) (item:accum)


remove_key :: StackItem -> StackItem -> StackItem
remove_key (SArray list) (SInt index) = SArray $ remove_at_index list index 0 []
remove_key (SRecord record) (SString key) = SRecord (Data.Map.delete key record)
remove_key object key = error("Can't remove " ++ (show key) ++ " from " ++ (show object))

reduce_list :: Interpreter -> [StackItem] -> StackItem -> String -> StackItem
reduce_list interp [] accum forthic = accum
reduce_list interp (item:rest) accum forthic = reduce_list interp rest accum' forthic
    where (accum', _) = stack_pop . flip_run forthic . flip_push accum . flip_push item $ interp
          flip_run = flip $ interp_run_string interp

select_items :: Interpreter -> [StackItem] -> String -> [StackItem] -> [StackItem]
select_items interp [] fpred accum = reverse accum
select_items interp (item:rest) fpred accum
    | should_select  = select_items interp rest fpred (item:accum)
    | otherwise      = select_items interp rest fpred accum
    where (SBool should_select, _) = stack_pop . flip_run fpred . flip_push item $ interp
          flip_run = flip $ interp_run_string interp

repeat_run_string :: Interpreter -> String -> Int -> Interpreter
repeat_run_string interp forthic 0 = interp
repeat_run_string interp forthic n = repeat_run_string interp' forthic (n-1)
    where run_string = interp_run_string interp
          interp' = run_string interp forthic


negate_item :: StackItem -> StackItem
negate_item (SInt a) = SInt (-a)
negate_item (SFloat a) = SFloat (-a)
negate_item a = error ("negate not defined for " ++ show a)

divide_items :: StackItem -> StackItem -> StackItem
divide_items (SInt a) (SInt b) = SInt $ a `div` b
divide_items (SFloat a) (SFloat b) = SFloat $ a / b
divide_items (SFloat a) (SInt b) = SFloat $ a / (fromIntegral b)
divide_items (SInt a) (SFloat b) = SFloat $ (fromIntegral a) / b
divide_items a b = error ("'/' not defined for " ++ show a ++ " and " ++ show b)

subtract_items :: StackItem -> StackItem -> StackItem
subtract_items (SInt a) (SInt b) = SInt $ a - b
subtract_items (SFloat a) (SFloat b) = SFloat $ a - b
subtract_items (SFloat a) (SInt b) = SFloat $ a - (fromIntegral b)
subtract_items (SInt a) (SFloat b) = SFloat $ (fromIntegral a) - b
subtract_items (SDate a) (SDate b) = SInt $ fromIntegral (diffDays a b)
subtract_items (SDate a) (SInt b) = SDate $ addDays (fromIntegral $ negate b) a
subtract_items a b = error ("'-' not defined for " ++ show a ++ " and " ++ show b)

add_items :: StackItem -> StackItem -> StackItem
add_items (SInt a) (SInt b) = SInt $ a + b
add_items (SFloat a) (SFloat b) = SFloat $ a + b
add_items (SFloat a) (SInt b) = SFloat $ a + (fromIntegral b)
add_items (SInt a) (SFloat b) = SFloat $ (fromIntegral a) + b
add_items (SInt a) (SDate b) = SDate $ addDays (fromIntegral a) b
add_items (SDate a) (SInt b) = SDate $ addDays (fromIntegral b) a
add_items a b = error ("'+' not defined for " ++ show a ++ " and " ++ show b)

multiply_items :: StackItem -> StackItem -> StackItem
multiply_items (SInt a) (SInt b) = SInt $ a * b
multiply_items (SFloat a) (SFloat b) = SFloat $ a * b
multiply_items (SFloat a) (SInt b) = SFloat $ a * (fromIntegral b)
multiply_items (SInt a) (SFloat b) = SFloat $ (fromIntegral a) * b
multiply_items a b = error ("'*' not defined for " ++ show a ++ " and " ++ show b)

reverse_SArray :: StackItem -> StackItem
reverse_SArray (SArray list) = SArray (reverse list)

append_group_rec :: String -> StackItem -> (Map String StackItem) -> (Map String StackItem)
append_group_rec field rec accum
    | isNothing maybe_list =  Data.Map.insert field (SArray [rec]) accum
    | otherwise            =  Data.Map.insert field (SArray (rec:list)) accum
    where maybe_list = Data.Map.lookup field accum
          SArray list = fromJust maybe_list

to_key_str :: StackItem -> String
to_key_str (SString str) = str
to_key_str (SInt int) = show int
to_key_str (SFloat float) = show float
to_key_str (SBool bool) = show bool
to_key_str _ = ""


get_rec_val_str :: String -> StackItem -> String
get_rec_val_str key (SRecord rec)
    | isJust maybe_val = to_key_str $ fromJust maybe_val
    | otherwise        = ""
    where maybe_val = Data.Map.lookup key rec

-- Uses interp to evaluate forthic to get rule string
-- NOTE: The interpreter state is unaffected by this function
get_rec_rule_str :: Interpreter -> String -> StackItem -> String
get_rec_rule_str interp forthic rec = to_key_str res
    where (res, _) = stack_pop (flip_run forthic . flip_push rec $ interp)
          flip_run = flip $ interp_run_string interp

expand_slice_indices :: Int -> Int -> [Int]
expand_slice_indices start end
    | start <= end  = [start..end]
    | otherwise     = reverse [end..start]

ensure_normal_index :: Int -> Int -> Int
ensure_normal_index length index
    | index >= 0   = index
    | otherwise    = length + index

slice_list :: [a] -> [Int] -> [a]
slice_list items [start]
    | start < 0  = slice_list items [start, 0]
    | otherwise  = slice_list items [start, length items - 1]

slice_list items indices@[start, end] = res
    where [start', end'] = Data.List.map (ensure_normal_index $ length items) indices
          expanded_indices = expand_slice_indices start' end'
          get_val = \i -> items !! i
          res = Data.List.map get_val expanded_indices


groups_of :: [StackItem] -> Int -> [StackItem] -> [StackItem]
groups_of [] _ accum = reverse accum
groups_of items size accum = groups_of rest size accum'
    where group = SArray $ take size items
          rest = drop size items
          accum' = group:accum

flatten :: [StackItem] -> [StackItem] -> [StackItem]
flatten [] accum = accum
flatten ((SArray items):rest) accum = flatten rest $ (flatten items []) ++ accum
flatten (item:rest) accum = flatten rest (item:accum)

push_items :: Interpreter -> [StackItem] -> Interpreter
push_items interp [] = interp
push_items interp (item:rest) = push_items (stack_push interp item) rest

empty_rec :: StackItem
empty_rec = SRecord (Data.Map.empty)


add_rec_fields :: StackItem -> [StackItem] -> StackItem
add_rec_fields rec [] = rec
add_rec_fields (SRecord rec) (kv:rest) = add_rec_fields (SRecord rec') rest
    where rec' = Data.Map.insert key value rec
          SArray [ SString key, value ] = kv

-- Converts StackItem to SInt
to_DATE :: StackItem -> StackItem
to_DATE res@(SDate _) = res
to_DATE (SString str)
    | isJust maybe_date  = res
    | otherwise          = SNone
    where maybe_date = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day
          res = SDate $ fromJust maybe_date
to_DATE item = error ("Can't convert to date: " ++ (show item))

-- Converts StackItem to SInt
to_INT :: StackItem -> StackItem
to_INT SNone = SInt 0
to_INT (SBool True) = SInt 1
to_INT (SBool False) = SInt 0
to_INT item@(SInt _) = item
to_INT (SFloat val) = SInt (truncate val)
to_INT (SString str) = SInt (length str)
to_INT item = error ("Can't convert to int: " ++ (show item))

-- Ensures Variable is in given Module
ensure_variable :: Module -> StackItem -> Module
ensure_variable mod (SString name)
    | Data.Map.member name vars == False = mod{mod_vars=vars'}
    | otherwise = mod
    where vars = mod_vars mod
          vars' = Data.Map.insert name new_var vars
          new_var = Variable name SNone

-- Helper to ensure Variables are in a given Module
ensure_variables' :: Module -> [StackItem] -> Module
ensure_variables' mod [] = mod
ensure_variables' mod (item:rest) = ensure_variables' mod' rest
    where mod' = ensure_variable mod item

-- Ensure Varaibles are in a given Module
ensure_variables :: Module -> StackItem -> Module
ensure_variables mod (SArray names) = ensure_variables' mod names
ensure_variables mod _ = error "Expecting SArray of names"

-- Converts a ForthicWord into an ImportedWord
to_imported :: Module -> String -> ForthicWord -> ForthicWord
to_imported mod prefix word = ImportedWord imported_name mod_ref word
    where imported_name = prefix ++ (word_name word)
          mod_ref = module_reference mod

-- Import words from a Module to a parent Module
import_words :: Module -> Module -> String -> Module
import_words parent imported prefix = parent'
    where exportable = mod_exportable imported
          words = Data.List.map fromJust $ Data.List.filter isJust $ Data.List.map to_word exportable
          to_word = find_mod_word imported
          imported_words = Data.List.map (to_imported imported prefix) words
          parent' = add_mod_words parent imported_words

-- Imports a Module by name into a parent Module
import_module :: Interpreter -> Module -> StackItem -> Module
import_module interp parent (SString mod_name)
    | isNothing maybe_index    = error ("Can't find module in module: " ++ mod_name)
    | isNothing maybe_imported = error ("Can't find module in interp: " ++ mod_name)
    | otherwise                = import_words parent (fromJust maybe_imported) prefix
    where maybe_index = Data.List.elemIndex mod_name modules
          modules = mod_modules parent
          imported_ref = mod_name ++ " " ++ (module_reference parent)
          maybe_imported = find_module interp imported_ref
          prefix = mod_name ++ "."

-- Imports a Module by name and prefix into a parent Module
import_module interp parent (SArray [SString mod_name, SString prefix])
    | isNothing maybe_index    = error ("Can't find module in module: " ++ mod_name)
    | isNothing maybe_imported = error ("Can't find module in interp: " ++ mod_name)
    | length prefix == 0       = import_words parent imported prefix
    | otherwise                = import_words parent imported (prefix ++ ".")
    where maybe_index = Data.List.elemIndex mod_name modules
          modules = mod_modules parent
          imported_ref = mod_name ++ " " ++ (module_reference parent)
          maybe_imported = find_module interp imported_ref
          imported = fromJust maybe_imported

-- Handle case where cannot import module based on module spec
import_module interp mod mod_spec = error ("Unable to import module: " ++ show mod_spec)

-- Helper to USE-MODULES by different module specifications
use_modules :: Interpreter -> Module -> StackItem -> Module
use_modules interp mod (SArray []) = mod
use_modules interp mod (SArray (mod_spec:rest)) = use_modules interp mod' (SArray rest)
    where mod' = import_module interp mod mod_spec

