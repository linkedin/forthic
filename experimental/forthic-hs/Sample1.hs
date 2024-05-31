module Sample1 (
) where

import Forthic.Types
import Forthic.Interpreter
import Modules.ModuleA
import Forthic.Module

sample_interp :: Interpreter
sample_interp = require_app_modules new_interp [module_a]

-- program = "{module_A MESSAGE }
-- run sample_interp program

prog1 = "[ 'app_1' ] VARIABLES : TACO app_1 ;  TACO"
prog2 = "[ 'app_1' ] VARIABLES  'Howdy, Everyone!' app_1 !  app_1 @"
prog3 = "{module_a MESSAGE }"
prog4 = "[ 'module_a' ] USE-MODULES  module_a.MESSAGE"
prog5 = "[ [ 'module_a' AS '' ] ] USE-MODULES  MESSAGE"
prog6 = "[ [ 'module_a' AS 'a' ] ] USE-MODULES  a.MESSAGE"
prog7 = ": APP-MESSAGE  'App says hi' ;  {module_a { APP-MESSAGE } }"
prog8 = ": APP-MESSAGE  {module_a MESSAGE } ;  APP-MESSAGE"
prog9 = ": APP-MESSAGE  {module_a ;  APP-MESSAGE"
prog10 = "{module_a"
prog11 = "[ 'app_1' ] VARIABLES  {module_a '{ app_1 @ }' INTERPRET"
prog12 = "'GREETING' '^Have a nice day^' MEMO  'Something else' GREETING!! GREETING GREETING! GREETING" 
prog13 = "'GREETING' '^Have a nice day^' MEMO  GREETING" 
prog14 = "'GREETING' '^Have a nice day^' MEMO  'Something else' GREETING!! GREETING" 
prog15 = "123 POP " 
prog16 = "123 456 SWAP" 
prog17 = "NONE" 
prog18 = "123 IDENTITY" 
prog19 = "TRUE FALSE" 
prog20 = "TRUE >STR  123 >STR 'Howdy' >STR" 
prog21 = "TRUE >INT  435.6 >INT"
prog22 = "TRUE >BOOL  435.6 >BOOL NONE >BOOL '' >BOOL"
prog23 = "NONE 51 DEFAULT  20 123 DEFAULT"
prog24 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC"
prog25 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC  'key2' REC@"
prog26 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC  'something new' 'key2' <REC!"
prog27 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC  [ [ 'key3'  33 ] [ 'key2' 44 ] ] REC MERGE"
prog28 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC  [ 'key2' ] [ 'beta' ] RELABEL"
prog29 = "[ 'a' 'b' 'c' ] [ 1 2 3 ] ZIP"
prog30 = "[ 'a' 'b' 'c' ]  1 NTH"
prog31 = "[ 'a' 'b' 'c' ]  LAST [ ] LAST"
prog32 = "[ 'a' 'b' 'c' ]  2 TAKE"
prog33 = "[ 'a' 'b' 'c' ]  2 DROP"
prog34 = "[ 'a' 'b' 'c' ]  UNPACK"
prog35 = "[ 'a' [ 'b' [ 'c' ] 'd' ] 'e' ]  FLATTEN"
prog36 = "[ 'a' 'b' 'c' ] [ 1 2 3 ] EXTEND"
prog37 = "[ 1 2 3 4 5 6 7 8 ] 3 GROUPS-OF"
prog38 = "[ 1 2 3 ] 'Howdy' APPEND"
prog39 = "'Cool message' DUP"
prog40 = "[ 1 2 3 4 5 6 7 8 ] [ 2 5 ] SLICE"
prog41 = "[ 1 2 3 4 5 6 7 8 ] [ -4 1 ] SLICE"
prog42 = "[ 'key1' 'key2' 'key1' 'key3' ] UNIQUE"
prog43 = "[ \
\  [ [ 'key1'  11 ] [ 'key2' 'alpha' ] ] REC \
\  [ [ 'key1'  11 ] [ 'key2' 'beta' ] ] REC \
\  [ [ 'key1'  33 ] [ 'key2' 'gamma' ] ] REC ]  'key1' GROUP-BY-KEY"
prog45 = "3.5 2 *"
prog46 = "[ : DOUBLE   2 * ; \
\  [ [ 'key1'  11 ] [ 'key2' 'alpha' ] ] REC \
\  [ [ 'key1'  11 ] [ 'key2' 'beta' ] ] REC \
\  [ [ 'key1'  33 ] [ 'key2' 'gamma' ] ] REC ]  ^'key1' REC@ DOUBLE^ GROUP-BY-RULE"
prog47 = "[ : DOUBLE   2 * ;  [ 1 'DUP DOUBLE' 6 REPEAT ]"
prog48 = "NONE NONE !=  1 NONE !="
prog49 = "[ 1 2 'alpha' NONE 4 ] 'NONE !=' SELECT"
prog50 = "[ 1 2 3 4 5 ] 1 '*' REDUCE"
prog51 = "EMPTY-REC"
prog52 = "[ 1 2 3 4 5 ] REVERSE"
prog53 = "[ 3 1 4 2 5 ] SORT"
prog54 = "[ 3 1 4 2 5 ] '-1 *' SORT-WITH"
prog55 = "[ 3 1 4 2 5 ] 2 REMOVE"
prog56 = "[ [ 'key1'  12 ] [ 'key2' 23 ] ] REC  'key1' REMOVE"
prog57 = "[ 1 2 3 ] KEYS  [ [ 'key1'  12 ] [ 'key2' 23 ] ] REC KEYS"
prog58 = "[ 'alpha' 'beta' 'gamma' ] VALUES  [ [ 'key1'  12 ] [ 'key2' 23 ] ] REC VALUES"
prog59 = "[ 'alpha' 'beta' 'gamma' ] LENGTH  [ [ 'key1'  12 ] [ 'key2' 23 ] ] REC LENGTH 'Howdy' LENGTH"
prog60 = "[ 'alpha' 'beta' 'gamma' ] 'LENGTH' MAP [ [ 'key1'  12 ] [ 'key2' 23 ] ] REC '3 *' MAP"
prog61 = "[ 5 5 5 ] '*' MAP-w/KEY  [ [ 'key1'  12 ] [ 'key22' 23 ] ] REC 'SWAP LENGTH *' MAP-w/KEY"
prog62 = "'Howdy, ' 'Everyone!' CONCAT  [ 'Howdy' ' ' 'all of you!' ] CONCAT"
prog63 = "^DON'T SHOUT^ >LOWER"
prog64 = "1.35 1 >FIXED"
prog65 = "'  hi    ' STRIP"
prog66 = "'This has spaces' ' ' '-' REPLACE"
prog67 = "'This-is-a-dashed-string' '-' SPLIT"
prog68 = "[ 'One' 'Two' 'Three' ] '--' JOIN"
prog69 = "2 3 +"
prog70 = "2 3 -"
prog71 = "3 2 /  3.0 2 /  3 2.0 /"
prog72 = "12 7 MOD"
prog73 = "12 NEGATE  3.5 NEGATE"
prog74 = "12 3.5 <=  4 15 >="
prog75 = "-4 0 100 CLAMP   10 0 100 CLAMP  400 0 100 CLAMP"
prog76 = "2020-05-11  10:30"
prog77 = "2020-05-11 21 +"
prog78 = "2020-05-11 2020-05-01 -   2020-05-01 20 - >STR"
prog79 = "'2020-05-11' >DATE  2020-10-21 >DATE"
prog80 = "'2020-05-11' >DATE 2020-10-21 >DATE .s"
