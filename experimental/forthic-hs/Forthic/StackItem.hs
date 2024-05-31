-- Forthic.StackItem: Defines stack items Interpreter can use
--
-- To extend the Interpreter to handle custom stack items, add value
-- constructors to the StackItem type

module Forthic.StackItem(
StackItem(..),       -- Objects that can be pushed onto the Interpreter stack
Variable(..),        -- Maintains a name and value and is stored in a Module
is_SString,
is_SArray
) where

import Data.Map
import Data.Time

-- Stores name and value of a variable
data Variable = Variable String StackItem deriving(Eq, Ord, Show)


-- Items that can be pushed onto Interpreter stack
data StackItem =
    SNone |
    SBool Bool |
    SStartArray |
    SInt Int |
    SFloat Float   |
    SString String  |
    SDate Day  |
    STime TimeOfDay  |
    SArray [StackItem] |
    SRecord (Map String StackItem) |
    SVariable Variable
    deriving(Eq, Ord, Show)

is_SString :: StackItem -> Bool
is_SString (SString _) = True
is_SString _           = False


is_SArray :: StackItem -> Bool
is_SArray (SArray _)  = True
is_SArray _           = False
