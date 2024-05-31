-- Forthic.Tokenizer: Converts Forthic string into list of Tokens

module Forthic.Tokenizer (
tokens               -- Converts string to list of Tokens
) where

import Forthic.Types

-- Note that parens are treated as whitespace
whitespaceChars = [' ', '\t', '\n', '\r', '(', ')']

-- Note that '^' is treated as a quote character
quoteChars      = ['"', '\'', '^']

isWhitespace c = c `elem` whitespaceChars
isNewline c    = c == '\n'
isQuote c      = c `elem` quoteChars

-- Returns next line and rest of string from a String
takeLine :: String -> (String, String)
takeLine string = (result, rest)
    where result = takeWhile (/= '\n') string
          rest = drop (length result) string

-- Returns next word from a String and the rest of the String
-- Note that words are separated by whitespace
takeWord :: String -> (String, String)
takeWord string = (result, rest)
    where result = takeWhile (not . isWhitespace) string
          rest = drop (length result) string

-- Returns next whitespace from string and rest of string
takeWhitespace :: String -> (String, String)
takeWhitespace string = (result, rest)
    where result = takeWhile (isWhitespace) string
          rest = drop (length result) string

-- True if string begins with a triple quote
isAnyTripleQuote :: String -> Bool
isAnyTripleQuote (c1:c2:c3:_) =  all (== c1) [c2, c3] && c1 `elem` quoteChars
isAnyTripleQuote (_)          =  False

-- True if string begins with a particular triple quote
isTripleQuote :: Char -> String -> Bool
isTripleQuote q (c1:c2:c3:_) =  all (== q) [c1, c2, c3] && q `elem` quoteChars
isTripleQuote q (_)          =  False

-- Accumulates a triple quote string, assuming it starts with a triple quote
accumTripleQuoteString :: Tokenizer -> Char -> (String, String) -> (String, String)
accumTripleQuoteString tokenizer q (accum, rest@(c:cs))
    | isTripleQuote q rest = (reverse accum, drop 3 rest)
    | cs == []    = error ("String missing end quote at: " ++ show tokenizer)
    | otherwise            = accumTripleQuoteString tokenizer q (c:accum, cs)


-- Accumulates a quote string, assuming it starts with a quote
accumQuoteString :: Tokenizer -> Char -> (String, String) -> (String, String)
accumQuoteString tokenizer q (accum, rest@(c:cs))
    | c == q      = (reverse accum, cs)
    | cs == []    = error ("String missing end quote at: " ++ show tokenizer)
    | otherwise   = accumQuoteString tokenizer q (c:accum, cs)


-- Returns contents of triple quoted string from a string and rest of string
takeTripleQuoteString :: Tokenizer -> String -> (String, String)
takeTripleQuoteString tokenizer string =
    accumTripleQuoteString tokenizer (string !! 0) ("", drop 3 string)


-- Returns contents of quoted string from a string and rest of string
takeQuoteString :: Tokenizer -> String -> (String, String)
takeQuoteString tokenizer string@(q:rest) = accumQuoteString tokenizer q ("", rest)

-- Matches CommentToken
gatherComment :: Tokenizer -> (Token, Tokenizer)
gatherComment tokenizer@Tokenizer{tokzr_input=ch:string, tokzr_line=l, tokzr_col=c}
    | ch == '#'  = (Token {tok_type=CommentToken, tok_text=comment, tok_line=l, tok_col=c},
                    Tokenizer {tokzr_input=rest, tokzr_line=l, tokzr_col=c'}) 
    | otherwise   = error ("Comment didn't start with '#' at: " ++ show tokenizer)
    where (comment, rest) = takeLine string
          c' = 1 + c + length comment

-- Matches a single character token
getSingleCharToken :: TokenType -> Tokenizer -> (Token, Tokenizer)
getSingleCharToken tokType Tokenizer{tokzr_input=ch:cs, tokzr_line=l, tokzr_col=c} =
    (Token{tok_type=tokType, tok_text=[ch], tok_line=l, tok_col=c'},
     Tokenizer{tokzr_input=cs, tokzr_line=l, tokzr_col=c'})
    where c' = c + 1

-- Matches WordToken
gatherWord :: Tokenizer -> (Token, Tokenizer)
gatherWord Tokenizer{tokzr_input=string, tokzr_line=l, tokzr_col=c} =
    (Token {tok_type=WordToken, tok_text=word, tok_line=l, tok_col=c'},
     Tokenizer {tokzr_input=rest, tokzr_line=l, tokzr_col=c'}) 
    where (word, rest) = takeWord string
          c' = c + length word

-- Matches StartModuleToken
gatherModule :: Tokenizer -> (Token, Tokenizer)
gatherModule Tokenizer{tokzr_input=_:string, tokzr_line=l, tokzr_col=c} =
    (Token {tok_type=StartModuleToken, tok_text=word, tok_line=l, tok_col=c},
     Tokenizer {tokzr_input=rest, tokzr_line=l, tokzr_col=c'}) 
    where (word, rest) = takeWord string
          c' = c + length word + 1

-- Matches StartDefToken
gatherStartDef :: Tokenizer -> (Token, Tokenizer)
gatherStartDef tokenizer@Tokenizer{tokzr_input=ch:string, tokzr_line=l, tokzr_col=c}
    | ch == ':'  =  (Token{tok_type=StartDefToken, tok_text=word, tok_line=l, tok_col=c},
                     Tokenizer{tokzr_input=rest, tokzr_line=l, tokzr_col=c'})
    | otherwise   = error ("Definition didn't start with ':' at: " ++ show tokenizer)
    where (wspace, string') = takeWhitespace string
          (word, rest) = takeWord string'
          c' = c + length wspace + length word


-- Matches triple-quoted StringToken
gatherTripleQuoteString :: Tokenizer -> (Token, Tokenizer)
gatherTripleQuoteString tokenizer@Tokenizer{tokzr_input=string, tokzr_line=l, tokzr_col=c} =
    (Token {tok_type=StringToken, tok_text=tripleQuoteString, tok_line=l, tok_col=c},
     Tokenizer {tokzr_input=rest, tokzr_line=l', tokzr_col=c'}) 
    where (tripleQuoteString, rest) = takeTripleQuoteString tokenizer string
          ls = lines tripleQuoteString
          l' = l + length ls - 1
          c' = 3 + (length $ last ls)   -- TODO: Fix this bug

-- Matches StringToken
gatherString :: Tokenizer -> (Token, Tokenizer)
gatherString tokenizer@Tokenizer{tokzr_input=string, tokzr_line=l, tokzr_col=c} =
    (Token {tok_type=StringToken, tok_text=quoteString, tok_line=l, tok_col=c},
     Tokenizer {tokzr_input=rest, tokzr_line=l', tokzr_col=c'}) 
    where (quoteString, rest) = takeQuoteString tokenizer string
          ls = lines quoteString
          l' = l + length ls - 1
          c' = 1 + (length $ last ls)

eosToken :: Tokenizer -> (Token, Tokenizer)
eosToken tokenizer = (Token {tok_type=EOSToken, tok_text="", tok_line=l, tok_col=c}, tokenizer)
    where l = tokzr_line tokenizer
          c = tokzr_col tokenizer

-- Returns next token and tokenizer state
nextToken :: Tokenizer -> (Token, Tokenizer)
nextToken (tokenizer@Tokenizer{tokzr_input=s@(ch:cs), tokzr_line=l, tokzr_col=c})
    | isNewline ch       = nextToken Tokenizer{tokzr_input=cs, tokzr_line=l+1, tokzr_col=1}
    | isWhitespace ch    = nextToken Tokenizer{tokzr_input=cs, tokzr_line=l, tokzr_col=c+1}
    | ch == '#'          = gatherComment tokenizer
    | ch == ':'          = gatherStartDef tokenizer
    | ch == ';'          = getSingleCharToken EndDefToken tokenizer
    | ch == '['          = getSingleCharToken StartArrayToken tokenizer
    | ch == ']'          = getSingleCharToken EndArrayToken tokenizer
    | ch == '{'          = gatherModule tokenizer
    | ch == '}'          = getSingleCharToken EndModuleToken tokenizer
    | isAnyTripleQuote s = gatherTripleQuoteString tokenizer
    | isQuote ch         = gatherString tokenizer
    | otherwise          = gatherWord tokenizer
nextToken (tokenizer@Tokenizer{tokzr_input=[]}) = eosToken tokenizer

-- Helper function to return all tokens in a Tokenizer
tokens' :: Tokenizer -> [Token]
tokens' Tokenizer{tokzr_input=""} = []
tokens' tokenizer = tokenMatch : tokens' newTokenizer
    where (tokenMatch, newTokenizer) = nextToken tokenizer

-- Returns all tokens from a string
tokens :: String -> [Token]
tokens string = tokens' $ Tokenizer{tokzr_input=string, tokzr_line=1, tokzr_col=1}
