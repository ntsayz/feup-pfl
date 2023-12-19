-- Entity which takes input as a string and converts the input into a set of tokens.
{-
     Hint: define an auxiliary function lexer :: String → [String] that splits the string
    into a list of words (tokens). Example:
    lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]

    -Part 2

-}
module Lexer where


-- Example of Token new data type
-- data Token = PlusTok | TimesTok | OpenTok | CloseTok | IntTok Integer
-- deriving (Show)

-- lexer :: String ->  [Token]
-- lexer = undefined -- TODO
-- --Eg: lexer "23 + 4 * 421" = ["23","+","4","*","421"]