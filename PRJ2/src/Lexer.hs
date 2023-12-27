-- Entity which takes input as a string and converts the input into a set of tokens.
{-
     Hint: define an auxiliary function lexer :: String → [String] that splits the string
    into a list of words (tokens). Example:
    lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]

    -Part 2

-}
module Lexer where

import Data.List (isPrefixOf)

-- Example of Token new data type
-- Defining a Token data tyope, can be usefull to in the future even incorporate more tokens or usefull informations about the tokens
data Token =
    -- Literals
    IntLit !Integer
    | BoolLit !Bool

    -- Identifiers
    | VarName !String

    -- Keywords
    | KWIf
    | KWWhile
    | KWElse
    | KWTrue
    | KWFalse

    -- Operators
    | OpAdd
    | OpSub
    | OpMult
    | OpLe
    | OpEq
    | OpNot
    | OpAnd
    | OpAssign

    -- Delimiters and Punctuators
    | Semicolon
    | OpenParen
    | CloseParen


    
    deriving (Eq, Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | c == ' ' || c == '\n' || c == '\t' || c == '\r' = lexer cs
    | c == ';' = Semicolon : lexer cs
    | c == '(' = OpenParen : lexer cs
    | c == ')' = CloseParen : lexer cs
    | c == '+' = OpAdd : lexer cs
    | c == '-' = OpSub : lexer cs
    | c == '*' = OpMult : lexer cs
    | c == '<' = OpLe : lexer cs
    | c == '&' = OpAnd : lexer cs
    | c == ':' && not (null cs) && head cs == '=' = OpAssign : lexer (tail cs)
    | "not" `isPrefixOf` (c:cs) = OpNot : lexer (drop 3 (c:cs))

    -- Add logic for integers, booleans, variables, etc.
    | otherwise = error "lexer: unexpected character or string out of our imperative language"

-- You need to import 'isPrefixOf' from Data.List

-- lexer = undefined -- TODO
-- --Eg: lexer "23 + 4 * 421" = ["23","+","4","*","421"]