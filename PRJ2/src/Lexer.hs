-- Entity which takes input as a string and converts the input into a set of tokens.
{-
     Hint: define an auxiliary function lexer :: String → [String] that splits the string
    into a list of words (tokens). Example:
    lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]

    -Part 2

-}
module Lexer where

import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isAlphaNum, isLower, toLower, isDigit, isAlpha)
import MachineStructures
import Text.Read (readMaybe)

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
    | KWThen
    | KWElse
    | KWWhile
    | KWDo
    | KWTrue
    | KWFalse

    -- Operators
    | OpAdd
    | OpSub
    | OpMult
    | OpLe
    | OpEqInt
    | OpEqBool
    | OpNot
    | OpAnd
    | OpAssign

    -- Delimiters and Punctuators
    | Semicolon
    | OpenParen
    | CloseParen



    deriving (Eq, Show)

-- like in C++ and other languages 
isVarChar :: Char -> Bool
isVarChar c = (isAlpha c ||  isDigit c || c == '_')
    && c /= ':'  --We allow alphanumeric characters and underscores in variable names, some operators but not ;, ' ' : or
    && notElem c [' ', '\n', '\t', '\r', ';', '(', ')', '+', '-', '*', '<', '=', ':','!'] -- we don't allow spaces, new lines, tabs, etc. in variable names

lexerVarName :: String -> [Token]
lexerVarName str =
    let (varName, rest) = span isVarChar str
    in VarName varName : lexer rest

lexerIntLit :: String -> (Token, String)
lexerIntLit str =
    let (intStr, rest) = span isDigit str
    in (IntLit (read intStr), rest)

-- lexer that takes a string/source code and returns a list of above defined data of Token
-- Using operation SUB and ASSIGN we can assign negative values to variables (eg: x := 0 - 1)
lexer :: String ->  [Token]
lexer [] = []
lexer (c:cs)
    | c == ' ' || c == '\n' || c == '\t' || c == '\r' = lexer cs -- new line, tab, space, etc.  are ignored
    | c == ';' = Semicolon : lexer cs
    | c == '(' = OpenParen : lexer cs
    | c == ')' = CloseParen : lexer cs
    | c == '+' = OpAdd : lexer cs
    | c == '-' = OpSub : lexer cs
    | c == '*' = OpMult : lexer cs
    | c == '<' && not (null cs) && head cs == '=' = OpLe : lexer (tail cs)
    | "and" `isPrefixOf` (c:cs) = OpAnd : lexer (drop 3 (c:cs))
    --Important to check first for == (Integer Equality) and then for = ( Boolean equality)
    | isDigit c =
        let (intToken, rest) = lexerIntLit (c:cs)
        in intToken : lexer rest -- we assume only positive integers

    | c == '=' && not (null cs) && head cs == '=' = OpEqInt : lexer (tail cs)--TODO: check if this is the correct way to do it using head and tail
    | "True" `isPrefixOf` (c:cs) = BoolLit True : lexer (drop 4 (c:cs))
    | "False" `isPrefixOf` (c:cs) = BoolLit False : lexer (drop 5 (c:cs))
    | c == '=' = OpEqBool : lexer cs -- Equality for boolean expressions
    | c == ':' && not (null cs) && head cs == '=' = OpAssign : lexer (tail cs)
    | "not" `isPrefixOf` (c:cs) = OpNot : lexer (drop 3 (c:cs))
    | "while" `isPrefixOf` (c:cs) = KWWhile : lexer (drop 5 (c:cs))
    | "do" `isPrefixOf` (c:cs) = KWDo : lexer (drop 2 (c:cs))
    | "if" `isPrefixOf` (c:cs) = KWIf : lexer (drop 2 (c:cs))
    | "then" `isPrefixOf` (c:cs) = KWThen : lexer (drop 4 (c:cs))
    | "else" `isPrefixOf` (c:cs) = KWElse : lexer (drop 4 (c:cs))
    | isAlpha c && isLower c  = lexerVarName (c:cs) -- we check here the first letter to be lowercase

    -- Add logic for integers, booleans, variables, etc.
    | otherwise = error "lexer: unexpected character or string out of our imperative language" --TODO: use lefts and rights instead of error, only run on Assembler use error
