{- 
   - Part 2


    Definition of our parser: 

    Entity

    which takes the tokens from the lexer and returns a syntax tree based on a grammar. 
    The grammar is often expressed in a meta language such as Backus-Naur Form (BNF) ( or others). 
    The grammar is the language of languages and provides the rules and syntax.

  
  
 -}


 {-
 
     Define a parser which transforms an imperative program represented as a string
into its corresponding representation in the Stm data (a list of statements Stm).
  The parser with the help oft the lexer, will take a list of tokens,
  and return a list of statements (Stm) that represents the program.
  Using pattern matching and recursion to process the list of tokens,
  builds an abstract syntax tree (AST) that represents the program.
  
  This AST is the output of the parser that will be used by the compiler.
  
  The compiler (compile) will take the AST and generate the machine code.
  
  The run (assembler/interpreter) will take the machine code and execute it.
 
 
 -}
module Parser where
import ImperativeLanguage

-- Defining a Token data tyope, can be usefull to in the future even incorporate more tokens or usefull informations about the tokens
data Token =
    -- Literals
    IntLit Integer
    | BoolLit Bool

    -- Identifiers
    | VarName String

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

    -- Add other tokens as necessary for your language
    deriving (Eq, Show)


--Parser will Define Abstract Syntax Trees, Concrete Syntax Tree or other kind of tree to use on the arithmetical and boolean expressions and other things
--  type Program = [Stm]

-- Parsing logic for assignments
-- Example: parseAssign [VarName "x", OpAssign, IntLit 5, Semicolon] 
-- returns (ASSIGN "x" (INTVAL 5), [])
parseAssign :: [Token] -> (Stm, [Token])
parseAssign (VarName var : OpAssign : restOfTokens) = 
   let (aexp, restOfTokens') = parseAexp restOfTokens 
   in (ASSIGN var aexp, restOfTokens')
parseAssign _ = error "Invalid assignment syntax" -- TODO: improve error handling, only run interprete/assembler handle error as Run-time error


{-
   Need to consider operator precedence and associativity in arithmetic expressions
   Using recursive descent parsing
   TODO: explain how it works on the report

-}

parseTerm :: [Token] -> (Aexp, [Token])
parseTerm tokens = 
    case tokens of
        (IntLit x : restOfTokens) -> (INTVAL x, restOfTokens)
        (VarName x : restOfTokens) -> (VAR x, restOfTokens)
        (OpenParen : restOfTokens) -> 
            let (aexp, restOfTokens') = parseAexp restOfTokens
            in case restOfTokens' of
                (CloseParen : restOfTokens'') -> (aexp, restOfTokens'')
                _ -> error "Missing closing parenthesis" -- TODO:Use Left, right -> improve error handling, only run interprete/assembler handle error as Run-time error, others returns string like  on exec of the Assembler
        _ -> error "Invalid term syntax on Aexp"

-- Parses multiplication expressions
parseProduct :: [Token] -> (Aexp, [Token])
parseProduct tokens =
    let (term1, rest) = parseTerm tokens in
    case rest of
        (OpMult : rest') ->
            let (term2, rest'') = parseProduct rest' in
            (MULT term1 term2, rest'')
        _ -> (term1, rest)


-- Parses addition and subtraction expressions
parseSum :: [Token] -> (Aexp, [Token])
parseSum tokens =
    let (term1, rest) = parseProduct tokens in
    case rest of
        (OpAdd : rest') ->
            let (term2, rest'') = parseSum rest' in
            (ADD term1 term2, rest'')
        (OpSub : rest') ->
            let (term2, rest'') = parseSum rest' in
            (SUB term1 term2, rest'')
        _ -> (term1, rest)



-- Parsing logic for arithmetic expressions
-- Example: parseAexp [IntLit 5, OpAdd, IntLit 3, OpMult, IntLit 2]
-- returns (MULT (ADD (INTVAL 5) (INTVAL 3)) (INTVAL 2), [])
parseAexp :: [Token] -> (Aexp, [Token])
parseAexp tokens = parseSum


-- Parsing logic for boolean expressions
-- Example: parseBexp [KWTrue, OpAnd, KWFalse, OpNot, OpAnd, KWTrue]
   -- returns (AND TRU (NEG (AND FALS TRU)), [])
parseBexp :: [Token] -> (Bexp, [Token])
parseBexp tokens = 



-- Check for parse statement cases using tokens@, alias for tokens to catch patterns of statements
parseStm :: [Token] -> (Stm, [Token])
parseStm tokens@(VarName _ : OpAssign : _) = parseAssign tokens
-- Add other statement patterns here, such as IF, WHILE, etc.
-- parseStm tokens@(KWIf : _) = parseIf tokens
-- parseStm tokens@(KWWhile : _) = parseWhile tokens
parseStm _ = error "Unknown statement syntax" -- TODO: improve error handling, only run interprete/assembler handle error as Run-time error



buildData :: [Token] -> Program
buildData tokens = 
    case tokens of
      let (stm, rest) = parseStm tokens 
      in stm : buildData rest -- list of statements constructed recursively
   

parse :: String -> Program

-- parse = undefined -- TODO
-- --Eg: parse = buildData . lexer
-- -- 