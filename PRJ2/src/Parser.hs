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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser where
import ImperativeLanguage

import Lexer
import Data.List (isPrefixOf, isInfixOf)
import Data.Char

--Parser will Define Abstract Syntax Trees, Concrete Syntax Tree or other kind of tree to use on the arithmetical and boolean expressions and other things
-- We construct our AST (Abstract Syntax Tree) that represents a given CFG ( context free grammar for our language)
-- The way the parser constructs AST's ditate the precedence and associativity of the operators/statements (arithmetical, boolean and use of parenthesis)
--  type Program = [Stm]

{-
   Need to consider operator precedence and associativity in arithmetic expressions and boolean expressions
   Using recursive descent parsing: we break down the parsing process into hierarchy of functions, 
   where they have a level of precedence between them.
   TODO: explain how it works on the report
   TODO: check project instructions for more details(
   TODO: check how negative numbers are handled on the report x + -1 is just x - 1

-}
-- Parser to deal with terms (e.g. 5, x, (5 + 3))
parseTerm :: [Token] -> (CompExpr, [Token])
parseTerm tokens =
    case tokens of
        (IntLit x : restOfTokens) -> (AEXPR (INTVAL x), restOfTokens) -- TODO: Improve this
        (VarName x : restOfTokens) -> (AEXPR (VAR x), restOfTokens)
        (OpenParen : restOfTokens) ->
            -- Determine if the expression inside the parentheses is Aexp or Bexp
            case nextExpressionType restOfTokens of
                AexpType -> 
                    let (aexp, restOfTokens') = parseAexp restOfTokens
                    in case restOfTokens' of
                        (CloseParen : restOfTokens'') -> (AEXPR aexp, restOfTokens'')
                        _ -> error "Missing closing parenthesis for Aexp"
                BexpType -> 
                    let (bexp, restOfTokens') = parseBexp restOfTokens
                    in case restOfTokens' of
                        (CloseParen : restOfTokens'') -> (BEXPR bexp, restOfTokens'')
                        _ -> error "Missing closing parenthesis for Bexp"
        _ -> error "Invalid term syntax on CompExpr"

--TODO: finish this and do this for other cases or statements cases
data ExprType = AexpType | BexpType
-- Helper function to determine the type of expression (Aexp or Bexp)
nextExpressionType :: [Token] -> ExprType
nextExpressionType (OpNot : _) = BexpType
nextExpressionType (OpenParen: OpNot : _) = BexpType 
nextExpressionType (BoolLit _ : _) = BexpType
nextExpressionType (_ : OpLe : _) = AexpType
nextExpressionType (_ : OpEqInt : _) = BexpType
nextExpressionType (VarName _ : OpAssign : BoolLit _ : _) = BexpType
    
nextExpressionType (IntLit _ : _) = AexpType
nextExpressionType ( _: OpEqBool : _ : _) = BexpType
nextExpressionType _ = AexpType -- Default to Aexp for other cases



-- Parses multiplication expressions
-- Parses multiplication expressions
parseProduct :: [Token] -> (CompExpr, [Token])
parseProduct tokens =
    let (term1, rest) = parseTerm tokens in
    case rest of
        (OpMult : rest') ->
            let (term2, rest'') = parseProduct rest' in
            case (term1, term2) of
                (AEXPR a1, AEXPR a2) -> (AEXPR (MULT a1 a2), rest'')
                _ -> error "Type mismatch in multiplication expression"
        _ -> (term1, rest) -- If we don't have a multiplication expression we return the term and the rest of the tokens


-- Parses addition and subtraction expressions
parseSum :: [Token] -> (Aexp, [Token])
parseSum tokens =
   -- here we call parseProduct to parse the multiplication expression, and calling before parseSum we ensure precedence
    let (term1, rest) = parseProduct tokens in
    case rest of
        (OpAdd : rest') ->
            let (term2, rest'') = parseSum rest' in
            (ADD term1 term2, rest'')
        (OpSub : rest') ->
            let (term2, rest'') = parseSum rest' in
            (SUB term1 term2, rest'')
        _noAddOrSub -> (term1, rest)



-- Parsing logic for arithmetic expressions
-- Example: parseAexp [IntLit 5, OpAdd, IntLit 3, OpMult, IntLit 2]
-- returns (MULT (ADD (INTVAL 5) (INTVAL 3)) (INTVAL 2), [])
parseAexp :: [Token] -> (Aexp, [Token])
parseAexp tokens = parseSum tokens

-- Auxiliary functions for parsing boolean expressions
-- We take into account operator precedence and associativity
parseAnd :: [Token] -> (Bexp, [Token])
parseAnd tokens =
    let (expr1, rest1) = parseEquality tokens
    in case rest1 of
        (OpAnd : rest2) -> let (expr2, rest3) = parseAnd rest2
                           in (AND expr1 expr2, rest3)
        _noAnd -> (expr1, rest1)

parseEquality :: [Token] -> (Bexp, [Token]) -- TODO: improve this
parseEquality tokens =
    let (expr1, rest1) = parseNegation tokens
    in case rest1 of
        (OpEqBool : rest2) -> let (expr2, rest3) = parseEquality rest2
                          in (EQU (BEXPR expr1) (BEXPR expr2), rest3)
        _noEquality -> (expr1, rest1)

parseNegation :: [Token] -> (Bexp, [Token])
parseNegation (OpNot : OpenParen : tokens) =
    let (expr, rest) = parseBexp tokens
    in case rest of
        CloseParen : rest' -> (NEG expr, rest')
        _noCloseParen -> error "Syntax error: missing closing parenthesis in negation"
parseNegation tokens = parseRelation tokens


parseRelation :: [Token] -> (Bexp, [Token])
parseRelation tokens =
    let (expr1, rest1) = parseAexp tokens
    in case rest1 of
        (OpLe : rest2) -> let (expr2, rest3) = parseAexp rest2
                          in (LE expr1 expr2, rest3)
        _ -> error "Error on: relational operator with boolean terms" -- TODO: improve error handling, only run interprete/assembler handle error as Run-time error
-- Parsing logic for boolean expressions
-- Example: parseBexp [KWTrue, OpAnd, KWFalse, OpNot, OpAnd, KWTrue]
   -- returns (AND TRU (NEG (AND FALS TRU)), [])
parseBexp :: [Token] -> (Bexp, [Token])
parseBexp tokens = parseAnd tokens  -- We need to Start with the lowest precedence


parseOptionalParensBexp :: [Token] -> (Bexp, [Token])
parseOptionalParensBexp (OpenParen : tokens) = 
    let (expr, rest) = parseBexp tokens
    in case rest of
        CloseParen : rest' -> (expr, rest')
        _noCloseParen -> error "Syntax error: missing closing parenthesis in bexp"
parseOptionalParensBexp tokens = parseBexp tokens

parseOptionalParensStm :: [Token] -> (Stm, [Token])
parseOptionalParensStm (OpenParen : tokens) = 
    let (stm, rest) = parseStm tokens
    in case rest of
        CloseParen : Semicolon : rest' -> (stm, rest')
        _noCloseParen -> error "Syntax error: missing closing parenthesis in stm"
parseOptionalParensStm tokens = parseStm tokens



-- Funtions to check if the variable name contains any reserved keywords (case-insensitive)
reservedKeywords :: [String]
reservedKeywords = ["else", "then", "if", "while", "not", "do", "and"]

-- Check if the variable name contains any reserved keywords (case-insensitive)
containsReservedKeyword :: String -> Bool
containsReservedKeyword varName =
    let varNameLower = map toLower varName
    in any (`isInfixOf` varNameLower) (map toLower <$> reservedKeywords)


-- Parses either an arithmetic or a boolean expression
parseCompExpr :: [Token] -> (CompExpr, [Token])
parseCompExpr tokens =
    let (aexpResult, aexpRest) = parseAexp tokens
    in if not (null aexpRest) && aexpRest /= tokens
       then (AEXPR aexpResult, aexpRest)
       else let (bexpResult, bexpRest) = parseBexp tokens
            in (BEXPR bexpResult, bexpRest)
-- Parsing logic for assignments
parseAssign :: [Token] -> (Stm, [Token])
parseAssign (VarName var : OpAssign : restOfTokens)
    | containsReservedKeyword var = error "Syntax error: variable name contains reserved keyword"
    | otherwise =
        let (compExpr, rest) = parseCompExpr restOfTokens
        in case rest of
            (Semicolon : rest') -> (ASSIGN var compExpr, rest')
            _noSemicolon -> error "Syntax error: missing semicolon after assignment"
parseAssign _ = error "Syntax error: parseAssign called for wrong syntax/Tokens" -- TODO: improve error handling, only run interprete/assembler handle error as Run-time error




-- Parsing logic for if statements
parseIf :: [Token] -> (Stm, [Token])
parseIf tokens =
    case tokens of
        KWIf : restAfterIf ->
            let (bexpCondition, restAfterBexp) = parseOptionalParensBexp restAfterIf
                (thenStm, tokensAfterThenStm) = parseOptionalParensStm $ tail (dropWhile (/= KWThen) restAfterBexp)
                (elseStm, restAfterElseStm) = parseOptionalParensStm $ tail (dropWhile (/= KWElse) tokensAfterThenStm)
            in (IF bexpCondition thenStm elseStm, restAfterElseStm)
        _ -> error "Syntax error: expected 'if' at the beginning"

parseWhile :: [Token] -> (Stm, [Token])
parseWhile tokens =
    case tokens of
        KWWhile : OpenParen : restAfterWhile ->
            let (bexpCondition, restAfterBexp) = parseBexp restAfterWhile
            in case restAfterBexp of
                CloseParen : KWDo : restAfterDo ->
                    let (doStm, restAfterDoStm) = parseOptionalParensStm restAfterDo
                    in (WHILE bexpCondition doStm, restAfterDoStm)
                _ -> error "Syntax error: missing closing parenthesis or 'do' keyword"
        _ -> error "Syntax error: expected 'while' at the beginning"

-- Function to parse a single statement (ASSIGN, IF, WHILE, etc.)
parseSingleStm :: [Token] -> (Stm, [Token])
parseSingleStm tokens =
    case tokens of
        OpenParen : rest -> 
            -- Parsing a sequence of statements inside parentheses
            let (stmList, rest') = parseStmList rest []
            in case rest' of
                CloseParen : rest'' -> (SEQ stmList, rest'')
                _noCloseParen -> error "Syntax error: missing closing parenthesis in sequence"
        tokens@(VarName _ : OpAssign : _) -> parseAssign tokens
        tokens@(KWIf : _) -> parseIf tokens
        tokens@(KWWhile : _) -> parseWhile tokens
        _invalidSingleStm -> error "Syntax error: invalid single statement"


-- Function to parse a sequence of statements (SEQ) or a single statement
-- we only deal with SEQ = (stm1 ; stm2), where cant have nested SEQ ( inside parenthesis)
--TODO: nested SEQ ( inside parenthesis) is not a good syntax, the proper way would be: SEQN = (stm1 ; stm2 ; ... ; stmn)
-- or using SEQ (!Stm, !Stm) we would have : code syntax like : (stm1; ((stm2; stm3;))) ... nested SEQ ...
-- Function to parse a sequence of statements
parseStm :: [Token] -> (Stm, [Token])
parseStm tokens =
    case tokens of
        OpenParen : rest ->
            -- Parsing a sequence of statements inside parentheses
            let (stmList, rest') = parseStmList rest []
            in case rest' of
                CloseParen : rest'' -> (SEQ stmList, rest'')
                _ -> error "Syntax error: missing closing parenthesis in sequence"
        _seqOfStmsWithoutParen ->
            -- Parsing a sequence of statements without parentheses
            let (stmList, rest') = parseStmList tokens []
            in (SEQ stmList, rest')
        


-- Helper function to parse a list of statements
parseStmList :: [Token] -> [Stm] -> ([Stm], [Token])
parseStmList tokens acc =
    case tokens of
        CloseParen : rest -> (acc, tokens)
        Semicolon : rest ->
            let (stm, rest') = parseSingleStm rest
            in parseStmList rest' (acc ++ [stm])
        _singleStm -> 
            let (stm, rest') = parseSingleStm tokens
            in if null rest' then (acc ++ [stm], rest') else parseStmList rest' (acc ++ [stm])


--TODO: use list of statements to deal with sequence of statements on SEQ?
-- TODO: check cases of: if can have or not () on condition and then, while always have () else can have or not, while always have () on condition and on body after do

buildData :: [Token] -> Program
buildData tokens =
        let (stm, restOfTokens) = parseStm tokens
        in stm : buildData restOfTokens -- list of statements constructed recursively


parse :: String -> Program
parse = buildData . lexer -- parse will be the composition of builData and lexer

