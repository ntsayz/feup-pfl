{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser where
import ImperativeLanguage

import Lexer
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Debug.Trace (trace)
import Control.Applicative ((<|>))




parseTerm :: [Token] -> Maybe (Aexp, [Token])
parseTerm (OpenParen : restTokensAfterOpenParen) =
    case parseAexp restTokensAfterOpenParen of
        Just (expr, CloseParen : restAfterCloseParen) -> Just (expr, restAfterCloseParen)
        _noAexp -> Nothing  -- Handle error, incomplete parse, or missing CloseParen
parseTerm (IntLit n : restAfterIntList) = Just (INTVAL n, restAfterIntList)
parseTerm (VarName v : restAfterVarName) = Just (VAR v, restAfterVarName)
parseTerm _ = Nothing  -- Handle error for unexpected token




parseProduct :: [Token] -> Maybe (Aexp, [Token])
parseProduct tokens =
    case parseTerm tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseProduct' leftTerm restAfterLeftTerm
        Nothing -> Nothing

parseProduct' :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseProduct' leftTerm (OpMult : tokens) =
    case parseTerm tokens of
        Just (rightTerm, restAfterRightTerm) -> parseProduct' (MULT leftTerm rightTerm) restAfterRightTerm
        Nothing -> Nothing
parseProduct' left tokens = Just (left, tokens)



parseSum :: [Token] -> Maybe (Aexp, [Token])
parseSum tokens =
    case parseProduct tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseSum' leftTerm restAfterLeftTerm
        Nothing -> Nothing

parseSum' :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseSum' leftTerm (OpAdd : tokens) =
    case parseProduct tokens of
        Just (rightTerm, restAfterRightTerm) -> parseSum' (ADD leftTerm rightTerm) restAfterRightTerm
        Nothing -> Nothing
parseSum' leftTerm (OpSub : tokens) =
    case parseProduct tokens of
        Just (rightTerm, restAfterRightTerm) -> parseSum' (SUB leftTerm rightTerm) restAfterRightTerm
        Nothing -> Nothing
parseSum' left tokens = Just (left, tokens)



parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
    case parseSum tokens of
        Just (aexp, []) -> Just (aexp, [])
        Just (aexp, restAfterAexp) -> Just (aexp, restAfterAexp)
        _incompleteParsing -> Nothing  -- Handle incomplete parse or error



-- parsers for Bexp expressions for our Imperative Language


parseBoolLiteral :: [Token] -> Maybe (Bexp, [Token])
parseBoolLiteral (BoolLit True : restTokens) = Just (TRU, restTokens)
parseBoolLiteral (BoolLit False : restTokens) = Just (FALS, restTokens)
parseBoolLiteral _ = Nothing  -- This case handles when the input token list does not start with a boolean literal


parseBoolVar :: [Token] -> Maybe (Bexp, [Token])
parseBoolVar (VarName name : restTokens) = Just (AEXPRBOOL (VAR name), restTokens)
parseBoolVar _ = Nothing  -- Handle the case where the token is not a variable name

parseBexpTerm :: [Token] -> Maybe (Bexp, [Token])
parseBexpTerm (OpenParen : restTokensAfterOpenParen) =
    case parseBexp restTokensAfterOpenParen of
        Just (bexp, CloseParen : restAfterBexp) -> Just (bexp, restAfterBexp)
        _noBexp -> Nothing

parseBexpTerm tokens =
    case parseAexp tokens of
        Just (aexp, restAfterAexp) -> Just (AEXPRBOOL aexp, restAfterAexp)
        Nothing -> parseSimpleBexp tokens
-- This is for case we dont have parentheses and we need to check for the other cases respecting the precedence and not just stop on BoolLiteral or VarBool eg: True = False


parseSimpleBexp :: [Token] -> Maybe (Bexp, [Token])
parseSimpleBexp tokens =
    parseBoolLiteral tokens <|>
    parseBoolVar tokens





parseLe:: [Token] -> Maybe (Bexp, [Token])
parseLe tokens =
    case parseBexpTerm tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseLe' leftTerm restAfterLeftTerm
        Nothing -> Nothing

parseLe' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseLe' leftTerm (OpLe : tokens) =
    case parseBexpTerm tokens of
        Just (rightTerm, restAfterRightTerm) ->
            case (leftTerm, rightTerm) of
                (AEXPRBOOL leftAexp, AEXPRBOOL rightAexp) ->
                    parseLe' (LE leftAexp rightAexp) restAfterRightTerm
                noAEXPRBOOL_ -> Nothing
        Nothing -> Nothing
parseLe' left tokens = Just (left, tokens)

parseEquality :: [Token] -> Maybe (Bexp, [Token])
parseEquality tokens =
    case parseLe tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseEquality' leftTerm restAfterLeftTerm
        Nothing -> Nothing

parseEquality' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseEquality' leftTerm (OpEqInt : tokens) =
    case parseLe tokens of
        Just (rightTerm, restAfterRightTerm) ->
            case (leftTerm, rightTerm) of
                (AEXPRBOOL leftAexp, AEXPRBOOL rightAexp) ->
                    parseEquality' (EQUINT leftAexp rightAexp) restAfterRightTerm
                noAEXPRBOOL_ -> Nothing
        Nothing -> Nothing
parseEquality' left tokens = Just (left, tokens)

--TODO: explain this to professor !!! we can have for not not not x = True -> not (not (not (x) ) )  = True)))
-- TODO: check this : left to right associativity (from project instructions), normally not is right to left

parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (OpNot : restTokens) =
    case parseNot restTokens of
        Just (bexp, remainingTokens) -> Just (NEG bexp, remainingTokens)
        Nothing -> parseEquality restTokens  -- Proceed to the next level of parsing
parseNot tokens = parseEquality tokens  -- Continue to call parseEquality for non-'not' cases


parseEqBool :: [Token] -> Maybe (Bexp, [Token])
parseEqBool tokens =
    case parseNot tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseEqBool' leftTerm restAfterLeftTerm
        Nothing -> Nothing

parseEqBool' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseEqBool' leftTerm (OpEqBool : tokens) =
    case parseNot tokens of
        Just (rightTerm, restAfterRightTerm) -> Just (EQUBOOL leftTerm rightTerm, restAfterRightTerm)
        Nothing -> Nothing
parseEqBool' left tokens = Just (left, tokens)

parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
    case parseEqBool tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseAnd' leftTerm restAfterLeftTerm
        Nothing -> Nothing
parseAnd' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseAnd' leftTerm (OpAnd : tokens) =
    case parseEqBool tokens of
        Just (rightTerm, restAfterRightTerm) -> Just (AND leftTerm rightTerm, restAfterRightTerm)
        Nothing -> Nothing
parseAnd' left tokens = Just (left, tokens)


-- Function that is the entry point for parsing Bexp expressions
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens =
   case parseAnd tokens of
        Just (bexp, []) -> Just (bexp, [])
        Just (bexp, restAfterBexp) -> Just (bexp, restAfterBexp)
        _incompleteParsing -> Nothing  -- Handle incomplete parse or error



-- parsers for Stm expressions for our Imperative Language



-- parser for assigments (eg: x := 42; or x := True; etc)
parseAssignment :: [Token] -> Maybe (Stm, [Token])
parseAssignment (VarName var : OpAssign : tokens) =
    case parseAexp tokens of -- if parseAexp dont parse nothing, or just parse a part of the tokens, parseBexp will be called
        Just (aexp, Semicolon : restTokens) -> Just (ASSIGNINT var aexp, restTokens)
        _noSemicolon ->
            case parseBexp tokens of
                Just (bexp, Semicolon : restTokens) -> Just (ASSIGNBOOL var bexp, restTokens)
                _noBexp -> Nothing
parseAssignment _ = Nothing

-- parser for Sequence of assigments ( more then one inside parenthesis)
parseSeq :: [Token] -> Maybe (Stm, [Token])
parseSeq (OpenParen : tokens) = parseSeq' [] tokens
parseSeq _ = Nothing

parseSeq' :: [Stm] -> [Token] -> Maybe (Stm, [Token])
parseSeq' acc (CloseParen : Semicolon : restTokens) =  Just (SEQ (reverse acc), restTokens)
parseSeq' acc (CloseParen : KWElse: restTokens) = Just (SEQ (reverse acc), KWElse: restTokens) -- for ) with no ; after then
parseSeq' acc tokens = 
    case parseStm tokens of
        Just (stm, restAfterStm) -> parseSeq' (stm : acc) restAfterStm
        _ -> Nothing

-- for then statements,SEQ needs to no evaluate Semicolon after CloseParen


parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf (KWIf : tokens) = do
    (condExpr, restAfterCond) <- parseBexp tokens
    case restAfterCond of
        KWThen : restAfterThen -> do
            (thenStm, restAfterThenStm) <- parseStm restAfterThen
            case restAfterThenStm of
                KWElse : restAfterElse -> do
                    (elseStm, restAfterElseStm) <- parseStm restAfterElse
                    return (IF condExpr thenStm elseStm, restAfterElseStm)
                _noElse -> Nothing -- Error handling if 'else' is missing
        _notThen -> Nothing -- Error handling if 'then' is missing
parseIf _ = Nothing

-- parser for while loops
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile (KWWhile : tokens) = do
    (condExpr, restAfterCond) <- parseBexp tokens
    case restAfterCond of
        KWDo : restAfterDo -> do
            (loopBody, restAfterLoopBody) <- parseStm restAfterDo
            return (WHILE condExpr loopBody, restAfterLoopBody)
        _notDo -> Nothing -- Error handling if 'do' is missing
parseWhile _ = Nothing



parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens =
    parseSeq tokens  <|> parseIf tokens <|> parseWhile tokens <|> parseAssignment tokens 
    


buildData :: [Token] -> Program
buildData tokens = fst $ parseStms tokens

parseStms :: [Token] -> (Program, [Token])
parseStms [] = ([], [])
parseStms tokens =
    case parseStm tokens of
        Just (stm, restTokens) -> 
            let (stms, remaining) = parseStms restTokens
            in (stm : stms, remaining)
        Nothing -> ([], tokens)  




parse :: String -> Program
parse = buildData . lexer -- parse is the composition of builData and lexer has we have on project instructions

