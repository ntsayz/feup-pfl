{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module ParserV2 where
import ImperativeLanguage

import Lexer
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Debug.Trace (trace)
import Control.Applicative ((<|>))




-- -- parser for arithmetic expressions

-- -- parser for integer literals
-- parseInt :: [Token] -> Maybe (Aexp, [Token])
-- parseInt (IntLit n : restTokens) = Just (INTVAL  n, restTokens)
-- parseInt tokens = Nothing




-- -- parser taking into account parentheses

-- parseIntOrParenAexp :: [Token] -> Maybe (Aexp, [Token])
-- parseIntOrParenAexp(IntLit n: restOfTokenAfterInt)
--     = Just (INTVAL n, restOfTokenAfterInt)
-- parseIntOrParenAexp (OpenParen: restOfTokenAfterOpenParen)
--     = case parseSumSubOrProdOrIntOrParen restOfTokenAfterOpenParen of
--         Just (aexp, CloseParen: restOfTokenAfterCloseParen)
--             -> Just (aexp, restOfTokenAfterCloseParen)
--         Just _-> Nothing -- missing closing parenthesis
--         Nothing -> Nothing -- invalid expression
-- parseIntOrParenAexp _ = Nothing -- invalid expression

-- -- parser for products or aexp with parentheses 
-- parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
-- parseProdOrIntOrPar tokens =
--     case parseIntOrParenAexp tokens of
--         Just (aexp1, OpMult : restTokensAfterMult) ->
--             case parseProdOrIntOrPar restTokensAfterMult of
--                 Just (aexp2, restTokensAfterAexp2) ->
--                     Just (MULT aexp1 aexp2, restTokensAfterAexp2)
--                 Nothing -> Nothing
--         result -> result

-- -- final aexp expressions parser with precedence
-- parseSumSubOrProdOrIntOrParen :: [Token] -> Maybe (Aexp, [Token])
-- parseSumSubOrProdOrIntOrParen tokens = 
--     parseSumSub tokens
--   where
--     parseSumSub ts = 
--         case parseProdOrIntOrPar ts of
--             Just (left, rest) -> 
--                 case rest of
--                     (OpAdd : rest') -> build ADD left rest'
--                     (OpSub : rest') -> build SUB left rest'
--                     _ -> Just (left, rest)
--             Nothing -> Nothing

--     build op left ts = 
--         case parseSumSub ts of
--             Just (right, rest') -> Just (op left right, rest')
--             Nothing -> Nothing

-- parseTerm :: [Token] -> Maybe (Aexp, [Token])
-- parseTerm (OpenParen : rest) = trace ("parseTerm: " ++ show( OpenParen : rest)) $
--     case parseExpression rest of
--         Just (expr, CloseParen : rest') -> Just (expr, rest')
--         Just (expr, _) -> trace "Missing closing parenthesis" Nothing  -- Handle missing CloseParen
--         _ -> trace "Failed to parse expression in parentheses" Nothing
-- parseTerm (IntLit n : rest) = trace ("parseTerm: " ++ show( IntLit n : rest)) $ Just (INTVAL n, rest)
-- parseTerm (VarName v : rest) = trace ("parseTerm: " ++ show( VarName v : rest)) $ Just (VAR v, rest)
-- parseTerm _ = trace "Unexpected token in parseTerm" Nothing
-- -- Add similar trace statements to parseProduct, parseProduct', parseSum, parseSum', and parseExpression

-- parseProduct :: [Token] -> Maybe (Aexp, [Token])
-- parseProduct tokens = trace ("parseProduct: " ++ show tokens) $
--     case parseTerm tokens of
--         Just (left, rest) -> parseProduct' left rest
--         Nothing -> trace "parseProduct: Failed in parseTerm" Nothing

-- parseProduct' :: Aexp -> [Token] -> Maybe (Aexp, [Token])
-- parseProduct' left tokens = trace ("parseProduct': " ++ show left ++ ", " ++ show tokens) $
--     case tokens of
--         OpMult : rest ->
--             case parseTerm rest of
--                 Just (right, rest') -> parseProduct' (MULT left right) rest'
--                 Nothing -> trace "parseProduct': Failed in parseTerm" Nothing
--         _ -> Just (left, tokens)

-- parseSum :: [Token] -> Maybe (Aexp, [Token])
-- parseSum tokens = trace ("parseSum: " ++ show tokens) $
--     case parseProduct tokens of
--         Just (left, rest) -> parseSum' left rest
--         Nothing -> trace "parseSum: Failed in parseProduct" Nothing

-- parseSum' :: Aexp -> [Token] -> Maybe (Aexp, [Token])
-- parseSum' left tokens = trace ("parseSum': " ++ show left ++ ", " ++ show tokens) $
--     case tokens of
--         OpAdd : rest ->
--             case parseProduct rest of
--                 Just (right, rest') -> parseSum' (ADD left right) rest'
--                 Nothing -> trace "parseSum': Failed in parseProduct" Nothing
--         OpSub : rest ->
--             case parseProduct rest of
--                 Just (right, rest') -> parseSum' (SUB left right) rest'
--                 Nothing -> trace "parseSum': Failed in parseProduct" Nothing
--         _ -> Just (left, tokens)

-- parseExpression :: [Token] -> Maybe (Aexp, [Token])
-- parseExpression tokens = trace ("parseExpression: " ++ show tokens) $
--     case parseSum tokens of
--         Just (expr, []) -> Just (expr, [])
--         Just (expr, rest) -> Just (expr, rest)  -- Handle remaining tokens
--         result -> trace ("parseExpression: Failed or incomplete parse: " ++ show result) Nothing

-- parsing arithmetic expressions Aexp with parentheses, left-to-right associativity and precedence


parseTerm :: [Token] -> Maybe (Aexp, [Token])
parseTerm (OpenParen : restTokensAfterOpenParen) = 
    case parseAexp restTokensAfterOpenParen of
        Just (expr, CloseParen : restAfterCloseParen) -> Just (expr, restAfterCloseParen)
        _ -> Nothing  -- Handle error, incomplete parse, or missing CloseParen
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



-- parser for Bexp expressions for our Imperative Language


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



parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = 
   case parseAnd tokens of
        Just (bexp, []) -> Just (bexp, [])
        Just (bexp, restAfterBexp) -> Just (bexp, restAfterBexp)
        _incompleteParsing -> Nothing  -- Handle incomplete parse or error
    
    






-- buildData :: [Token] -> Maybe (Aexp, [Token])
-- buildData tokens =
--         case parseExpression tokens of
--             Just (aexp, []) -> Just (aexp, [])
--             _errorParsingTokens ->  Nothing

-- parse :: String -> Maybe (Aexp, [Token])
-- parse = parseExpression . lexer -- parse will be the composition of builData and lexer
