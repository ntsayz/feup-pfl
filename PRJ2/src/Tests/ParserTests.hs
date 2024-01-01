module Main where

import ParserV2
import Assembler
import ImperativeLanguage
import Lexer
import Test.HUnit hiding (State)
import Test.HUnit.Text (runTestTTAndExit,runTestTT)
import ParserV2 (parseAnd)

--Test functions for the Parser module

-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- IF possibilities: 
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")

-- WHILE possibilities:
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


-- Test functions for arithmetic expressions with parentheses

-- Function to parsing and evaluating Aexp expressions
parseAndEvaluateAexp :: String -> Aexp
parseAndEvaluateAexp str =
    case parseAexp . lexer $ str of
        Just (aexp, _) -> aexp
        Nothing -> error "Parse error"

-- Function to parsing and evaluating Bexp expressions
parseAndEvaluateBexp :: String -> Bexp
parseAndEvaluateBexp str =
    case parseBexp . lexer $ str of
        Just (bexp, _) -> bexp
        Nothing -> error "Parse error"

parseAndEvaluateStm :: String -> Stm
parseAndEvaluateStm str =
    case parseStm . lexer $ str of
        Just (stm, _) -> stm
        Nothing -> error "Parse error"


-- Test for precedence
testPrecedence :: Test
testPrecedence = TestList [
    TestCase $ assertEqual "3 * 4 + 5" (ADD (MULT (INTVAL 3) (INTVAL 4)) (INTVAL 5)) (parseAndEvaluateAexp "3 * 4 + 5"),
    TestCase $ assertEqual "6 - 2 + 1" (ADD (SUB (INTVAL 6) (INTVAL 2)) (INTVAL 1)) (parseAndEvaluateAexp "6 - 2 + 1"),
    TestCase $ assertEqual "5 + 3 * 2 * 4" (ADD (INTVAL 5) (MULT (MULT (INTVAL 3) (INTVAL 2)) (INTVAL 4))) (parseAndEvaluateAexp "5 + 3 * 2 * 4"),
    TestCase $ assertEqual "8 - 4 * 2" (SUB (INTVAL 8) (MULT (INTVAL 4) (INTVAL 2))) (parseAndEvaluateAexp "8 - 4 * 2"),
    TestCase $ assertEqual "7 * 2 + 10 * 5" (ADD (MULT (INTVAL 7) (INTVAL 2)) (MULT (INTVAL 10) (INTVAL 5))) (parseAndEvaluateAexp "7 * 2 + 10 * 5"),
    TestCase $ assertEqual "1 + 2 * 3" (ADD (INTVAL 1) (MULT (INTVAL 2) (INTVAL 3))) (parseAndEvaluateAexp "1 + 2 * 3"),
    TestCase $ assertEqual "2 + 3 * 4 - 5" (SUB (ADD (INTVAL 2) (MULT (INTVAL 3) (INTVAL 4))) (INTVAL 5)) (parseAndEvaluateAexp "2 + 3 * 4 - 5"),
    TestCase $ assertEqual "4 + 5 * 6 - 7" (SUB (ADD (INTVAL 4) (MULT (INTVAL 5) (INTVAL 6))) (INTVAL 7)) (parseAndEvaluateAexp "4 + 5 * 6 - 7"),
    TestCase $ assertEqual "3 * 4 + 6 * 2" (ADD (MULT (INTVAL 3) (INTVAL 4)) (MULT (INTVAL 6) (INTVAL 2))) (parseAndEvaluateAexp "3 * 4 + 6 * 2")
   
    ]



-- Test for left-to-right associativity
testAssociativity :: Test
testAssociativity = TestList [
    TestCase $ assertEqual "20 - 5 - 2" (SUB (SUB (INTVAL 20) (INTVAL 5)) (INTVAL 2)) (parseAndEvaluateAexp "20 - 5 - 2"),
    TestCase $ assertEqual "30 + 10 + 5" (ADD (ADD (INTVAL 30) (INTVAL 10)) (INTVAL 5)) (parseAndEvaluateAexp "30 + 10 + 5"),
    TestCase $ assertEqual "4 * 3 * 2" (MULT (MULT (INTVAL 4) (INTVAL 3)) (INTVAL 2)) (parseAndEvaluateAexp "4 * 3 * 2"),
    TestCase $ assertEqual "18 - 3 - 2" (SUB (SUB (INTVAL 18) (INTVAL 3)) (INTVAL 2)) (parseAndEvaluateAexp "18 - 3 - 2"),
    TestCase $ assertEqual "15 - 5 + 3" (ADD (SUB (INTVAL 15) (INTVAL 5)) (INTVAL 3)) (parseAndEvaluateAexp "15 - 5 + 3"),
    TestCase $ assertEqual "20 + 5 - 2" (SUB (ADD (INTVAL 20) (INTVAL 5)) (INTVAL 2)) (parseAndEvaluateAexp "20 + 5 - 2"),
    TestCase $ assertEqual "10 - 5 - 3" (SUB (SUB (INTVAL 10) (INTVAL 5)) (INTVAL 3)) (parseAndEvaluateAexp "10 - 5 - 3"),
    TestCase $ assertEqual "12 * 4 * 2" (MULT (MULT (INTVAL 12) (INTVAL 4)) (INTVAL 2)) (parseAndEvaluateAexp "12 * 4 * 2")
    ]



-- Test for parentheses overriding precedence
testParentheses :: Test
testParentheses = TestList [
    TestCase $ assertEqual "(3 + 4) * (5 - 2)" (MULT (ADD (INTVAL 3) (INTVAL 4)) (SUB (INTVAL 5) (INTVAL 2))) (parseAndEvaluateAexp "(3 + 4) * (5 - 2)"),
    TestCase $ assertEqual "(6 - 3 + 2) * 4" (MULT (ADD (SUB (INTVAL 6) (INTVAL 3)) (INTVAL 2)) (INTVAL 4)) (parseAndEvaluateAexp "(6 - 3 + 2) * 4"),
    TestCase $ assertEqual "400 * (3 + (2 * 5))" (MULT (INTVAL 400) (ADD (INTVAL 3) (MULT (INTVAL 2) (INTVAL 5)))) (parseAndEvaluateAexp "400 * (3 + (2 * 5))"),
    TestCase $ assertEqual "(8 - (4 * 2)) * 3" (MULT (SUB (INTVAL 8) (MULT (INTVAL 4) (INTVAL 2))) (INTVAL 3)) (parseAndEvaluateAexp "(8 - (4 * 2)) * 3"),
    TestCase $ assertEqual "(7 + 2) * 10 * 3" (MULT (MULT (ADD (INTVAL 7) (INTVAL 2)) (INTVAL 10)) (INTVAL 3)) (parseAndEvaluateAexp "(7 + 2) * 10 * 3"),
    TestCase $ assertEqual "(2 + 3) * (4 - 5)" (MULT (ADD (INTVAL 2) (INTVAL 3)) (SUB (INTVAL 4) (INTVAL 5))) (parseAndEvaluateAexp "(2 + 3) * (4 - 5)"),
    TestCase $ assertEqual "4 * ((3 + 2) * 5)" (MULT (INTVAL 4) (MULT (ADD (INTVAL 3) (INTVAL 2)) (INTVAL 5))) (parseAndEvaluateAexp "4 * ((3 + 2) * 5)"),
    TestCase $ assertEqual "(8 - 4) * (2 + 3)" (MULT (SUB (INTVAL 8) (INTVAL 4)) (ADD (INTVAL 2) (INTVAL 3))) (parseAndEvaluateAexp "(8 - 4) * (2 + 3)")
    ]



-- Grouping all tests

-- Run the tests
allAexpTests :: Test
allAexpTests = TestList [TestLabel "testPrecedence" testPrecedence,
                  TestLabel "testAssociativity" testAssociativity,
                  TestLabel "testParentheses" testParentheses]

-- Other tests for Bexp

testParseBoolLiteral :: Test
testParseBoolLiteral = TestList [
    TestCase $ assertEqual "for True," TRU (parseAndEvaluateBexp "True"),
    TestCase $ assertEqual "for False," FALS (parseAndEvaluateBexp "False")
    ]


testParseEquality :: Test
testParseEquality = TestList [
    TestCase $ assertEqual "for 3 == 3," (EQUINT (INTVAL 3) (INTVAL 3)) (parseAndEvaluateBexp "3 == 3")
   
    ]



testParseEqBool :: Test
testParseEqBool = TestList [
    TestCase $ assertEqual "for True = False," (EQUBOOL TRU FALS) (parseAndEvaluateBexp "True = False"),
    TestCase $ assertEqual "for (True = False) = (False = False)," (EQUBOOL (EQUBOOL TRU FALS) (EQUBOOL FALS FALS)) (parseAndEvaluateBexp "(True = False) = (False = False)"),-- using parentheses
    TestCase $ assertEqual "for (2 + 1 == x) = (False = False)," (EQUBOOL (EQUINT (ADD (INTVAL 2) (INTVAL 1)) (VAR "x")) (EQUBOOL FALS FALS)) (parseAndEvaluateBexp "(2 + 1 == x) = (False = False)")-- using parentheses
    
    
    ]



testParseBoolVar :: Test
testParseBoolVar = TestList [
    TestCase $ assertEqual "for x," (AEXPRBOOL (VAR "x")) (parseAndEvaluateBexp "x")
    
    ]

testParseLe :: Test
testParseLe = TestList [ 
    TestCase $ assertEqual "for 2 <= 3," (LE (INTVAL 2) (INTVAL 3)) (parseAndEvaluateBexp "2 <= 3"),
    TestCase $ assertEqual "for x <= 5," (LE (VAR "x") (INTVAL 5)) (parseAndEvaluateBexp "x <= 5"),
    TestCase $ assertEqual "for (1 <= 2) = True," (EQUBOOL (LE (INTVAL 1) (INTVAL 2)) TRU) (parseAndEvaluateBexp "(1 <= 2) = True"),
    TestCase $ assertEqual "for (x + 1) <= (y - 2)," (LE (ADD (VAR "x") (INTVAL 1)) (SUB (VAR "y") (INTVAL 2))) (parseAndEvaluateBexp "(x + 1) <= (y - 2)"),
    TestCase $ assertEqual "for (3 * 4) <= (2 + 5)," (LE (MULT (INTVAL 3) (INTVAL 4)) (ADD (INTVAL 2) (INTVAL 5))) (parseAndEvaluateBexp "(3 * 4) <= (2 + 5)")


    ]

testNot :: Test
testNot = TestList [
    TestCase $ assertEqual "for not True," (NEG TRU) (parseAndEvaluateBexp "not True"),
    TestCase $ assertEqual "for not x," (NEG (AEXPRBOOL (VAR "x"))) (parseAndEvaluateBexp "not x"),
    TestCase $ assertEqual "for not (1 == 2)," (NEG (EQUINT (INTVAL 1) (INTVAL 2))) (parseAndEvaluateBexp "not (1 == 2)"),
    TestCase $ assertEqual "for not (not False)," (NEG (NEG FALS)) (parseAndEvaluateBexp "not (not False)"),
    --TestCase $ assertEqual "for not (x and True)," (NEG (AND (VARBOOL "x") TRU)) (parseAndEvaluateBexp "not (x and True)")
    TestCase $ assertEqual "for not False," (NEG FALS) (parseAndEvaluateBexp "not False"),
    TestCase $ assertEqual "for not (False)," (NEG FALS) (parseAndEvaluateBexp "not (False)"),
    TestCase $ assertEqual "for not not False," (NEG (NEG FALS)) (parseAndEvaluateBexp "not not False"),
    TestCase $ assertEqual "for not (not False)," (NEG (NEG FALS)) (parseAndEvaluateBexp "not (not False)"),
    TestCase $ assertEqual "for not not (not False)," (NEG (NEG (NEG FALS))) (parseAndEvaluateBexp "not not (not False)"),
    TestCase $ assertEqual "for not (1 == 2)," (NEG (EQUINT (INTVAL 1) (INTVAL 2))) (parseAndEvaluateBexp "not (1 == 2)"),
    TestCase $ assertEqual "for not (x = True)," (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) TRU)) (parseAndEvaluateBexp "not (x = True)"),
    TestCase $ assertEqual "for not (not (x == 1))," (NEG (NEG (EQUINT (VAR "x") (INTVAL 1)))) (parseAndEvaluateBexp "not (not (x == 1))"),
    TestCase $ assertEqual "for not not (x = False)," (NEG (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) FALS))) (parseAndEvaluateBexp "not not (x = False)"),
    TestCase $ assertEqual "for not not not x == 2," 
               (NEG (NEG (NEG (EQUINT (VAR "x") (INTVAL 2)))) )
               (parseAndEvaluateBexp "not not not x == 2"),

    TestCase $ assertEqual "for not not not x = True," 
               (EQUBOOL (NEG (NEG (NEG (AEXPRBOOL (VAR "x"))))) TRU) 
               (parseAndEvaluateBexp "not not not x = True"),

    TestCase $ assertEqual "for not not not x = y," 
               (EQUBOOL (NEG (NEG (NEG (AEXPRBOOL (VAR "x"))))) (AEXPRBOOL (VAR "y")))
               (parseAndEvaluateBexp "not not not x = y")
    ]



testAndOperations :: Test
testAndOperations = TestList [
    TestCase $ assertEqual "for True and True," 
               (AND TRU TRU) 
               (parseAndEvaluateBexp "True and True"),

    TestCase $ assertEqual "for False and True," 
               (AND FALS TRU) 
               (parseAndEvaluateBexp "False and True"),

    TestCase $ assertEqual "for x and y," 
               (AND (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y"))) 
               (parseAndEvaluateBexp "x and y"),

    TestCase $ assertEqual "for not x and y," 
               (AND (NEG (AEXPRBOOL (VAR "x"))) (AEXPRBOOL (VAR "y"))) 
               (parseAndEvaluateBexp "not x and y"),

    TestCase $ assertEqual "for x and not y," 
               (AND (AEXPRBOOL (VAR "x")) (NEG (AEXPRBOOL (VAR "y")))) 
               (parseAndEvaluateBexp "x and not y"),

    TestCase $ assertEqual "for (x = 2) and (y = 3)," 
           (AND (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (INTVAL 2))) 
                (EQUBOOL (AEXPRBOOL (VAR "y")) (AEXPRBOOL (INTVAL 3)))) 
           (parseAndEvaluateBexp "(x = 2) and (y = 3)"),

    TestCase $ assertEqual "for (1 + 1 == 2) and True," 
           (AND (EQUINT (ADD (INTVAL 1) (INTVAL 1)) (INTVAL 2)) TRU) 
           (parseAndEvaluateBexp "(1 + 1 == 2) and True"),

    TestCase $ assertEqual "for False and (3 <= 4)," 
               (AND FALS (LE (INTVAL 3) (INTVAL 4))) 
               (parseAndEvaluateBexp "False and (3 <= 4)"),

    TestCase $ assertEqual "for not (x = y) and (3 == 3)," 
           (AND (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y")))) 
                (EQUINT (INTVAL 3) (INTVAL 3))) 
           (parseAndEvaluateBexp "not (x = y) and (3 == 3)"),

    TestCase $ assertEqual "for (x and y) and (not z)," 
           (AND (AND (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y"))) 
                (NEG (AEXPRBOOL (VAR "z")))) 
           (parseAndEvaluateBexp "(x and y) and (not z)")
    ]




allNewBexpTests :: Test


allNewBexpTests = TestList [
    TestLabel "Bool Literal Tests" testParseBoolLiteral,
    TestLabel "Equality Tests" testParseEquality,
    TestLabel "Boolean Equality Tests" testParseEqBool,
    TestLabel "Boolean Variable Tests" testParseBoolVar,
    TestLabel "Less than or equal Tests" testParseLe,
    TestLabel "Not Tests" testNot,
    TestLabel "And Tests" testAndOperations
    ]



testBoolTerm :: Test
testBoolTerm = TestList [
    TestCase $ assertEqual "True" TRU (parseAndEvaluateBexp "True"),
    TestCase $ assertEqual "False" FALS (parseAndEvaluateBexp "False")
    ]


testEQUINT :: Test
testEQUINT = TestList [
    TestCase $ assertEqual "1 == 1" (EQUINT (INTVAL 1) (INTVAL 1)) (parseAndEvaluateBexp "1 == 1"),
    TestCase $ assertEqual "2 == 3" (EQUINT (INTVAL 2) (INTVAL 3)) (parseAndEvaluateBexp "2 == 3"),
    TestCase $ assertEqual "(1 + 2) == 3" (EQUINT (ADD (INTVAL 1) (INTVAL 2)) (INTVAL 3)) (parseAndEvaluateBexp "(1 + 2) == 3"),
    TestCase $ assertEqual "4 == (2 * 2)" (EQUINT (INTVAL 4) (MULT (INTVAL 2) (INTVAL 2))) (parseAndEvaluateBexp "4 == (2 * 2)"),
    TestCase $ assertEqual "(4 + 3) == (2 * 2)" (EQUINT (ADD (INTVAL 4) (INTVAL 3)) (MULT (INTVAL 2) (INTVAL 2))) (parseAndEvaluateBexp "(4 + 3) == (2 * 2)")
    ]


testLE :: Test
testLE = TestList [
    TestCase $ assertEqual "1 <= 2" (LE (INTVAL 1) (INTVAL 2)) (parseAndEvaluateBexp "1 <= 2"),
    TestCase $ assertEqual "3 <= 3" (LE (INTVAL 3) (INTVAL 3)) (parseAndEvaluateBexp "3 <= 3"),
    TestCase $ assertEqual "(3 * 2) <= (4 + 2)" (LE (MULT (INTVAL 3) (INTVAL 2)) (ADD (INTVAL 4) (INTVAL 2))) (parseAndEvaluateBexp "(3 * 2) <= (4 + 2)"),
    TestCase $ assertEqual "(5 - 1) <= 4" (LE (SUB (INTVAL 5) (INTVAL 1)) (INTVAL 4)) (parseAndEvaluateBexp "(5 - 1) <= 4")
    ]

testEQUBOOL :: Test
testEQUBOOL = TestList [
    TestCase $ assertEqual "True = True" (EQUBOOL TRU TRU) (parseAndEvaluateBexp "True = True"),
    TestCase $ assertEqual "False = True" (EQUBOOL FALS TRU) (parseAndEvaluateBexp "False = True"),
    TestCase $ assertEqual "(True and False) = False" (EQUBOOL (AND TRU FALS) FALS) (parseAndEvaluateBexp "(True and False) = False"),
    TestCase $ assertEqual "not (True = False)" (NEG (EQUBOOL TRU FALS)) (parseAndEvaluateBexp "not (True = False)"),
    TestCase $ assertEqual "(True and False) = (False)" (EQUBOOL (AND TRU FALS) FALS) (parseAndEvaluateBexp "(True and False) = (False)")
    ]


testNEG :: Test
testNEG = TestList [
    TestCase $ assertEqual "not True" (NEG TRU) (parseAndEvaluateBexp "not True"),
    TestCase $ assertEqual "not False" (NEG FALS) (parseAndEvaluateBexp "not False"),
    TestCase $ assertEqual "not (1 == 2)" (NEG (EQUINT (INTVAL 1) (INTVAL 2))) (parseAndEvaluateBexp "not (1 == 2)"),
    TestCase $ assertEqual "not (True and False)" (NEG (AND TRU FALS)) (parseAndEvaluateBexp "not (True and False)")
    ]


testAND :: Test
testAND = TestList [
    TestCase $ assertEqual "True and False" (AND TRU FALS) (parseAndEvaluateBexp "True and False"),
    TestCase $ assertEqual "True and True" (AND TRU TRU) (parseAndEvaluateBexp "True and True"),
    TestCase $ assertEqual "(True and not False) and (1 <= 2)" (AND (AND TRU (NEG FALS)) (LE (INTVAL 1) (INTVAL 2))) (parseAndEvaluateBexp "(True and not False) and (1 <= 2)"),
    TestCase $ assertEqual "(3 == 3) and not (4 == 5)" (AND (EQUINT (INTVAL 3) (INTVAL 3)) (NEG (EQUINT (INTVAL 4) (INTVAL 5)))) (parseAndEvaluateBexp "(3 == 3) and not (4 == 5)")
    ]

-- Grouping all tests
allFirstBexpTests :: Test
allFirstBexpTests = TestList [TestLabel "testBoolTerm" testBoolTerm,
                         TestLabel "testEQUINT" testEQUINT,
                         TestLabel "testLE" testLE,
                         TestLabel "testEQUBOOL" testEQUBOOL,
                         TestLabel "testNEG" testNEG,
                         TestLabel "testAND" testAND]



-- Test parsers for statements

testAssigmentParsing :: Test
testAssigmentParsing = TestList [
    
    TestCase $ assertEqual "x := 5" (ASSIGNINT "x" (INTVAL 5)) (parseAndEvaluateStm "x := 5;"),
    TestCase $ assertEqual "y := 3 * 4" (ASSIGNINT "y" (MULT (INTVAL 3) (INTVAL 4))) (parseAndEvaluateStm "y := 3 * 4;"),
    TestCase $ assertEqual "z := 2 + 3 - 1" (ASSIGNINT "z" (SUB (ADD (INTVAL 2) (INTVAL 3)) (INTVAL 1))) (parseAndEvaluateStm "z := 2 + 3 - 1;"),
    
    
    TestCase $ assertEqual "a := True" (ASSIGNBOOL "a" TRU) (parseAndEvaluateStm "a := True;"),
    TestCase $ assertEqual "b := False" (ASSIGNBOOL "b" FALS) (parseAndEvaluateStm "b := False;"),
    TestCase $ assertEqual "c := x <= y" (ASSIGNBOOL "c" (LE (VAR "x") (VAR "y"))) (parseAndEvaluateStm "c := x <= y;"),
    
    
    TestCase $ assertEqual "d := (5 + 3) * 2" (ASSIGNINT "d" (MULT (ADD (INTVAL 5) (INTVAL 3)) (INTVAL 2))) (parseAndEvaluateStm "d := (5 + 3) * 2;"),
    TestCase $ assertEqual "e := not (x = y)" 
           (ASSIGNBOOL "e" (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y"))))) 
           (parseAndEvaluateStm "e := not (x = y);"),
 
  
    TestCase $ assertEqual "f := 1 + 2 * 3" (ASSIGNINT "f" (ADD (INTVAL 1) (MULT (INTVAL 2) (INTVAL 3)))) (parseAndEvaluateStm "f := 1 + 2 * 3;"),
    TestCase $ assertEqual "g := (True and False) and x = 5" 
           (ASSIGNBOOL "g" (AND (AND TRU FALS) (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (INTVAL 5))))) 
           (parseAndEvaluateStm "g := (True and False) and x = 5;"),
    TestCase $ assertEqual "a := x <= 5" (ASSIGNBOOL "a" (LE (VAR "x") (INTVAL 5))) (parseAndEvaluateStm "a := x <= 5;"),
    TestCase $ assertEqual "b := y == 7" (ASSIGNBOOL "b" (EQUINT (VAR "y") (INTVAL 7))) (parseAndEvaluateStm "b := y == 7;"),
    TestCase $ assertEqual "c := z = True" (ASSIGNBOOL "c" (EQUBOOL (AEXPRBOOL (VAR "z")) TRU)) (parseAndEvaluateStm "c := z = True;"),
    TestCase $ assertEqual "d := (x <= 5) = (y == 3)" (ASSIGNBOOL "d" (EQUBOOL (LE (VAR "x") (INTVAL 5)) (EQUINT (VAR "y") (INTVAL 3)))) (parseAndEvaluateStm "d := (x <= 5) = (y == 3);"),
    TestCase $ assertEqual "e := (w <= 2) = False" (ASSIGNBOOL "e" (EQUBOOL (LE (VAR "w") (INTVAL 2)) FALS)) (parseAndEvaluateStm "e := (w <= 2) = False;"),
    TestCase $ assertEqual "f := (v == 4) = True" (ASSIGNBOOL "f" (EQUBOOL (EQUINT (VAR "v") (INTVAL 4)) TRU)) (parseAndEvaluateStm "f := (v == 4) = True;"),
    TestCase $ assertEqual "g := (u <= 6) = (t == 1) and False" (ASSIGNBOOL "g" (AND (EQUBOOL (LE (VAR "u") (INTVAL 6)) (EQUINT (VAR "t") (INTVAL 1))) FALS)) (parseAndEvaluateStm "g := (u <= 6) = (t == 1) and False;"),
    TestCase $ assertEqual "h := x <= 4 and y = True" (ASSIGNBOOL "h" (AND (LE (VAR "x") (INTVAL 4)) (EQUBOOL (AEXPRBOOL (VAR "y")) TRU))) (parseAndEvaluateStm "h := x <= 4 and y = True;"),
    TestCase $ assertEqual "i := not (z = False)" (ASSIGNBOOL "i" (NEG (EQUBOOL (AEXPRBOOL (VAR "z")) FALS))) (parseAndEvaluateStm "i := not (z = False);"),
    TestCase $ assertEqual "k := (u <= 2) and not (t = False)" (ASSIGNBOOL "k" (AND (LE (VAR "u") (INTVAL 2)) (NEG (EQUBOOL (AEXPRBOOL (VAR "t")) FALS)))) (parseAndEvaluateStm "k := (u <= 2) and not (t = False);"),
    TestCase $ assertEqual "l := not (x = y) and (z <= 5) = True" (ASSIGNBOOL "l" (AND (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y")))) (EQUBOOL (LE (VAR "z") (INTVAL 5)) TRU))) (parseAndEvaluateStm "l := not (x = y) and (z <= 5) = True;")

    ]

allStatmentsTests :: Test
allStatmentsTests = TestList [TestLabel "testSAssigmentParsing" testAssigmentParsing]

main :: IO ()
main = do
    let allTests = TestList [ allNewBexpTests, allAexpTests, allFirstBexpTests, allStatmentsTests]
    runTestTTAndExit allTests