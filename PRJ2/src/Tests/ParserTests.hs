module Main where

import Assembler
import ImperativeLanguage
import Lexer
import Test.HUnit hiding (State)
import Test.HUnit.Text (runTestTTAndExit,runTestTT)
import Parser
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

testSeqStatements :: Test
testSeqStatements = TestList [
    TestCase $ assertEqual "(x := 5; x := x - 1;);"
               (SEQ [ASSIGNINT "x" (INTVAL 5), ASSIGNINT "x" (SUB (VAR "x") (INTVAL 1))])
               (parseAndEvaluateStm "(x := 5; x := x - 1;);"),

    TestCase $ assertEqual "(y := 10; z := y + 5;);"
               (SEQ [ASSIGNINT "y" (INTVAL 10), ASSIGNINT "z" (ADD (VAR "y") (INTVAL 5))])
               (parseAndEvaluateStm "(y := 10; z := y + 5;);"),

    TestCase $ assertEqual "(a := True; b := a = False;);"
               (SEQ [ASSIGNBOOL "a" TRU, ASSIGNBOOL "b" (EQUBOOL (AEXPRBOOL (VAR "a")) FALS)])
               (parseAndEvaluateStm "(a := True; b := a = False;);"),

    TestCase $ assertEqual "(c := x <= y; d := x == 2;)"
               (SEQ [ASSIGNBOOL "c" (LE (VAR "x") (VAR "y")), ASSIGNBOOL "d" (EQUINT (VAR "x") (INTVAL 2))])
               (parseAndEvaluateStm "(c := x <= y; d := x == 2;);"),

    TestCase $ assertEqual "(e := not (x = y); f := True and False;)"
               (SEQ [ASSIGNBOOL "e" (NEG (EQUBOOL (AEXPRBOOL (VAR "x")) (AEXPRBOOL (VAR "y")))), ASSIGNBOOL "f" (AND TRU FALS)])
               (parseAndEvaluateStm "(e := not (x = y); f := True and False;);")
    ]

   -- tests for if statements
testIfStatements :: Test
testIfStatements = TestList [
    TestCase $ assertEqual "Simple if-then-else without parentheses"
               (IF (LE (VAR "x") (INTVAL 43)) (ASSIGNINT "x" (INTVAL 1)) (ASSIGNINT "y" (INTVAL 2)))
               (parseAndEvaluateStm "if x <= 43 then x := 1; else y := 2;"),

    TestCase $ assertEqual "Complex if-then-else with parentheses"
               (IF (AND (NEG TRU) (EQUBOOL (LE (INTVAL 2) (INTVAL 5)) (EQUINT (INTVAL 3) (INTVAL 4)))) (SEQ [ASSIGNINT "x" (INTVAL 1), ASSIGNINT "y" (INTVAL 2)]) (ASSIGNINT "y" (INTVAL 3)))
               (parseAndEvaluateStm "if (not True and 2 <= 5 = 3 == 4) then (x := 1; y := 2;) else y := 3;"),
    TestCase $ assertEqual "Single statement in then and else without parentheses"
               (IF (EQUINT (VAR "x") (INTVAL 33)) (ASSIGNINT "x" (INTVAL 1)) (ASSIGNINT "y" (INTVAL 2)))
               (parseAndEvaluateStm "if x == 33 then x := 1; else y := 2;"),

    TestCase $ assertEqual "Multiple statements in then and else with parentheses"
               (IF (EQUBOOL (AEXPRBOOL (VAR "y")) TRU) (SEQ [ASSIGNINT "x" (INTVAL 33), ASSIGNINT "x" (ADD (VAR "x") (INTVAL 1))]) (SEQ [ASSIGNINT "x" (INTVAL 33), ASSIGNINT "x" (ADD (VAR "x") (INTVAL 1))]))
               (parseAndEvaluateStm "if (y = True) then (x := 33; x := x + 1;) else (x := 33; x := x + 1;);"),

    TestCase $ assertEqual "Nested if statement"
               (IF (EQUBOOL (AEXPRBOOL (VAR "z")) FALS) (IF (LE (VAR "x") (INTVAL 10)) (ASSIGNINT "y" (INTVAL 1)) (ASSIGNINT "y" (INTVAL 2))) (ASSIGNINT "y" (INTVAL 3)))
               (parseAndEvaluateStm "if z = False then if x <= 10 then y := 1; else y := 2; else y := 3;"),

    TestCase $ assertEqual "If statement with complex boolean expression"
               (IF (AND (NEG (EQUBOOL TRU FALS)) (LE (VAR "x") (VAR "y"))) (ASSIGNINT "z" (INTVAL 5)) (ASSIGNINT "z" (INTVAL 10)))
               (parseAndEvaluateStm "if not (True = False) and x <= y then z := 5; else z := 10;"),

    TestCase $ assertEqual "If statement with single then statement and multiple else statements"
               (IF (EQUBOOL (AEXPRBOOL (VAR "x")) TRU) (ASSIGNINT "y" (INTVAL 100)) (SEQ [ASSIGNINT "y" (INTVAL 200), ASSIGNINT "z" (INTVAL 300)]))
               (parseAndEvaluateStm "if x = True then y := 100; else (y := 200; z := 300;);"),

    TestCase $ assertEqual "If statement with arithmetic comparison in condition"
               (IF (LE (ADD (VAR "x") (VAR "y")) (INTVAL 50)) (ASSIGNINT "z" (INTVAL 5)) (ASSIGNINT "z" (INTVAL 10)))
               (parseAndEvaluateStm "if x + y <= 50 then z := 5; else z := 10;"),

    TestCase $ assertEqual "If statement with boolean expression in then and else"
               (IF (LE (VAR "x") (VAR "y")) (ASSIGNBOOL "flag" TRU) (ASSIGNBOOL "flag" FALS))
               (parseAndEvaluateStm "if x <= y then flag := True; else flag := False;"),

    TestCase $ assertEqual "If statement with nested boolean expressions"
               (IF (AND (LE (VAR "x") (VAR "y")) (NEG (EQUBOOL (AEXPRBOOL (VAR "z")) TRU))) (ASSIGNINT "result" (INTVAL 1)) (ASSIGNINT "result" (INTVAL 0)))
               (parseAndEvaluateStm "if x <= y and not (z = True) then result := 1; else result := 0;")
    ]
-- tests for parser of while statements

testWhileStatements :: Test
testWhileStatements = TestList [
    TestCase $ assertEqual "Simple while loop with arithmetic condition"
               (WHILE (LE (VAR "count") (INTVAL 10)) (ASSIGNINT "count" (ADD (VAR "count") (INTVAL 1))))
               (parseAndEvaluateStm "while (count <= 10) do count := count + 1;"),

    TestCase $ assertEqual "While loop with boolean condition"
               (WHILE TRU (ASSIGNINT "flag" (INTVAL 0)))
               (parseAndEvaluateStm "while (True) do flag := 0;"),

    TestCase $ assertEqual "Nested while loops"
               (WHILE (NEG (EQUINT (VAR "x") (INTVAL 0)))
                    (SEQ [WHILE (EQUINT (VAR "y") (INTVAL 0)) (ASSIGNINT "y" (INTVAL 10)), ASSIGNINT "x" (SUB (VAR "x") (INTVAL 1))]))
               (parseAndEvaluateStm "while (not(x == 0)) do ( while (y == 0) do y := 10; x := x - 1; );"),

    TestCase $ assertEqual "While loop with If-Else statement"
               (WHILE (LE (VAR "i") (INTVAL 5))
                    (IF (EQUINT (VAR "i") (INTVAL 3)) (ASSIGNINT "result" (INTVAL 1)) (ASSIGNINT "result" (INTVAL 0))))
               (parseAndEvaluateStm "while (i <= 5) do if (i == 3) then result := 1; else result := 0;"),

    TestCase $ assertEqual "Complex boolean expression in while loop"
               (WHILE (AND (LE (VAR "a") (VAR "b")) (NEG (EQUINT (VAR "c") (INTVAL 0))))
                    (ASSIGNINT "a" (ADD (VAR "a") (INTVAL 1))))
               (parseAndEvaluateStm "while (a <= b and not(c == 0)) do a := a + 1;"),

    TestCase $ assertEqual "While loop with multiple statements"
               (WHILE (TRU)
                    (SEQ [ASSIGNINT "x" (INTVAL 5), ASSIGNINT "y" (MULT (VAR "x") (INTVAL 2))]))
               (parseAndEvaluateStm "while (True) do ( x := 5; y := x * 2; );"),

    TestCase $ assertEqual "While loop with nested If and nested While"
               (WHILE (EQUBOOL (AEXPRBOOL (VAR "x")) FALS)
                    (IF (LE (VAR "y") (INTVAL 10))
                       (SEQ [WHILE (EQUINT (VAR "z") (INTVAL 0)) (ASSIGNINT "z" (INTVAL 1)), ASSIGNINT "y" (ADD (VAR "y") (INTVAL 1))])
                       (ASSIGNINT "y" (INTVAL 0))))
               (parseAndEvaluateStm "while (x = False) do if (y <= 10) then ( while (z == 0) do z := 1; y := y + 1; ) else y := 0;"),

   TestCase $ assertEqual "Parsing while loop with complex body"
    (WHILE (NEG (EQUINT (VAR "a") (VAR "b")))
        (SEQ [ASSIGNINT "a" (ADD (VAR "a") (INTVAL 1)), 
              ASSIGNINT "b" (SUB (VAR "b") (INTVAL 1)), 
              ASSIGNINT "result" (ADD (VAR "a") (VAR "b"))]))
    (parseAndEvaluateStm "while (not(a == b)) do ( a := a + 1; b := b - 1; result := a + b; );"),

    TestCase $ assertEqual "Parsing while loop with complex body"
    (WHILE (NEG (EQUINT (VAR "a") (VAR "b")))
        (SEQ [ASSIGNINT "a" (ADD (VAR "a") (INTVAL 1)),
              ASSIGNINT "b" (SUB (VAR "b") (INTVAL 1)),
              ASSIGNINT "result" (ADD (VAR "a") (VAR "b"))]))
    (parseAndEvaluateStm "while (not(a == b)) do ( a := a + 1; b := b - 1; result := a + b; );"),

    TestCase $ assertEqual "While loop with complex boolean expression and nested loops"
               (WHILE (AND (NEG (EQUINT (VAR "x") (INTVAL 5))) (EQUBOOL (AEXPRBOOL (VAR "y")) TRU))
                    (SEQ [WHILE (LE (VAR "z") (INTVAL 20)) (ASSIGNINT "z" (ADD (VAR "z") (INTVAL 1))), ASSIGNINT "x" (SUB (VAR "x") (INTVAL 1))]))
               (parseAndEvaluateStm "while (not(x == 5) and y = True) do ( while (z <= 20) do z := z + 1; x := x - 1; );")

    ]

-- integration tests using final parser : parse
parseTests :: Test
parseTests = TestList [
    TestCase $ assertEqual "Parsing multiple assignment statements"
    [ASSIGNINT "x" (INTVAL 42), ASSIGNINT "y" (INTVAL 46)]
    (parse "x := 42; y := 46;")




    ]


allStatmentsTests :: Test
allStatmentsTests = TestList [TestLabel "testSAssigmentParsing" testAssigmentParsing,
                         TestLabel "testSeqStatements" testSeqStatements, TestLabel "testIfStatements" testIfStatements, TestLabel "testWhileStatements" testWhileStatements]

allParseTests:: Test
allParseTests = TestList [TestLabel "parseTests" parseTests]

main :: IO ()
main = do
    let allTests = TestList [ allNewBexpTests, allAexpTests, allFirstBexpTests, allStatmentsTests, allParseTests]
    runTestTTAndExit allTests