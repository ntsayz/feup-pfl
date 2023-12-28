module Main where

import Test.HUnit
import Lexer
import MachineStructures
import ImperativeLanguage
import Lexer (lexer, Token (OpEq))


-- Tests for every possible Token/statements as group of tokens





-- Tests using code examples from the project statement

-- Test cases
lexerProjectExamplesCase :: Test
lexerProjectExamplesCase = TestList [
    TestCase (assertEqual "for (lexer \"x := 5; x := x - 1;\")"
                         [VarName "x", OpAssign, IntLit 5, Semicolon, VarName "x", OpAssign, VarName "x", OpSub, IntLit 1, Semicolon]
                         (lexer "x := 5; x := x - 1;")),
    TestCase (assertEqual "for (lexer \"if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;\")"
                         [KWIf, OpenParen, OpNot, BoolLit True, OpAnd, IntLit 2, OpLe, IntLit 5, OpEq, IntLit 3, OpEq, IntLit 4, CloseParen, KWThen, VarName "x", OpAssign, IntLit 1, Semicolon, KWElse, VarName "y", OpAssign, IntLit 2, Semicolon]
                         (lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;")),
    -- Add more test cases here
    TestCase (assertEqual "for (lexer \"x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)\")"
                         [VarName "x", OpAssign, IntLit 42, Semicolon, KWIf, VarName "x", OpLe, IntLit 43 , KWThen, VarName "x", OpAssign, IntLit 1, Semicolon, KWElse, OpenParen, VarName "x", OpAssign, IntLit 33, Semicolon, VarName "x", OpAssign, VarName "x", OpAdd, IntLit 1, Semicolon, CloseParen]
                         (lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"))
    ]

lexerVarNameTests :: Test
lexerVarNameTests = TestList [
    TestCase (assertEqual "for (lexerVarName \"x\")" [VarName "x"] (lexerVarName "x")),
    TestCase (assertEqual "for (lexerVarName \"x1\")" [VarName "x1"] (lexerVarName "x1")),
    TestCase (assertEqual "for (lexerVarName \"x_\")" [VarName "x_"] (lexerVarName "x_")),
    TestCase (assertEqual "for (lexerVarName \"x_1\")" [VarName "x_1"] (lexerVarName "x_1")),
    TestCase (assertEqual "for (lexerVarName \"x_1_\")" [VarName "x_1_"] (lexerVarName "x_1_")),
    TestCase (assertEqual "for (lexerVarName \"x_1_2\")" [VarName "x_1_2"] (lexerVarName "x_1_2")),
    TestCase (assertEqual "for (lexerVarName \"x_1_2_\")" [VarName "x_1_2_"] (lexerVarName "x_1_2_")),
    TestCase (assertEqual "for (lexerVarName \"x_1_2_3\")" [VarName "x_1_2_3"] (lexerVarName "x_1_2_3")),
    TestCase (assertEqual "Simple variable name" [VarName "x123"] (lexerVarName "x123")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpAdd, OpEq] (lexerVarName "y+=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpSub, OpEq] (lexerVarName "y-=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpMult, OpEq] (lexerVarName "y*=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpLe] (lexerVarName "y<=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpEq] (lexerVarName "y==")),
    TestCase (assertEqual "Variable name with operators" [VarName "y!",OpEq] (lexerVarName "y!=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y&&"] (lexerVarName "y&&")),
    TestCase (assertEqual "Variable name with operators" [VarName "y||"] (lexerVarName "y||")),
    TestCase (assertEqual "Variable name with operators" [VarName "y_2_A_ZX!"] (lexerVarName "y_2_A_ZX!")),
    TestCase (assertEqual "Variable name with semicolon" [VarName "variable", Semicolon] (lexerVarName "variable;"))
    ]

lexerIntLitTests :: Test
lexerIntLitTests = TestList [
    TestCase (assertEqual "for (lexerIntLit \"42+\")" (IntLit 42, "+") (lexerIntLit "42+")),
    TestCase (assertEqual "for (lexerIntLit \"123 \")" (IntLit 123, " ") (lexerIntLit "123 ")),
    TestCase (assertEqual "for (lexerIntLit \"999-\")" (IntLit 999, "-") (lexerIntLit "999-"))
    ]

-- Test cases for lexer
lexerTests :: Test
lexerTests= TestList [ 
    TestCase (assertEqual "Combination of tokens" [VarName "x", OpAssign, IntLit 5, Semicolon] (lexer "x := 5;")),
    TestCase (assertEqual "Complex expression" [KWIf, VarName "x", OpLe, IntLit 10, KWThen, VarName "y", OpAssign, BoolLit True, Semicolon, KWElse, VarName "y", OpAssign, BoolLit False, Semicolon] (lexer "if x < 10 then y := True; else y := False;")),
    TestCase (assertEqual "Keywords and operators" [KWWhile, VarName "x", OpEq, IntLit 0, KWDo, VarName "x", OpAssign, VarName "x", OpSub, IntLit 1, Semicolon] (lexer "while x != 0 do x := x - 1;")),
    TestCase (assertEqual "Keywords and operators" [KWWhile, VarName "x", OpEq, IntLit 0, KWDo, VarName "x", OpAssign, VarName "x", OpSub, IntLit 1, Semicolon] (lexer "while (x != 0) do (x := x - 1;)"))
    ]

  
-- Main function to run tests
main = do
    let allTests = TestList [lexerVarNameTests, lexerIntLitTests, lexerProjectExamplesCase]

    runTestTTAndExit allTests