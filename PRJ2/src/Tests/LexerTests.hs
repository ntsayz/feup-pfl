module Main where

import Test.HUnit
import Lexer
import MachineStructures
import ImperativeLanguage
import Lexer 
import Control.Exception (evaluate, try, ErrorCall(..), SomeException(..) )

import Data.List (isPrefixOf, isInfixOf)
-- Tests for every possible Token/statements as group of tokens


import Control.Exception
import Control.Monad



-- Tests using code examples from the project statement

-- Test cases
lexerProjectExamplesCase :: Test
lexerProjectExamplesCase = TestList [
    TestCase (assertEqual "for (lexer \"x := 5; x := x - 1;\")"
                         [VarName "x", OpAssign, IntLit 5, Semicolon, VarName "x", OpAssign, VarName "x", OpSub, IntLit 1, Semicolon]
                         (lexer "x := 5; x := x - 1;")),
    TestCase (assertEqual "for (lexer \"if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;\")"
                         [KWIf, OpenParen, OpNot, BoolLit True, OpAnd, IntLit 2, OpLe, IntLit 5, OpEqBool, IntLit 3, OpEqInt, IntLit 4, CloseParen, KWThen, VarName "x", OpAssign, IntLit 1, Semicolon, KWElse, VarName "y", OpAssign, IntLit 2, Semicolon]
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
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpAdd, OpEqBool] (lexerVarName "y+=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpSub, OpEqBool] (lexerVarName "y-=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpMult, OpEqBool] (lexerVarName "y*=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpLe] (lexerVarName "y<=")),
    TestCase (assertEqual "Variable name with operators" [VarName "y",OpEqInt] (lexerVarName "y==")),
    TestCase (assertEqual "Variable name with operators" [VarName "y!",OpEqBool] (lexerVarName "y!=")), -- needs to throw error
    TestCase (assertEqual "Variable name with operators" [VarName "y&&"] (lexerVarName "y&&")), -- needs to throw error
    TestCase (assertEqual "Variable name with operators" [VarName "y||"] (lexerVarName "y||")),-- needs to throw error
    TestCase (assertEqual "Variable name with operators" [VarName "y_2_A_Z!"] (lexerVarName "y_2_A_Z!")), -- needs to throw error
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
    TestCase (assertEqual "Complex expression" [KWIf, VarName "x", OpLe, IntLit 10, KWThen, VarName "y", OpAssign, BoolLit True, Semicolon, KWElse, VarName "y", OpAssign, BoolLit False, Semicolon] (lexer "if x < 10 then y := True; else y := False;")), --needs to throm error, because we don use <, just <=
    TestCase (assertEqual "Complex expression" [KWIf, VarName "x", OpLe, IntLit 10, KWThen, VarName "y", OpAssign, BoolLit True, Semicolon, KWElse, VarName "y", OpAssign, BoolLit False, Semicolon] (lexer "if x <= 10 then y := True; else y := False;")), --no error, because we use <=
    TestCase (assertEqual "Complex expression" [KWIf,OpenParen,OpNot,BoolLit True,OpAnd,IntLit 2,OpLe,IntLit 5,OpEqBool,IntLit 3,OpEqInt,IntLit 4,CloseParen,KWThen,VarName "x",OpAssign,IntLit 1,Semicolon,KWElse,VarName "y",OpAssign,IntLit 2,Semicolon](lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;")),
   
    TestCase (assertEqual "Complex expression" [KWWhile,OpenParen,OpNot,OpenParen,VarName "i",OpEqInt,IntLit 1,CloseParen,CloseParen,KWDo,OpenParen,VarName "fact",OpAssign,VarName "fact",OpMult,VarName "i",Semicolon,VarName "i",OpAssign,VarName "i",OpSub,IntLit 1,Semicolon,CloseParen,Semicolon](lexer "while (not(i == 1)) do (fact := fact * i; i := i - 1;);")),
    TestCase (assertEqual "Complex expression" [VarName "x",OpAssign,IntLit 42,Semicolon,KWIf,VarName "x",OpLe,IntLit 43,KWThen,VarName "x",OpAssign,IntLit 1,Semicolon,KWElse,OpenParen,VarName "x",OpAssign,IntLit 33,Semicolon,VarName "x",OpAssign,VarName "x",OpAdd,IntLit 1,Semicolon,CloseParen] (lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)")),
    TestCase (assertEqual "Complex expression" [VarName "x",OpAssign,IntLit 2,Semicolon,VarName "y",OpAssign,OpenParen,VarName "x",OpSub,IntLit 3,CloseParen,OpMult,OpenParen,IntLit 4,OpAdd,IntLit 2,OpMult,IntLit 3,CloseParen,Semicolon,VarName "z",OpAssign,VarName "x",OpAdd,VarName "x",OpMult,OpenParen,IntLit 2,CloseParen,Semicolon] (lexer "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"))
    
    ]

-- Total only 5 errors in 30 Tried the rest of TestCases with assertEqual are True
-- The errors are supposed to be thrown, because we don't allow them in our language using the lexer
-- Main function to run tests
main :: IO ()
main = do
    let allTests = TestList [lexerVarNameTests, lexerIntLitTests, lexerProjectExamplesCase, lexerTests]

    runTestTTAndExit allTests