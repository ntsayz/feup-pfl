-- Our Tests for the compiler: Part2




module Main where

import Test.HUnit
import Compiler
import MachineStructures
import ImperativeLanguage


-- Test compA function
testCompA :: Test
testCompA = TestList [
    TestCase (assertEqual "for INTVAL" [Push 5] (compA (INTVAL 5))),
    TestCase (assertEqual "for VAR" [Fetch "x"] (compA (VAR "x"))),
    TestCase (assertEqual "for ADD" [Push 2, Push 1, Add] (compA (ADD (INTVAL 1) (INTVAL 2)))),
    TestCase (assertEqual "for SUB" [Push 3, Push 5, Sub] (compA (SUB (INTVAL 5) (INTVAL 3)))),
    TestCase (assertEqual "for MULT" [Push 4, Push 2, Mult] (compA (MULT (INTVAL 2) (INTVAL 4))))
    ]

-- Test compB function
testCompB :: Test
testCompB = TestList [
    TestCase (assertEqual "for TRU" [Tru] (compB TRU)),
    TestCase (assertEqual "for FALS" [Fals] (compB FALS)),
    TestCase (assertEqual "for EQU" [Push 1, Push 1, Equ] (compB (EQU (AEXPR (INTVAL 1)) (AEXPR (INTVAL 1))))),
    TestCase (assertEqual "for LE" [Push 2, Push 1, Le] (compB (LE (INTVAL 1) (INTVAL 2)))),
    TestCase (assertEqual "for NEG" [Tru, Neg] (compB (NEG TRU))),
    TestCase (assertEqual "for AND" [Fals, Tru, And] (compB (AND TRU FALS)))
    ]

-- Test compCompExpr function
testCompCompExpr :: Test
testCompCompExpr = TestList [
    TestCase (assertEqual "for AEXPR" [Push 5] (compCompExpr (AEXPR (INTVAL 5)))),
    TestCase (assertEqual "for BEXPR" [Tru] (compCompExpr (BEXPR TRU)))
    ]

-- Test compile function
testCompile :: Test
testCompile = TestList [
    TestCase (assertEqual "for ASSIGN" [Push 10, Store "x"] (compile [ASSIGN "x" (INTVAL 10)])),
    TestCase (assertEqual "for SEQ" [Push 10, Store "x", Push 20, Store "y"] (compile [SEQ (ASSIGN "x" (INTVAL 10)) (ASSIGN "y" (INTVAL 20))])),
    TestCase (assertEqual "for IF" [Tru, Branch [Push 1, Store "x"] [Push 0, Store "x"]] (compile [IF TRU (ASSIGN "x" (INTVAL 1)) (ASSIGN "x" (INTVAL 0))])),
    TestCase (assertEqual "for WHILE" [Loop [Push 0, Fetch "x", Equ] [Push 1, Fetch "x", Sub, Store "x"]] (compile [WHILE (EQU (AEXPR (VAR "x")) (AEXPR (INTVAL 0))) (ASSIGN "x" (SUB (VAR "x") (INTVAL 1)))]))
    ]

-- Main test runner
runCompilerTests :: IO ()
main = do 
    _ <- runTestTT $ TestList [testCompA, testCompB, testCompCompExpr, testCompile]
    return ()
