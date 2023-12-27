module Main where

import Test.HUnit
import ImperativeLanguage

-- Test Aexp creation
testAexpCreation :: Test
testAexpCreation = TestList [
    TestCase (assertEqual "for INTVAL 5" (INTVAL 5) (INTVAL 5)),
    TestCase (assertEqual "for VAR x" (VAR "x") (VAR "x")),
    TestCase (assertEqual "for ADD" (ADD (INTVAL 1) (INTVAL 2)) (ADD (INTVAL 1) (INTVAL 2))),
    TestCase (assertEqual "for SUB" (SUB (INTVAL 5) (INTVAL 3)) (SUB (INTVAL 5) (INTVAL 3))),
    TestCase (assertEqual "for MULT" (MULT (INTVAL 2) (INTVAL 4)) (MULT (INTVAL 2) (INTVAL 4)))
    ]

-- Test Bexp creation
testBexpCreation :: Test
testBexpCreation = TestList [
    TestCase (assertEqual "for TRU" TRU TRU),
    TestCase (assertEqual "for FALS" FALS FALS),
    TestCase (assertEqual "for EQU" (EQU (AEXPR (INTVAL 1)) (AEXPR (INTVAL 1))) (EQU (AEXPR (INTVAL 1)) (AEXPR (INTVAL 1)))),
    TestCase (assertEqual "for LE" (LE (INTVAL 1) (INTVAL 2)) (LE (INTVAL 1) (INTVAL 2))),
    TestCase (assertEqual "for NEG" (NEG TRU) (NEG TRU)),
    TestCase (assertEqual "for AND" (AND TRU FALS) (AND TRU FALS))
    ]

-- Test Stm creation
testStmCreation :: Test
testStmCreation = TestList [
    TestCase (assertEqual "for ASSIGN" (ASSIGN "x" (INTVAL 10)) (ASSIGN "x" (INTVAL 10))),
    TestCase (assertEqual "for SEQ" (SEQ (ASSIGN "x" (INTVAL 10)) (ASSIGN "y" (INTVAL 20))) (SEQ (ASSIGN "x" (INTVAL 10)) (ASSIGN "y" (INTVAL 20)))),
    TestCase (assertEqual "for IF" (IF TRU (ASSIGN "x" (INTVAL 1)) (ASSIGN "x" (INTVAL 0))) (IF TRU (ASSIGN "x" (INTVAL 1)) (ASSIGN "x" (INTVAL 0)))),
    TestCase (assertEqual "for WHILE" (WHILE (EQU (AEXPR (VAR "x")) (AEXPR (INTVAL 0))) (ASSIGN "x" (SUB (VAR "x") (INTVAL 1)))) (WHILE (EQU (AEXPR (VAR "x")) (AEXPR (INTVAL 0))) (ASSIGN "x" (SUB (VAR "x") (INTVAL 1)))))
    ]

-- Test CompExpr creation
testCompExprCreation :: Test
testCompExprCreation = TestList [
    TestCase (assertEqual "for AEXPR" (AEXPR (INTVAL 5)) (AEXPR (INTVAL 5))),
    TestCase (assertEqual "for BEXPR" (BEXPR TRU) (BEXPR TRU))
    ]

-- Main test runner
main = do
    
    let allTests = TestList [testAexpCreation, testBexpCreation, testStmCreation, testCompExprCreation]
    runTestTTAndExit allTests