-- Our Tests for the compiler: Part2




module Main where

import Test.HUnit
import Compiler
import MachineStructures
import ImperativeLanguage
import ImperativeLanguage (Bexp(FALS))


-- Test compA function
testCompA :: Test
testCompA = TestList [
    TestCase (assertEqual "for INTVAL" [Push 5] (compA (INTVAL 5))),
    TestCase (assertEqual "for VAR" [Fetch "x"] (compA (VAR "x"))),
    TestCase (assertEqual "for ADD" [Push 2, Push 1, Add] (compA (ADD (INTVAL 1) (INTVAL 2)))),
    TestCase (assertEqual "for SUB" [Push 3, Push 5, Sub] (compA (SUB (INTVAL 5) (INTVAL 3)))),
    TestCase (assertEqual "for MULT" [Push 4, Push 2, Mult] (compA (MULT (INTVAL 2) (INTVAL 4)) )),
    TestCase (assertEqual "compA for complex Add" [Push 1,Fetch "a",Add,Push 2,Fetch "b",Add,Add] (compA (ADD (ADD (VAR "b") (INTVAL 2)) (ADD (VAR "a") (INTVAL 1))))),
    TestCase (assertEqual "compA for complex arithmetic expression" 
                       [Push 2, Push 5, Fetch "x", Add, Mult] 
                       (compA (MULT (ADD (VAR "x") (INTVAL 5)) (INTVAL 2)))),
    TestCase (assertEqual "compA for nested Mult expression"
  [Push 1, Push 2, Mult, Fetch "x", Push 3, Mult, Mult] -- Expected instructions
  (compA (MULT (MULT (INTVAL 3) (VAR "x")) (MULT (INTVAL 2) (INTVAL 1))))) -- Expression to compile,
    -- TestCase (assertEqual "compA for complex Sub" [Push 1, Push 2, Sub, Fetch "y", Sub] (compA (SUB (SUB (VAR "y") (INTVAL 2)) (INTVAL 1)))),
    -- TestCase (assertEqual "compA for Add with nested Mult" [Push 2, Push 3, Mult, Fetch "z", Add] (compA (ADD (VAR "z") (MULT (INTVAL 3) (INTVAL 2))))),
    -- TestCase (assertEqual "compA for nested Add and Sub" [Push 3, Push 4, Add, Fetch "w", Sub, Push 5, Add] (compA (ADD (SUB (VAR "w") (ADD (INTVAL 4) (INTVAL 3))) (INTVAL 5))))
    ]

-- Test compB function
testCompB :: Test
testCompB = TestList [
    TestCase (assertEqual "for TRU" [Tru] (compB TRU)),
    TestCase (assertEqual "for FALS" [Fals] (compB FALS)),
    TestCase (assertEqual "for EQU" [Push 1, Push 1, Equ] (compB (EQU (AEXPR (INTVAL 1)) (AEXPR (INTVAL 1))))),
    TestCase (assertEqual "for LE" [Push 2, Push 1, Le] (compB (LE (INTVAL 1) (INTVAL 2)))),
    TestCase (assertEqual "for NEG" [Tru, Neg] (compB (NEG TRU))),
    TestCase (assertEqual "for AND" [Fals, Tru, And] (compB (AND TRU FALS))),
    TestCase (assertEqual "compB for complex boolean expression" 
                       [Tru, Fetch "x", Equ, Neg] 
                       (compB (NEG (EQU (AEXPR (VAR "x")) (BEXPR TRU)))))

    -- TestCase (assertEqual "compB for complex Equ" [Push 1, Fetch "x",Equ ,Push 2, Fetch "y", Equ, Equ] (compB (EQU (EQU (VAR "y") (INTVAL 2) (EQU (VAR "x") (INTVAL 1)))))),
    -- TestCase (assertEqual "compB for nested And and Neg" [Fetch "x", Neg, Fetch "y", And] (compB (AND (NEG (VAR "x")) (VAR "y")))),
    -- TestCase (assertEqual "compB for And with Equ" [Push 1, Fetch "x", Equ, Tru, And] (compB (AND (EQU (VAR "x") (INTVAL 1)) TRU))),
    -- TestCase (assertEqual "compB for complex Neg" [Push 1, Fetch "z", Equ, Neg] (compB (NEG (EQU (VAR "z") (INTVAL 1))))),
    -- TestCase (assertEqual "compB for nested Le and And" [Push 1, Fetch "w", Le, Fals, And] (compB (AND (LE (VAR "w") (INTVAL 1)) FALS)))
    ]

-- Test compCompExpr function
testCompCompExpr :: Test
testCompCompExpr = TestList [
    TestCase (assertEqual "for AEXPR" [Push 5] (compCompExpr (AEXPR (INTVAL 5)))),
    TestCase (assertEqual "for BEXPR" [Tru] (compCompExpr (BEXPR TRU))),
    TestCase (assertEqual "for complex AEXPR with Add" [Push 2, Push 3, Add] (compCompExpr (AEXPR (ADD (INTVAL 3) (INTVAL 2))))),
    TestCase (assertEqual "for complex AEXPR with Sub" [Push 10, Push 5, Sub] (compCompExpr (AEXPR (SUB (INTVAL 5) (INTVAL 10))))),
    TestCase (assertEqual "for complex BEXPR with And" [Tru, Fals, And] (compCompExpr (BEXPR (AND FALS TRU)))),
    TestCase (assertEqual "for complex BEXPR with nested Equ" [Push 0,Push 1,Push 1,Equ,Equ] (compCompExpr (BEXPR (EQU (BEXPR (EQU (AEXPR (INTVAL 1)) (AEXPR (INTVAL 1)))) (AEXPR (INTVAL 0))))))
    ]

-- Test compile function
testCompile :: Test
testCompile = TestList [
    TestCase (assertEqual "for ASSIGN" [Push 10, Store "x"] (compile [ASSIGN "x" (INTVAL 10)])),
    TestCase (assertEqual "for SEQ" [Push 10, Store "x", Push 20, Store "y"] (compile [SEQ (ASSIGN "x" (INTVAL 10)) (ASSIGN "y" (INTVAL 20))])),
    TestCase (assertEqual "for IF" [Tru, Branch [Push 1, Store "x"] [Push 0, Store "x"]] (compile [IF TRU (ASSIGN "x" (INTVAL 1)) (ASSIGN "x" (INTVAL 0))])),
    TestCase (assertEqual "for WHILE" [Loop [Push 0, Fetch "x", Equ] [Push 1, Fetch "x", Sub, Store "x"]] (compile [WHILE (EQU (AEXPR (VAR "x")) (AEXPR (INTVAL 0))) (ASSIGN "x" (SUB (VAR "x") (INTVAL 1)))])),
    TestCase (assertEqual "compile for ASSIGN statement" 
                              [Push 2, Fetch "y", Add, Store "x"] 
                              (compile [ASSIGN "x" (ADD (VAR "y") (INTVAL 2))])),
    TestCase (assertEqual "compile for SEQ of statements" 
                           [Push 1, Store "x", Push 2, Fetch "x", Add, Store "y"] 
                           (compile [SEQ (ASSIGN "x" (INTVAL 1)) (ASSIGN "y" (ADD (VAR "x") (INTVAL 2)))])),
    TestCase (assertEqual "compile for IF statement" 
                          [Push 5, Fetch "x", Le, Branch [Push 2, Store "y"] [Push 3, Store "y"]] 
                          (compile [IF (LE (VAR "x") (INTVAL 5)) (ASSIGN "y" (INTVAL 2)) (ASSIGN "y" (INTVAL 3))])),
    TestCase (assertEqual "compile for WHILE loop" 
                             [Loop [Push 3, Fetch "x", Equ, Neg] [Push 1, Fetch "x", Add, Store "x"]] 
                             (compile [WHILE (NEG (EQU (AEXPR (VAR "x")) (AEXPR (INTVAL 3)))) (ASSIGN "x" (ADD (VAR "x") (INTVAL 1)))]))

    -- TestCase (assertEqual "compile for multiple nested IFs" [TRU, Branch [Push 1, Store "a"] [FALS, Branch [Push 2, Store "a"] [TRU, Branch [Push 3, Store "a"] [Push 4, Store "a"]]], Push 5, Store "b"] (compile [IF TT (ASSIGN "a" (INTVAL 1)) (IF FF (ASSIGN "a" (INTVAL 2)) (IF TT (ASSIGN "a" (INTVAL 3)) (ASSIGN "a" (INTVAL 4)))), ASSIGN "b" (INTVAL 5)])),
    -- TestCase (assertEqual "compile for nested WHILE inside IF" [Fetch "x", Push 10, Le, Branch [Loop [Fetch "y", Push 5, Le] [Fetch "y", Push 1, Add, Store "y"], Noop] [Push 100, Store "z"]] (compile [IF (LE (VAR "x") (INTVAL 10)) (WHILE (LE (VAR "y") (INTVAL 5)) (ASSIGN "y" (ADD (VAR "y") (INTVAL 1)))) (ASSIGN "z" (INTVAL 100))])),
    -- TestCase (assertEqual "compile for complex SEQ with arithmetic and boolean" [Push 10, Store "x", Fetch "x", Push 20, Add, Store "y", Push 0, Store "z", Loop [Fetch "z", Fetch "y", Le] [Fetch "z", Push 1, Add, Store "z"]] (compile [ASSIGN "x" (INTVAL 10), ASSIGN "y" (ADD (VAR "x") (INTVAL 20)), ASSIGN "z" (INTVAL 0), WHILE (LE (VAR "z") (VAR "y")) (ASSIGN "z" (ADD (VAR "z") (INTVAL 1)))])),
    -- TestCase (assertEqual "compile for SEQ with nested loops" [Loop [Fetch "i", Push 1, Le] [Loop [Fetch "j", Push 10, Le] [Fetch "j", Push 1, Add, Store "j"], Fetch "i", Push 1, Add, Store "i"]] (compile [WHILE (LE (VAR "i") (INTVAL 1)) (WHILE (LE (VAR "j") (INTVAL 10)) (ASSIGN "j" (ADD (VAR "j") (INTVAL 1))), ASSIGN "i" (ADD (VAR "i") (INTVAL 1)))])),
    -- TestCase (assertEqual "compile for IF with complex boolean and assignment" [Fetch "x", Push 5, Le, Neg, Branch [Fetch "x", Push 10, Mult, Store "x"] [Fetch "x", Push 2, Add, Store "x"]] (compile [IF (NEG (LE (VAR "x") (INTVAL 5))) (ASSIGN "x" (MULT (VAR "x") (INTVAL 10))) (ASSIGN "x" (ADD (VAR "x") (INTVAL 2)))])),
    -- TestCase (assertEqual "compile for nested IF" [TRU, Branch [Push 1, Store "x"] [FALS, Branch [Push 0, Store "x"] [Push 2, Store "x"]]] (compile [IF TT (ASSIGN "x" (INTVAL 1)) (IF FF (ASSIGN "x" (INTVAL 0)) (ASSIGN "x" (INTVAL 2)))])),
    -- TestCase (assertEqual "compile for complex WHILE" [Loop [Fetch "x", Push 1, Le] [Fetch "x", Push 1, Add, Store "x"]] (compile [WHILE (LE (VAR "x") (INTVAL 1)) (ASSIGN "x" (ADD (VAR "x") (INTVAL 1)))])),
    -- TestCase (assertEqual "compile for SEQ with multiple statements" [Push 10, Store "x", Push 5, Store "y", Fetch "x", Fetch "y", Add, Store "z"] (compile [ASSIGN "x" (INTVAL 10), ASSIGN "y" (INTVAL 5), ASSIGN "z" (ADD (VAR "x") (VAR "y"))])),
    -- TestCase (assertEqual "compile for WHILE with nested IF" [Loop [Fetch "x", Push 0, Le] [Fetch "x", Push 1, EQU, Branch [Push 1, Store "y"] [Push 0, Store "y"], Fetch "x", Push 1, Sub, Store "x"]] (compile [WHILE (LE (VAR "x") (INTVAL 0)) (IF (EQU (VAR "x") (INTVAL 1)) (ASSIGN "y" (INTVAL 1)) (ASSIGN "y" (INTVAL 0)), ASSIGN "x" (SUB (VAR "x") (INTVAL 1)))]))


    ]

-- Main test runner
main = do
    let allTests = TestList [testCompA, testCompB, testCompCompExpr, testCompile]
    runTestTTAndExit allTests
