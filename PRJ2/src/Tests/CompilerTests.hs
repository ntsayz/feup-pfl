-- Our Tests for the compiler: Part2




module Main where

import Test.HUnit
import Compiler
import MachineStructures
import ImperativeLanguage



testCompA :: Test
testCompA = TestList [
    TestCase $ assertEqual "Simple integer value" [Push 42] (compA (INTVAL 42)),
    TestCase $ assertEqual "Variable fetch" [Fetch "x"] (compA (VAR "x")),
    TestCase $ assertEqual "Addition" [Push 2, Push 5, Add] (compA (ADD (INTVAL 5) (INTVAL 2))),
    TestCase $ assertEqual "Subtraction" [Push 3, Push 10, Sub] (compA (SUB (INTVAL 10) (INTVAL 3))),
    TestCase $ assertEqual "Multiplication" [Push 4, Push 6, Mult] (compA (MULT (INTVAL 6) (INTVAL 4))),
    TestCase $ assertEqual "Nested arithmetic expression" [Push 4,Push 2,Push 3,Add,Mult] (compA (MULT (ADD (INTVAL 3) (INTVAL 2)) (INTVAL 4)))
    ]


testCompB :: Test
testCompB = TestList [
    TestCase $ assertEqual "True literal" [Tru] (compB TRU),
    TestCase $ assertEqual "False literal" [Fals] (compB FALS),
    TestCase $ assertEqual "Equality of integers" [Push 5, Push 5, Equ] (compB (EQUINT (INTVAL 5) (INTVAL 5))),
    TestCase $ assertEqual "Less or equal" [Push 3, Push 4, Le] (compB (LE (INTVAL 4) (INTVAL 3))),
    TestCase $ assertEqual "Boolean negation" [Tru, Neg] (compB (NEG TRU)),
    TestCase $ assertEqual "Nested boolean expression" [Fals,Push 1,Push 2,Le,And] (compB (AND (LE (INTVAL 2) (INTVAL 1)) FALS))
    ]


testCompile :: Test
testCompile = TestList [
    TestCase $ assertEqual "Simple assignment (int)" [Push 5, Store "x"] (compile [ASSIGNINT "x" (INTVAL 5)]),
    TestCase $ assertEqual "Simple assignment (bool)" [Fals, Store "y"] (compile [ASSIGNBOOL "y" FALS]),
    TestCase $ assertEqual "Sequence of assignments" [Push 5, Store "x", Push 10, Store "y"] (compile [ASSIGNINT "x" (INTVAL 5), ASSIGNINT "y" (INTVAL 10)]),
    TestCase $ assertEqual "If statement" [Push 5, Push 10, Le, Branch [Push 1, Store "z"] [Push 2, Store "z"]] (compile [IF (LE (INTVAL 10) (INTVAL 5)) (ASSIGNINT "z" (INTVAL 1)) (ASSIGNINT "z" (INTVAL 2))]),
    TestCase $ assertEqual "While loop" [Loop [Push 10,Fetch "i",Le] [Push 1,Fetch "i",Sub,Store "i"]] (compile [WHILE (LE (VAR "i") (INTVAL 10)) (ASSIGNINT "i" (SUB (VAR "i") (INTVAL 1)))]),
    TestCase $ assertEqual "Nested sequence" [Push 5, Store "x", Push 6, Store "y", Push 1, Store "z"] (compile [SEQ [ASSIGNINT "x" (INTVAL 5), SEQ [ASSIGNINT "y" (INTVAL 6), ASSIGNINT "z" (INTVAL 1)]]])
    ]





-- Main test runner
main = do
    let allTests = TestList [testCompA, testCompB, testCompile]
    runTestTTAndExit allTests
