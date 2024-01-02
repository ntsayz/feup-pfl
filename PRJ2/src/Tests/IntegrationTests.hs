module Main where

import Assembler
import ImperativeLanguage
import MachineStructures
import Lexer
import Test.HUnit hiding (State)
import Test.HUnit.Text (runTestTTAndExit,runTestTT)
import Parser
import Compiler



-- integration tests
testAssemblerV2 :: Code -> (String, String)
testAssemblerV2 code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParserV2 :: String -> (String, String)
testParserV2 programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


testParse :: Test
testParse = TestList [
    TestCase $ assertEqual "Assign and decrement" ("", "x=4") (testParserV2 "x := 5; x := x - 1;"),
    TestCase $ assertEqual "Assign negative value" ("", "x=-2") (testParserV2 "x := 0 - 2;"),
    TestCase $ assertEqual "Complex if-then-else" ("", "y=2") (testParserV2 "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"),
    TestCase $ assertEqual "If-then-else with assignment" ("", "x=1") (testParserV2 "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"),
    TestCase $ assertEqual "If-then-else without parentheses" ("", "x=2") (testParserV2 "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"),
    TestCase $ assertEqual "If-then-else with additional assignment" ("", "x=2,z=4") (testParserV2 "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"),
    TestCase $ assertEqual "If-else with nested assignment" ("", "x=34,y=68") (testParserV2 "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"),
    TestCase $ assertEqual "If-then with nested assignment" ("", "x=34") (testParserV2 "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"),
    TestCase $ assertEqual "Complex boolean in if condition" ("", "x=1") (testParserV2 "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"),
    TestCase $ assertEqual "Nested boolean operations in if condition" ("", "x=2") (testParserV2 "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"),
    TestCase $ assertEqual "Assignments with arithmetic operations" ("", "x=2,y=-10,z=6") (testParserV2 "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"),
    TestCase $ assertEqual "While loop for factorial calculation" ("", "fact=3628800,i=1") (testParserV2 "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")
    ]

allParseTests:: Test
allParseTests = TestList [TestLabel "parseTests" testParse ]


main :: IO ()
main = do
    let allTests = TestList [allParseTests]
    runTestTTAndExit allTests