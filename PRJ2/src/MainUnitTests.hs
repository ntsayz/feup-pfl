module MainUnitTests where

import Assembler
import MachineStructures
import Lexer
import Compiler
import ImperativeLanguage
import Parser
import Control.Exception (evaluate, try, ErrorCall(..), SomeException(..), handleJust  )
import Test.HUnit hiding (State)



-- Function to handle exceptions errors on tests that should throw errors eg: testAssembler [Push 1, Push 2, And]
assertThrows :: IO a -> String -> Assertion
assertThrows action errorMsg =
    handleJust isWanted (const $ return ()) $ do
        _ <- action
        assertFailure $ "Expected exception: " ++ errorMsg
  where
    isWanted :: SomeException -> Maybe ()
    isWanted _ = Just ()




testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


testingParse :: Test
testingParse = TestList [
    TestCase $ assertEqual "Assign and decrement" ("", "x=4") (testParser "x := 5; x := x - 1;"),
    TestCase $ assertEqual "Assign negative value" ("", "x=-2") (testParser "x := 0 - 2;"),
    TestCase $ assertEqual "Complex if-then-else" ("", "y=2") (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"),
    TestCase $ assertEqual "If-then-else with assignment" ("", "x=1") (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"),
    TestCase $ assertEqual "If-then-else without parentheses" ("", "x=2") (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"),
    TestCase $ assertEqual "If-then-else with additional assignment" ("", "x=2,z=4") (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"),
    TestCase $ assertEqual "If-else with nested assignment" ("", "x=34,y=68") (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"),
    TestCase $ assertEqual "If-then with nested assignment" ("", "x=34") (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"),
    TestCase $ assertEqual "Complex boolean in if condition" ("", "x=1") (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"),
    TestCase $ assertEqual "Nested boolean operations in if condition" ("", "x=2") (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"),
    TestCase $ assertEqual "Assignments with arithmetic operations" ("", "x=2,y=-10,z=6") (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"),
    TestCase $ assertEqual "While loop for factorial calculation" ("", "fact=3628800,i=1") (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")
    ]


testingAssembler :: Test
testingAssembler = TestList [
    TestCase $ assertEqual "Push and arithmetic operations"
        ("-10", "")
        (testAssembler [Push 10, Push 4, Push 3, Sub, Mult]),

    TestCase $ assertEqual "Boolean and store operations"
        ("", "a=3,someVar=False,var=True")
        (testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]),

    TestCase $ assertEqual "Fetch operation"
        ("False", "var=False")
        (testAssembler [Fals, Store "var", Fetch "var"]),

    TestCase $ assertEqual "Push and Boolean operations"
        ("False,True,-20", "")
        (testAssembler [Push (-20), Tru, Fals]),

    TestCase $ assertEqual "Negation operation"
        ("False,True,-20", "")
        (testAssembler [Push (-20), Tru, Tru, Neg]),

    TestCase $ assertEqual "Equality operation"
        ("False,-20", "")
        (testAssembler [Push (-20), Tru, Tru, Neg, Equ]),

    TestCase $ assertEqual "Less or equal operation"
        ("True", "")
        (testAssembler [Push (-20), Push (-21), Le]),

    TestCase $ assertEqual "Subtract and store operation"
        ("", "x=4")
        (testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"]),

    TestCase $ assertEqual "Loop with factorial calculation"
        ("", "fact=3628800,i=1")
        (testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]])


    ]


testAssemblerExceptions :: Test
testAssemblerExceptions = TestList [
    TestCase $ assertThrows (evaluate $ testAssembler [Push 1, Push 2, And]) "Run-time error",
    TestCase $ assertThrows (evaluate $ testAssembler [Tru, Tru, Store "y", Fetch "x", Tru]) "Run-time error"
    ]

allTeachersTests:: Test
allTeachersTests = TestList [TestLabel "parseTests" testingParse, TestLabel "assemblerTests" testingAssembler, TestLabel "assemblerExceptions" testAssemblerExceptions ]


main :: IO ()
main = do

    -- Tests using putStrLn

    putStrLn $ "Assembler test 1: " ++ show (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
    putStrLn $ "Assembler test 2: " ++ show (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
    putStrLn $ "Assembler test 3: " ++ show (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
    putStrLn $ "Assembler test 4: " ++ show (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
    putStrLn $ "Assembler test 5: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
    putStrLn $ "Assembler test 6: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
    putStrLn $ "Assembler test 7: " ++ show (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
    putStrLn $ "Assembler test 8: " ++ show (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
    putStrLn $ "Assembler test 9: " ++ show (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
    putStrLn $ "Parser test 1: " ++ show (testParser "x := 5; x := x - 1;" == ("","x=4"))
    putStrLn $ "Parser test 2: " ++ show (testParser "x := 0 - 2;" == ("","x=-2"))
    putStrLn $ "Parser test 3: " ++ show (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
    putStrLn $ "Parser test 4: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
    putStrLn $ "Parser test 5: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
    putStrLn $ "Parser test 6: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
    putStrLn $ "Parser test 7: " ++ show (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
    putStrLn $ "Parser test 8: " ++ show (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
    putStrLn $ "Parser test 9: " ++ show (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
    putStrLn $ "Parser test 10: " ++ show (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
    putStrLn $ "Parser test 11: " ++ show (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
    putStrLn $ "Parser test 12: " ++ show (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))
    -- putStrLn $ "Assembler error 1: " ++ show (testAssembler [Push 1,Push 2,And]) -- Should get Run-time error
    -- putStrLn $ "Assembler error 2: " ++ show (testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]) -- Should get Run-time error


    -- Here we: Test using TestCases and handling also the exceptions
    let allTests = TestList [allTeachersTests]
    counts <- runTestTT allTests

    putStrLn "Teachers Tests: TestParser, TestAssembler and TestAssemblerExceptions -> UnitTesting"
    putStrLn $ "Total Tests: " ++ show (cases counts)
    putStrLn $ "Total Tried UnitTests: " ++ show (tried counts)
    putStrLn $ "Total UniTests correct: " ++ show (cases counts - errors counts - failures counts)
    putStrLn $ "Total Errors: " ++ show (errors counts)
    putStrLn $ "Total Failures: " ++ show (failures counts)
    putStrLn $ "Total Errors and Failures: " ++ show (errors counts + failures counts)
