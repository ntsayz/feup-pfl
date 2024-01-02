{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module Main where

import MachineStructures
import Assembler
import Test.HUnit hiding (State)
import Test.HUnit.Text (runTestTTAndExit)
import Data.Map as Map
import Data.List (isInfixOf)

import Control.Exception (try, evaluate, SomeException)

import Text.Read.Lex (expect)



snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
testLE :: Test
testLE = TestList [
    TestCase (assertEqual "Testing LE with 2 <= 3" expectedOutput1 output1),
    TestCase (assertEqual "Testing LE with 4 <= 4" expectedOutput2 output2),
    TestCase (assertEqual "Testing LE with 5 <= 4" expectedOutput3 output3),
    TestCase (assertEqual "Testing LE with 0 <= 0" expectedOutput4 output4),
    TestCase (assertEqual "Testing LE with 0 - 1 <= 5" expectedOutput5 output5)
  ]
  where
    input1 = ([Push 3, Push 2, Le], createEmptyStack, createEmptyState)
    output1 = snd3 $ run input1
    expectedOutput1 = [TT]

    input2 = ([Push 4, Push 4, Le], createEmptyStack, createEmptyState)
    output2 = snd3 $ run input2
    expectedOutput2 = [TT]

    input3 = ([Push 4, Push 5, Le], createEmptyStack, createEmptyState)
    output3 = snd3 $ run input3
    expectedOutput3 = [FF]

    input4 = ([Push 0, Push 0, Le], createEmptyStack, createEmptyState)
    output4 = snd3 $ run input4
    expectedOutput4 = [TT]

    input5 = ([Push 5, Push 1,Push 0, Sub, Le], createEmptyStack, createEmptyState)
    output5 = snd3 $ run input5
    expectedOutput5 = [TT]


testFetchLe :: Test
testFetchLe = TestCase $ do
    -- Test with x > 0
    let initialState1 = State (Map.fromList [("x", IntVal 5)])
    let finalResult1 = run ([Fetch "x", Push 0, Le], [], initialState1)
    assertEqual "Testing Fetch x > 0, Push 0, Le" [FF] (snd3 finalResult1)

    -- Test with x = 0
    let initialState2 = State (Map.fromList [("x", IntVal 0)])
    let finalResult2 = run ([Fetch "x", Push 0, Le], [], initialState2)
    assertEqual "Testing Fetch x = 0, Push 0, Le" [TT] (snd3 finalResult2)

    -- Test with x < 0
    let initialState3 = State (Map.fromList [("x", IntVal (-5))])
    let finalResult3 = run ([Fetch "x", Push 0, Le], [], initialState3)
    assertEqual "Testing Fetch x < 0, Push 0, Le" [TT] (snd3 finalResult3)


testEQ :: Test
testEQ = TestList [
    TestCase (assertEqual "Testing EQ with 2 == 2" expectedOutput1 output1),
    TestCase (assertEqual "Testing EQ with 2 != 3" expectedOutput2 output2),
    TestCase (assertEqual "Testing EQ with 3 != 4" expectedOutput3 output3),
    TestCase (assertEqual "Testing EQ with 0 == 0" expectedOutput4 output4)
  ]
  where
    input1 = ([Push 2, Push 2, Equ], createEmptyStack, createEmptyState)
    output1 = snd3 $ run input1
    expectedOutput1 = [TT]

    input2 = ([Push 3, Push 2, Equ], createEmptyStack, createEmptyState)
    output2 = snd3 $ run input2
    expectedOutput2 = [FF]

    input3 = ([Push 4, Push 3, Equ], createEmptyStack, createEmptyState)
    output3 = snd3 $ run input3
    expectedOutput3 = [FF]

    input4 = ([Push 0, Push 0, Equ], createEmptyStack, createEmptyState)
    output4 = snd3 $ run input4
    expectedOutput4 = [TT]

testAnd :: Test
testAnd = TestList [
    TestCase (assertEqual "Testing And with True and False" [FF] (snd3 $ run ([Tru, Fals, And], [], createEmptyState))),
    TestCase (assertEqual "Testing And with True and True" [TT] (snd3 $ run ([Tru, Tru, And], [], createEmptyState)))
    ]

testNeg :: Test
testNeg = TestList [
    TestCase (assertEqual "Testing Neg with True" [FF] (snd3 $ run ([Tru, Neg], [], createEmptyState))),
    TestCase (assertEqual "Testing Neg with False" [TT] (snd3 $ run ([Fals, Neg], [], createEmptyState)))
    ]

testFetch :: Test
testFetch = TestCase $ do
    let state = State (Map.fromList [("x", IntVal 10)])
    let stack = []
    let expectedStack = [IntVal 10]
    case fetch "x" stack state of
        Right fetchedStack -> assertEqual "fetch x" expectedStack fetchedStack
        Left errorMsg -> assertFailure $ "Unexpected error during fetch: " ++ errorMsg


testStore :: Test -- The helper funtion store function only deals with the state, exec then deals also with the stack doing pop
testStore = TestCase $ do
    let state = State Map.empty
    let stack = [IntVal 20]
    let expectedState = State (Map.fromList [("x", IntVal 20)])
    let expectedStack = []
    case store "x" stack state of
        Right (stackResult, stateResult) -> do
            assertEqual "store x" expectedState stateResult
            assertEqual "store x" expectedStack stackResult
        Left errorMsg -> assertFailure $ "Unexpected error during store: " ++ errorMsg


testAdd :: Test
testAdd = TestCase $ do
    let stack = [IntVal 5, IntVal 3]
    let expectedStack = [IntVal 8]
    case add stack of
            Right newStack -> assertEqual "add" expectedStack newStack
            Left errorMsg -> assertFailure $ "Unexpected error during add: " ++ errorMsg


testSub :: Test
testSub = TestCase $ do
    let stack = [IntVal 10, IntVal 4]
    let expectedStack = [IntVal 6]
    case sub stack of
            Right newStack -> assertEqual "sub" expectedStack newStack
            Left errorMsg -> assertFailure $ "Unexpected error during add: " ++ errorMsg

testMult :: Test
testMult = TestCase $ do
    let stack = [IntVal 7, IntVal 6]
    let expectedStack = [IntVal 42]
    case mult stack of
            Right newStack -> assertEqual "mult" expectedStack newStack
            Left errorMsg -> assertFailure $ "Unexpected error during add: " ++ errorMsg

-- Testing operations on exec function

testFetchStoreExec :: Test
testFetchStoreExec = TestCase $ do
    let initialStack = [IntVal 10]
    let initialCode = [Store "x", Fetch "x"]
    let initialState = State Map.empty
    let expectedStack = [IntVal 10] -- Fetch "x" should push the value of x (10) back onto the stack
    let expectedState = State (Map.fromList [("x", IntVal 10)]) -- Store "x" should have stored 10 in x

    -- Test Store "x"
    case exec (initialCode, initialStack, initialState) of
        Right (codeAfterStore, stackAfterStore, stateAfterStore) -> do
            -- Expected stack after Store "x" should be empty, and state should have "x" mapped to 10
            assertEqual "Store Exec" ([Fetch "x"], [], stateAfterStore) (codeAfterStore, stackAfterStore, stateAfterStore)

            -- Test Fetch "x"
            case exec (tail initialCode, stackAfterStore, stateAfterStore) of
                Right result -> assertEqual "Fetch Exec" ([], expectedStack, expectedState) result
                Left errorMsg -> assertFailure $ "Unexpected error on Fetch: " ++ errorMsg
        Left errorMsg -> assertFailure $ "Unexpected error on Store: " ++ errorMsg

testAddExec :: Test
testAddExec = TestCase $ do
    let initialStack = [IntVal 3, IntVal 2]
    let initialCode = [Add]
    let initialState = State Map.empty
    let expectedStack = [IntVal 5]

    case exec (initialCode, initialStack, initialState) of
        Right result -> assertEqual "Add Exec" ([], expectedStack, initialState) result
        Left errorMsg -> assertFailure $ "Unexpected error: " ++ errorMsg


testPushExec :: Test
testPushExec = TestCase $ do
    let initialStack = []
    let initialCode = [Push 5]
    let expectedCode = []
    let initialState = State Map.empty
    let expectedStack = [IntVal 5]

    case exec (initialCode, initialStack, initialState) of
        Right result -> assertEqual "Push Exec" (expectedCode, expectedStack, initialState) result
        Left errorMsg -> assertFailure $ "Unexpected error: " ++ errorMsg

testBranchExec :: Test
testBranchExec = TestCase $ do
    let initialState = State Map.empty
    let trueBranch = [Push 20]
    let falseBranch = [Push 30]
    -- Test True branch
    let executedResultTrue = run ([Tru, Branch trueBranch falseBranch], [], initialState)
    assertEqual "Testing Branch operation (True branch)" ([IntVal 20], initialState) (snd3 executedResultTrue, trd3 executedResultTrue)
    -- Test False branch
    let executedResultFalse = run ([Fals, Branch trueBranch falseBranch], [], initialState)
    assertEqual "Testing Branch operation (False branch)" ([IntVal 30], initialState) (snd3 executedResultFalse, trd3 executedResultFalse)

testNoopExec :: Test
testNoopExec = TestCase $ do
    let initialState = State Map.empty
    let initialStack = [IntVal 1, TT, FF]
    let executedResult = run ([Noop], initialStack, initialState)
    assertEqual "Testing Noop operation" (initialStack, initialState) (snd3 executedResult, trd3 executedResult)

testTruExec :: Test
testTruExec = TestCase $ do
    let initialState = State Map.empty
    let initialStack = []
    let expectedStack = [TT]
    let executedResult = run ([Tru], initialStack, initialState)
    assertEqual "Testing Tru operation" (expectedStack, initialState) (snd3 executedResult, trd3 executedResult)

testFalsExec :: Test
testFalsExec = TestCase $ do
    let initialState = State Map.empty
    let initialStack = []
    let expectedStack = [FF]
    let executedResult = run ([Fals], initialStack, initialState)
    assertEqual "Testing Fals operation" (expectedStack, initialState) (snd3 executedResult, trd3 executedResult)


-- Assignments tests for the Assembler/interpreter module
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)



-- Examples:
testAssemblerExamples :: Test
testAssemblerExamples = TestList [
    TestLabel "Arithmetic Operations Test" $ TestCase (assertEqual "Testing basic arithmetic operations" ("-10","") (testAssembler [Push 10,Push 4,Push 3,Sub,Mult])),
    TestLabel "Boolean and Store Operations Test" $ TestCase (assertEqual "Testing boolean and store operations" ("","a=3,someVar=False,var=True") (testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"])),
    TestLabel "Fetch After Store Test" $ TestCase (assertEqual "Testing fetch after store" ("False","var=False") (testAssembler [Fals,Store "var",Fetch "var"])),
    TestLabel "Stack Manipulation Test" $ TestCase (assertEqual "Testing stack manipulation" ("False,True,-20","") (testAssembler [Push (-20),Tru,Fals])),
    TestLabel "Negation Test" $ TestCase (assertEqual "Testing negation" ("False,True,-20","") (testAssembler [Push (-20),Tru,Tru,Neg])),
    TestLabel "Equality Test" $ TestCase (assertEqual "Testing equality" ("False,-20","") (testAssembler [Push (-20),Tru,Tru,Neg,Equ])),
    TestLabel "Less Than or Equal To Test" $ TestCase (assertEqual "Testing less than or equal to" ("True","") (testAssembler [Push (-20),Push (-21), Le])),
    TestLabel "Arithmetic with Store and Fetch Test" $ TestCase (assertEqual "Testing arithmetic with store and fetch" ("","x=4") (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"])),
    TestLabel "Loop for Factorial Calculation Test" $ TestCase (assertEqual "Testing loop for factorial calculation" ("","fact=3628800,i=1") (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]))
    ]


-- Function to test for runt-time error
assertErrorMessage :: String -> IO a -> Assertion
assertErrorMessage expectedMessage action = do
    result <- try action
    case result of
        Left ex ->
            let actualMessage = show (ex :: SomeException)
            in assertBool ("Expected error message not found: " ++ actualMessage) (expectedMessage `isInfixOf` actualMessage)
        Right _ -> assertFailure "Expected an exception, but none was thrown."


-- Modified testAssemblerv2 function so we can also test for run-time errors and not just get a string with error
testAssemblerv2 :: Code -> IO (Either String (String, String))
testAssemblerv2 code = do
    result <- try $ evaluate $ run (code, createEmptyStack, createEmptyState)
    case result of
        Right (_, stack, state) -> return $ Right (stack2Str stack, state2Str state)
        Left ex -> return $ Left $ show (ex :: SomeException)

testCase1 :: Test
testCase1 = TestCase $ do
    result <- testAssemblerv2 [Push 1, Push 2, And]
    case result of
        Right _ -> assertFailure "Expected a runtime error for invalid 'And' operation."
        Left errMsg -> assertBool "Expected runtime error not found" ("Run-time error" `isInfixOf` errMsg)

testCase2 :: Test
testCase2 = TestCase $ do
    result <- testAssemblerv2 [Tru, Tru, Store "y", Fetch "x", Tru]
    case result of
        Right _ -> assertFailure "Expected a runtime error for invalid 'Fetch' operation."
        Left errMsg -> assertBool "Expected runtime error not found" ("Run-time error" `isInfixOf` errMsg)



testSuiteRunTimeError :: Test
testSuiteRunTimeError = TestList [testCase1, testCase2]


main :: IO ()
main = do
    let allTests = TestList [testLE, testEQ, testFetch ,testAnd, testNeg,testFetch, testStore, testAdd, testSub, testMult,testPushExec, testFetchStoreExec, testAddExec, testBranchExec, testNoopExec, testTruExec, testFalsExec, testAssemblerExamples, testSuiteRunTimeError]
    runTestTTAndExit allTests



