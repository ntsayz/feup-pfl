{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module Main where

import MachineStructures 
import Assembler
import Test.HUnit hiding (State)
import Data.Map as Map
import Control.Exception (ErrorCall(..), evaluate, handle)




testLE :: Test
testLE = TestList [
    TestCase (assertEqual "Testing LE with 2 <= 3" expectedOutput1 output1),
    TestCase (assertEqual "Testing LE with 4 <= 4" expectedOutput2 output2),
    TestCase (assertEqual "Testing LE with 5 <= 4" expectedOutput3 output3),
    TestCase (assertEqual "Testing LE with 0 <= 0" expectedOutput4 output4)
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





testLEUnderflow :: Test
testLEUnderflow = TestList [
    TestCase $ assertError "Testing LE with one element on stack" (run ([Le], [IntVal 1], createEmptyState)),
    TestCase $ assertError "Testing LE with empty stack" (run ([Le], [], createEmptyState))
  ]

testEQUnderflow :: Test
testEQUnderflow = TestList [
    TestCase $ assertError "Testing EQ with one element on stack" (run ([Equ], [IntVal 1], createEmptyState)),
    TestCase $ assertError "Testing EQ with empty stack" (run ([Equ], [], createEmptyState))
  ]

-- HELPER FUNCTIONS

--  assert an error
assertError :: String -> a -> Assertion
assertError msg action = 
    handle handler $ do
        evaluate action
        assertFailure msg
  where
    handler :: ErrorCall -> IO ()
    handler _ = return ()

-- We use this function to extract the second element of a triple, which is the stack (Code, *Stack*, State)
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y


main :: IO ()
main = do
    resultsLE <- runTestTT testLE
    print resultsLE

    resultsEQ <- runTestTT testEQ
    print resultsEQ

    resultsLEUnderflow <- runTestTT testLEUnderflow
    print resultsLEUnderflow

    resultsEQUnderflow <- runTestTT testEQUnderflow
    print resultsEQUnderflow
