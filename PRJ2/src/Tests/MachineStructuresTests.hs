
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module Main where

import MachineStructures

import Test.HUnit hiding (State)

import  Data.Map as Map

-- Other necessary imports...

-- The rest of your test suite code...


-- Unit tests for MachineStructures

testStackValueShow :: Test
testStackValueShow = TestList [
    TestCase (assertEqual "for (show (IntVal 42))," "42" (show (IntVal 42))),
    TestCase (assertEqual "for (show (TT))," "True" (show TT)),
    TestCase (assertEqual "for (show (FF))," "False" (show FF))
    ]


testStateShow :: Test
testStateShow = TestList [
    let state = State $ Map.fromList [("x", IntVal 3), ("y", IntVal (-1)), ("z", IntVal 5)]
    in TestCase (assertEqual "for (show state)," "x=3,y=-1,z=5" (show state))
    ]


testCreateEmptyStack :: Test
testCreateEmptyStack = TestCase (assertEqual "for createEmptyStack," [] createEmptyStack)



testCreateEmptyState :: Test
testCreateEmptyState = TestCase (assertEqual "for createEmptyState," (State Map.empty) createEmptyState)


testStack2Str :: Test
testStack2Str = TestList [
    let stack = [IntVal 42, TT, IntVal (-5)]
    in TestCase (assertEqual "for (stack2Str stack)," "42,True,-5" (stack2Str stack))
    ]


testState2Str :: Test
testState2Str = TestList [
    let state = State $ Map.fromList [("a", IntVal 1), ("b", IntVal 2), ("c", IntVal 3)]
    in TestCase (assertEqual "for (state2Str state)," "a=1,b=2,c=3" (state2Str state))
    ]




main = do
    resultsStackValueShow <- runTestTT testStackValueShow
    print resultsStackValueShow

    resultsStateShow <- runTestTT testStateShow
    print resultsStateShow

    resultsCreateEmptyStack <- runTestTT testCreateEmptyStack
    print resultsCreateEmptyStack

    resultsCreateEmptyState <- runTestTT testCreateEmptyState
    print resultsCreateEmptyState

    resultsStack2Str <- runTestTT testStack2Str
    print resultsStack2Str

    resultsState2Str <- runTestTT testState2Str
    print resultsState2Str

