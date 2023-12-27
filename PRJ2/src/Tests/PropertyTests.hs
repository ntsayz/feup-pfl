
-- TODO: Add more tests and improve the existing ones

-- Property tests for the Assembler/Interpreter, Parser, Lexer, MachineStructures modules

-- Check this : 
-- Correção
-- O interpretador implementa correctamente a semântica
-- operacional:
-- e ⇒ n se e só se eval e = n
-- Prova: por indução estrutural sobre e.

-- For compiler:
-- Invariante
-- A execução de compile e acrescenta o valor de e ao topo
-- da pilha.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module Main where
-- Do property tests for the main functions of of every source file ( The lexer, assembler, compiler etc)
import MachineStructures
import Data.List (isInfixOf,sort)
import Data.Map as Map

prop_createEmptyStack :: Bool
prop_createEmptyStack = null createEmptyStack

prop_createEmptyState :: Bool
prop_createEmptyState = createEmptyState == State Map.empty

-- Here we check if the output represents a correct list of StackValues, invariant:  ensure that when a stack is converted to a string using stack2Str, the resulting string accurately reflects the contents of the stack.
prop_stack2Str :: Stack -> Bool
prop_stack2Str stack = all ((`isInfixOf` stackStr) . show) stack
  where stackStr = stack2Str stack
-- here we if variable-value pair in the state is correctly represented and the output is sorted by variable names, invariant: sorted order
prop_state2Str :: State -> Bool
prop_state2Str (State state) = 
  stateStr == expectedStr
  where 
    stateList = Map.toList state
    sortedStateList = sort stateList
    expectedStr = intercalate "," . map (\(k, v) -> k ++ "=" ++ show v) $ sortedStateList
    stateStr = state2Str (State state)

main :: IO ()
main = do
    quickCheck prop_createEmptyStack
    quickCheck prop_createEmptyState
    quickCheck prop_stack2Str
    -- quickCheck prop_state2Str (if you define a relevant property)
