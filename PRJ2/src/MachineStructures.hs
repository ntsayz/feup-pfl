
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module MachineStructures where
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Part 1


import qualified Data.Map as Map
import Data.List (intercalate, sortOn)-- to use on stack2Str and state2Str



-- Using ! to make the data strict, so that it is evaluated when it is created(memory efficiency, performance, predictability)
-- specialy on pushing and fetching instructions
data Inst =
  Push !Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch !String | Store !String | Noop |
  Branch !Code !Code | Loop !Code !Code
  deriving Show


type Code = [Inst]
-- Using GADTs (Generalized Algebraic Data Types): Integers and Booleans ( booleans are represented as Integer)
data StackValue where
  IntVal :: Integer -> StackValue
  BoolVal :: Bool -> StackValue

-- Here we are deriving Show to use on stack2Str to convert the stack to a string
instance Show StackValue where
  show (IntVal xs) = show xs
  show (BoolVal xs) = show xs
-- To compare two StackValues on Tests
instance Eq StackValue where
    (IntVal x) == (IntVal y) = x == y
    (BoolVal x) == (BoolVal y) = x == y
    _ == _ = False





-- With the GADT definition above, we define the Stack type to that can deal with Integers and Booleans on the same stack
type Stack = [StackValue]




-- State/storage using map for variable = value
-- State is a new is a new type of Map (Map.Map String Integer)
newtype State = State (Map.Map String Integer) deriving (Eq) --Our State uses Map.Map String Integer Eq to compare two states on Tests



instance Show State where
  show (State st) =
    intercalate "," . map (\(k, v) -> k ++ "=" ++ show v) . sortOn fst $ Map.toList st


createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = State Map.empty

stack2Str :: Stack -> String
stack2Str = intercalate "," . map show


state2Str :: State -> String
state2Str st = "State: " ++ show st