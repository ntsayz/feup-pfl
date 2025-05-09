
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module MachineStructures where
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Part 1


import qualified Data.Map as Map
import Data.List (intercalate, sortOn, delete)-- to use on stack2Str and state2Str



-- Using ! to make the data strict, so that it is evaluated when it is created(memory efficiency, performance, predictability)
-- specialy on pushing and fetching instructions
data Inst =
  Push !Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch !String | Store !String | Noop |
  Branch !Code !Code | Loop !Code !Code
  deriving Show

-- We use this for Unit and property tests
instance Eq Inst where
    (Push x) == (Push y) = x == y
    Add == Add = True
    Mult == Mult = True
    Sub == Sub = True
    Tru == Tru = True
    Fals == Fals = True
    Equ == Equ = True
    Le == Le = True
    And == And = True
    Neg == Neg = True
    Fetch x == Fetch y = x == y
    Store x == Store y = x == y
    Noop == Noop = True
    (Branch c1 d1) == (Branch c2 d2) = c1 == c2 && d1 == d2
    (Loop c1 d1) == (Loop c2 d2) = c1 == c2 && d1 == d2
    _ == _ = False




type Code = [Inst]
-- Using GADTs (Generalized Algebraic Data Types): Integers and Booleans ( booleans are represented as TT or FF)
data StackValue where
  IntVal :: !Integer -> StackValue
  TT :: StackValue
  FF :: StackValue

-- Here we are deriving Show to use on stack2Str to convert the stack to a string
instance Show StackValue where
  show (IntVal xs) = show xs
  show TT = show True
  show FF = show False
-- To compare two StackValues on Tests
instance Eq StackValue where
  (IntVal x) == (IntVal y) = x == y
  TT == TT = True
  FF == FF = True
  _ == _ = False


isBoolVal :: StackValue -> Bool
isBoolVal x = x == TT || x == FF

isIntVal:: StackValue -> Bool
isIntVal (IntVal _) = True
isIntVal _ = False

-- define ord for StackValue IntVal


-- With the GADT definition above, we define the Stack type to that can deal with Integers and Booleans on the same stack
type Stack = [StackValue]
-- Here we define operations on the stack ( LIFO)
push :: StackValue -> Stack -> Stack
push x ys = x:ys


pop :: Stack -> Stack
pop  (_:xs) = xs
pop _ = []

top :: Stack -> Maybe StackValue
top (x:_) = Just x
top _ = Nothing


createEmptyStack :: Stack
createEmptyStack = []

isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _ = False

stack2Str :: Stack -> String
stack2Str = intercalate "," . map show



-- State/storage using map for variable = value
-- State is  new type of Map (Map.Map String StackValue)
-- State/Storage uses StackValues (Integers and Booleans)
newtype State = State (Map.Map String StackValue) deriving (Eq) --Our State uses Map.Map String Integer Eq to compare two states on Tests



instance Show State where
  show (State st) =
    intercalate "," . map (\(k, v) -> k ++ "=" ++ show v) . sortOn fst $ Map.toList st



insertOrUpdate :: String -> StackValue -> State -> State
insertOrUpdate var val (State st) = State ( Map.insert var val st )

deleteVar :: String -> State -> State
deleteVar var (State st) = State ( Map.delete var st )

getVarValue :: String -> State -> Either String StackValue
getVarValue var (State st) =
  case Map.lookup var st of
    Just val -> Right val
    Nothing -> Left $ "Variable '" ++ var ++ "' not found in state."

createEmptyState :: State
createEmptyState = State Map.empty


state2Str :: State -> String
state2Str st =  show st


-- to ensure that stack, code and state are valid on execution
isValidStack :: Stack -> Bool
isValidStack _ = True  

isValidState :: State -> Bool
isValidState _ = True  

isValidCode :: Code -> Bool
isValidCode _ = True  

