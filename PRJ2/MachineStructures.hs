-- PFL 2023/24 - Haskell practical assignment quickstart
-- Part 1
import qualified Data.Map as Map
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show


type Code = [Inst]
type Stack = [Integer]
type State = Map.Map String Integer

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = Map.empty

stack2Str :: Stack -> String
stack2Str = show

state2Str :: State -> String
state2Str st = "State: " ++ show st