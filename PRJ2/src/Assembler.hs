

{-Write an interpreter ( our Asselbler) for programs in the same machine, which given a list of instructions (type defined as Code, i.e. type Code = [Inst]), a stack (type defined as
    Stack) and that is initially empty, and a storage (type defined as State), runs the
    list of instructions returning as ouput an empty code list, a stack and the output
    values in the storage. This evaluation function must be declared as:
    run :: (Code, Stack, State) → (Code, Stack, State)
    
    An assembler is a type of computer program that takes in basic
     instructions and converts them into a pattern of bits that 
     the computer's processor can use to perform basic operations
    
    Part 1
-}

-- ATTENTION: If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,State "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- !!!!!!NEEDS TO OUTPUT "Run-time error", calling a function error $ "Run-time error"
-- when called with wrong configurations
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Assembler where


import MachineStructures

-- helper functions to deal with stack and state at same time
-- Needs to get the value for a given variable from the state and put the value on the stack
fetch :: String -> Stack -> State -> Either String Stack
fetch x stack state = case getVarValue x state of
                        Right val -> Right $ push val stack
                        Left errorMsg -> Left errorMsg


--TODO: change this to be deal in exec returning a Left with error message string
store :: String -> Stack -> State -> Either String (Stack, State)
store x stack state = case stack of
    value:rest -> Right (rest, insertOrUpdate x value state)

    _ -> Left "Run-time error: Invalid operation for store."



true :: Stack -> Either String Stack
true stack = case stack of
    stack -> Right (push TT stack)
    _ -> Left "Run-time error: Invalid operation for Tru."

false :: Stack -> Either String Stack
false stack = case stack of
    stack -> Right (push FF stack)
    _ -> Left "Run-time error: Invalid operation for Fals"
add :: Stack -> Either String Stack
add stack = case stack of
    (IntVal x):IntVal y:rest -> Right $ push (IntVal (x + y)) rest
    _ -> Left "Run-time error: insufficient values or type error for 'add'"

sub :: Stack -> Either String Stack
sub stack = case stack of  
    (IntVal x):IntVal y:rest -> Right $ push (IntVal (x - y)) rest
    _ -> Left "Run-time error: insufficient values or type error for for 'sub'"


mult :: Stack -> Either String Stack
mult stack = case stack of  
    (IntVal x):IntVal y:rest -> Right $ push (IntVal (x * y)) rest
    _ -> Left "Run-time error: insufficient values or type error for 'mult'"



-- helper functions to deal with conditional instructions
le :: Stack -> Either String Stack
le stack = case stack of
    IntVal x:IntVal y:rest -> Right $ push (if x < y then TT else FF) rest
    _ -> Left "Run-time error: insufficient values or invalid values for 'le'"

areComparable :: StackValue -> StackValue -> Bool
areComparable (IntVal _) (IntVal _) = True
areComparable TT TT = True
areComparable FF FF = True
areComparable TT FF= True
areComparable FF TT= True
areComparable _ _ = False



equ :: Stack -> Either String Stack
equ stack = case stack of
    x:y:rest | areComparable x y -> 
        Right $ push (if x == y then TT else FF) rest
    _ -> Left "Run-time error: insufficient values or incomparable types for 'equ'"


andInstr :: Stack -> Either String Stack
andInstr stack = case stack of
    a:b:rest | isBoolVal a && isBoolVal b -> 
        Right $ push (if a == TT && b == TT then TT else FF) rest
    _ -> Left "Run-time error: insufficient or invalid values for 'and'"


neg :: Stack -> Either String Stack
neg stack = case top stack of
    Just FF -> Right $ push TT (pop stack)
    Just TT -> Right $ push FF (pop stack)
    _ -> Left "Run-time error: Top of stack is not a boolean value for 'neg'"


branch :: Code -> Code -> Stack -> Either String Code
branch c1 c2 stack = case top stack of
    Just TT -> Right c1
    Just FF -> Right c2
    _       -> Left "Run-time error: Top of stack is not a boolean value"

noop:: Stack -> State -> (Stack, State)
noop stack state = (stack, state)
-- Loop defined as "a combination of other constructs, including the branch instruction and itself."
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

--TODO: IMPORTANTE: deal with runtime errors on all cases, Left on every case
-- TODO: on run to a more simpler general aproach, or on exec we need to deal when Instr is 
-- used with not enough values on the stack, invalid stack, invalid state, etc, 
-- invalid type of values to use on store etc

-- Transition function for assembler 
exec :: (Code, Stack, State) -> Either String (Code, Stack, State)

exec (instr:code, stack, state)
    | not (isValidCode (instr:code))   = Left "Invalid Code detected."
    | not (isValidStack stack) = Left "Invalid Stack state detected."
    | not (isValidState state) = Left "Invalid State/Store detected."
    | otherwise = 
    case instr of
        Push n -> Right (code, push (IntVal n) stack, state)
        Fetch x -> case fetch x stack state of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Store x -> case store x stack state of
              Right (newStack, newState) -> Right (code, newStack, newState)
              Left errMsg -> Left errMsg
        Tru -> case true stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Fals -> case false stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Add -> case add stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Sub -> case sub stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Mult -> case mult stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Branch c1 c2 -> case branch c1 c2 stack of
              Right newCode -> Right (newCode ++ code,pop stack, state)
              Left errMsg -> Left errMsg
        Loop c1 c2 -> Right (loop c1 c2 ++ code, stack, state)
        Noop -> let (nextStack, nextState) = noop stack state
                in Right (code, nextStack, nextState)
        Le -> case le stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Equ -> case equ stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        And -> case andInstr stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
           
        Neg -> case neg stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        _ -> Left "Instruction does not exist" -- Indeed we use GRDts, so maybe this is not necessary, but the project assigment asks for it
exec ([], stack, state) = Right ([], stack, state) -- No instructions to execute

exec (code, stack, state) 
    | not (isValidCode code)   = Left "Invalid Code detected."
    | not (isValidStack stack) = Left "Invalid Stack state detected."
    | not (isValidState state) = Left "Invalid State/Store detected."
        
-- run :: (Code, Stack, State) -> (Code, Stack, State)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No instructions left to run
run (code, stack, state) =
    case exec (code, stack, state) of
        Right result -> run result
        Left errorMsg -> error "Run-time error"