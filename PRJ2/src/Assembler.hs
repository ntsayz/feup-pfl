

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
module Assembler where


import MachineStructures

-- helper functions to deal with stack and state at same time
-- Needs to get the value for a given variable from the state and put the value on the stack
fetch :: String -> Stack -> State -> Stack
fetch x stack state = push (getVarValue x state) stack

store :: String -> Stack -> State -> State
store x stack state = insertOrUpdate x (top stack) state

add :: Stack -> Stack
add stack = let (IntVal x) = top stack
                (IntVal y) = top (pop stack)
            in push (IntVal (x+y))  (pop stack)
sub :: Stack -> Stack
sub stack = let (IntVal x) = top stack
                (IntVal y) = top (pop stack)
            in push (IntVal (x-y))  (pop stack)

mult :: Stack -> Stack
mult stack = let (IntVal x) = top stack
                 (IntVal y) = top (pop stack)
            in push (IntVal (x*y))  (pop stack)


-- helper functions to deal with conditional instructions

branch :: Code -> Code -> Stack -> Code
branch c1 c2 stack = 
    case top stack of
        TT -> c1
        FF -> c2
        _  -> error "Top of stack is not a boolean value"
noop:: Stack -> State -> (Stack, State)
noop stack state = (stack, state)
-- Loop defined as "a combination of other constructs, including the branch instruction and itself."
loop :: Code -> Code -> Code 
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

-- Transition function for assembler 
exec :: (Code, Stack, State) -> (Code, Stack, State)
exec (instr:code, stack, state) = 
    case instr of
        Push n ->   (code, push (IntVal n) stack, state)
        Fetch x ->  (code, fetch x stack state,state)
        Store x -> let updatedState = store x stack state -- Here, we ensure the top value of stack is stored in state before doung pop on the stack
               in (code, pop stack, updatedState)
        Tru -> (code, true stack, state)
        Fals -> (code, false stack, state)
        Le ->
          if length stack < 2
              then error "Runtime error: The stack does not have enough values for Le instruction"
              else let
                  (IntVal x) = top stack -- Extracting the top element
                  stack' = pop stack -- Remove the top element from the stack
                  (IntVal y) = top stack' -- Extracting the next element
                  stack'' = pop stack' -- Remove the next element from the stack
              in (code, push (if x <= y then TT else FF) stack'', state)
        Equ ->
            if length stack < 2
                then error "Run-time error: The stack does not have enough values for Equ instruction"
                else let
                    (IntVal x) = top stack
                    stack' = pop stack
                    (IntVal y) = top stack'
                    stack'' = pop stack'
                in (code, push (if x == y then TT else FF) stack'', state)
        Add -> (code, add stack, state)
        Sub -> (code, sub stack, state)
        Mult -> (code, mult stack, state)
        Branch c1 c2 -> let nextCodeToRun = branch c1 c2 stack
                        in (nextCodeToRun ++ code, pop stack, state) -- we add the result code of the conditional evaluation to the code to run, on the begining of Code
        Loop c1 c2 -> (loop c1 c2 ++ code, stack, state)
        Noop -> let (nextStack, nextState) = noop stack state
                in (code, nextStack, nextState)
        Equ -> --TODO
        Le -> --TODO
        And -> --TODO
        Neg -> --TODO
        otherwise -> error $ "Run-time error"
exec ([], stack, state) = ([], stack, state) -- No instructions to execute

runInstructions ((instr:rest), stack, state) = 
    let (newCode, newStack, newState) = exec ([instr], stack, state)
    in runInstructions (rest, newStack, newState)




-- run :: (Code, Stack, State) -> (Code, Stack, State)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No instructions left to run
run (code, stack, state) = run (exec (code, stack, state)) -- run the code using exec for every instruction for a givan code, stack and state
