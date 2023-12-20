

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
module Assembler where


import MachineStructures



-- Transition function for assembler 
exec :: (Code, Stack, State) -> (Code, Stack, State)
exec (instr:code, stack, state) = 
    case instr of
        Push n -> (code, push (IntVal n) stack, state)
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
              in (code, push (if y <= x then TT else FF) stack'', state)
        Equ ->
            if length stack < 2
                then error "Run-time error: The stack does not have enough values for Equ instruction"
                else let
                    (IntVal x) = top stack
                    stack' = pop stack
                    (IntVal y) = top stack'
                    stack'' = pop stack'
                in (code, push (if x == y then TT else FF) stack'', state)

-- to run the code using exec for every instruction for a givan code, stack and state
runInstructions :: (Code, Stack, State) -> (Code, Stack, State)
runInstructions ([], stack, state) = ([], stack, state)



-- run :: (Code, Stack, State) -> (Code, Stack, State)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No instructions left


