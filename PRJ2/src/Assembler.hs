

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

-- to run the code using exec for every instruction for a givan code, stack and state
runState :: State -> Int


-- run :: (Code, Stack, State) -> (Code, Stack, State)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- No instructions left
