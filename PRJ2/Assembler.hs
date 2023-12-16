

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




-- run :: (Code, Stack, State) -> (Code, Stack, State)
