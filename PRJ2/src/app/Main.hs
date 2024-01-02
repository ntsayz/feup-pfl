module Main where
-- PFL 2023/24 - Haskell practical assignment quickstart

import Assembler 
import MachineStructures
import Parser
import Lexer
import Compiler



-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, state2Str state)
--   where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


-- testAssembler :: Code -> (String, String)
-- testAssembler code = (stack2Str stack, state2Str state)
--   where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
{--
main :: IO ()
main = do
    putStrLn $ "Final Stack: " 
    putStrLn $ "Final State: " 

--}

{--
pp :: Code
pp = [
    Push 5, Push 3, Add, 
    Push 10, Push 2, Sub, 
    Push 2, Push 3, Mult, 
    
    Store "x", 
    Fetch "x", 
    Push 1, Add, 

    Tru, 
    Fals,
    And, 
    Neg, 

    Push 7,
    Le,
    Equ]
--}


part_one_example :: Code
part_one_example = [
    -- Stack: [], : {}
    Push 5,      -- Stack: [5], {}
    Push 3,      -- Stack: [3, 5], {}
    Add,         -- Stack: [8], {}   (3 + 5)
    Store "x",   -- Stack: [], {"x": 8} 

    Push 10,     -- Stack: [10],  {"x": 8}
    Push 2,      -- Stack: [2, 10], {"x": 8}
    Sub,         -- Stack: [8],  {"x": 8} (10 - 2)
    Push 2,      -- Stack: [2, 8],  {"x": 8}
    Push 3,      -- Stack: [3, 2, 8], {"x": 8}
    Mult,        -- Stack: [6, 8],  {"x": 8} (2 * 3)
    Fetch "x",   -- Stack: [8, 6, 8],  {"x": 8} 
    Add,         -- Stack: [14, 8], {"x": 8} (8 + 6)
    Push 7,      -- Stack: [7, 14, 8], {"x": 8}
    Le
    ]


main :: IO ()
main = do
    let (_, finalStack, finalState) = run (part_one_example, createEmptyStack, createEmptyState)

    putStrLn "Stack:"
    print $ stack2Str finalStack
    putStrLn "State:"
    print $ state2Str finalState




