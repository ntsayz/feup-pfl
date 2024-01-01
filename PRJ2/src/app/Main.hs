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

main :: IO ()
main = do
    putStrLn $ "Final Stack: " 
    putStrLn $ "Final State: " 






