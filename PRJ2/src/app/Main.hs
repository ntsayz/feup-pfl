module Main where
-- PFL 2023/24 - Haskell practical assignment quickstart

import Assembler 
import MachineStructures
import Parser
import Lexer
import Compiler



testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
main :: IO ()
main = do
    let (stack, state) = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
    putStrLn $ "Final Stack: " ++ show stack
    putStrLn $ "Final State: " ++ show state






