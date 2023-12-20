module Main where
-- PFL 2023/24 - Haskell practical assignment quickstart

import Assembler (run)
import MachineStructures 

-- simple test program
testProgram :: (Code, Stack, State)
testProgram = 
    ( [ Push 10
      , Push 20
      , Add
      , Store "x" 
      , Fetch "x"
      , Push 5
      , Mult
      ], 
      [], 
      createEmptyState 
    )

main :: IO ()
main = do
    let (code, stack, state) = run testProgram
    putStrLn $ "Final Stack: " ++ show stack
    putStrLn $ "Final State: " ++ show state






