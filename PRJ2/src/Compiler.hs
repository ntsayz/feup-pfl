-- Part 2

{-

Define a compiler from a program in this small imperative language into a list of
machine instructions (as defined in part 1). The main compiler is function:
• compile :: [Stm] → Code


-}

module Compiler where


-- compile :: [Stm] → Code

-- -- two mandatory auxiliary functions which compile arithmetic and boolean expressions
-- compA :: Aexp -> Code
-- compA = undefined -- TODO

-- compB :: Bexp -> Code
-- compB = undefined -- TODO