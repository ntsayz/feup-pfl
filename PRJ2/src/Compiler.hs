-- Part 2

{-

Define a compiler from a program in this small imperative language into a list of
machine instructions (as defined in part 1). The main compiler is function:
• compile :: [Stm] → Code


Traduz uma expressão numa sequência de instruções
Análogo a eval, mas gera instruções em vez valores
CHECK THIS:
Invariante
A execução de compile e acrescenta o valor de e ao topo
da pilha.



-}

module Compiler where


-- compile :: [Stm] → Code

-- -- two mandatory auxiliary functions which compile arithmetic and boolean expressions
-- compA :: Aexp -> Code
-- compA = undefined -- TODO

-- compB :: Bexp -> Code
-- compB = undefined -- TODO