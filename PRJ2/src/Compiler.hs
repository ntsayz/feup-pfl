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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}

module Compiler where

import ImperativeLanguage
    ( Aexp(..), Bexp(..), Stm(..), CompExpr(..), Program )
import MachineStructures

-- -- two mandatory auxiliary functions which compile arithmetic and boolean expressions
compA :: Aexp -> Code
compA (INTVAL  x) = [Push x]
compA (VAR x) = [Fetch x]
compA (ADD x y) = compA y ++ compA x ++ [Add]
compA (SUB x y ) = compA y ++ compA x ++ [Sub] -- in the correct order to have the right stack values to add x + y
compA (MULT x y) = compA y ++ compA x ++ [Mult]



-- We use this auxiliary function to help deal with the two types of expressions ( Aexp and Bexp)
compCompExpr :: CompExpr -> Code
compCompExpr (AEXPR x) = compA x
compCompExpr (BEXPR x) = compB x


compB :: Bexp -> Code
compB TRU = [Tru]
compB FALS = [Fals]
compB (EQU x y) = compCompExpr y ++ compCompExpr x ++ [Equ]
compB (LE x y) = compA y ++ compA x ++ [Le]
compB (NEG x) = compB x ++ [Neg]
compB (AND x y) = compB y ++ compB x ++ [And]




compile :: Program -> Code
compile [] = [] --base case
compile (stm:xs) = 
    case stm of
        ASSIGN var aexp -> compA aexp ++ [Store var] ++ compile xs
        SEQ stm1 stm2 -> compile [stm1] ++ compile [stm2] ++ compile xs
        IF bexp stm1 stm2 -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile xs
        WHILE bexp stm -> [Loop (compB bexp) (compile [stm])] ++ compile xs
