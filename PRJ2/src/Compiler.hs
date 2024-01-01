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

-- The two mandatory auxiliary functions which compile arithmetic and boolean expressions
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
compB (EQUINT x y) = compA y ++ compA x ++ [Equ]
compB (AEXPRBOOL x) = compA x
compB (EQUBOOL x y) = compB y ++ compB x ++ [Equ]
compB (LE x y) = compA y ++ compA x ++ [Le]
compB (NEG x) = compB x ++ [Neg]
compB (AND x y) = compB y ++ compB x ++ [And]



-- To deal with Stm on of our imperative language 
compile :: Program -> Code
compile [] = [] --base case
compile (stm:xs) =
    case stm of
        ASSIGNBOOL var bexp -> compB bexp ++ [Store var] ++ compile xs
        ASSIGNINT var aexp -> compA aexp ++ [Store var] ++ compile xs
        SEQ [] -> compile xs -- Handle empty sequence
        SEQ (stm1:restOfStms) -> compile [stm1]  ++ compile restOfStms ++compile xs
        IF bexp stm1 stm2 -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile xs -- stm1 or stm1 can be SEQ
        WHILE bexp stm -> [Loop (compB bexp) (compile [stm])] ++ compile xs -- stm can be SEQ

    
