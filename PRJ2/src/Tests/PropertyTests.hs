
-- TODO: Add more tests and improve the existing ones

-- Property tests for the Assembler/Interpreter, Parser, Lexer, MachineStructures modules

-- Check this : 
-- Correção
-- O interpretador implementa correctamente a semântica
-- operacional:
-- e ⇒ n se e só se eval e = n
-- Prova: por indução estrutural sobre e.

-- For compiler:
-- Invariante
-- A execução de compile e acrescenta o valor de e ao topo
-- da pilha.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
module Main where
-- Do property tests for the main functions of of every source file ( The lexer, assembler, compiler etc)
import Parser
import Assembler
import Compiler

--Possible Property tests for the Assembler/Interpreter, Parser, Compiler modules