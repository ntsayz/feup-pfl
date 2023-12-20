{- Entity

    which takes the tokens from the lexer and returns a syntax tree based on a grammar. 
    The grammar is often expressed in a meta language such as Backus-Naur Form (BNF) ( or others). 
    The grammar is the language of languages and provides the rules and syntax.

  
  
   - Part 2
 -}


 {-
 
     Define a parser which transforms an imperative program represented as a string
into its corresponding representation in the Stm data (a list of statements Stm).
The parser2
function must have the following signature:
 
 
 -}
module Parser where

--  -- Define Abstract Syntax Tree, Concrete Syntax Tree or other kind of tree to use on the arithmetical and boolean expressions and other things
--  type Program = [Stm]

-- parse :: String -> Program
-- parse = undefined -- TODO
-- --Eg: parse = buildData . lexer
-- -- 