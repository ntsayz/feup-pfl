-- Part2
{-
    Define three datas in Haskell to represent expressions and statements of this imperative language:
• Aexp for arithmetic expressions
• Bexp for boolean expressions
• Stm for statements (x := a, sequence of
statements (instr1 ; instr2), if then else statements, and while loops)

    Here we construct our AST (Abstract Syntax Tree) that represents a given CFG ( context free grammar for our language)

    Examples: 
    Example 3 The compilation of x + 1 is [push − 1, fetch − x, add]. :
    The code generated for an arithmetic expression must ensure that the value of
the expression is on top of the evaluation stack when it has been computed.

    Example 4 The compilation of the factorial program
    y := 1; while ¬(x = 1) do (y := y ∗ x; x := x − 1)
    outputs the code:
    [push-1,store-y,loop([push-1,fetch-x,eq,neg],
    [fetch-x,fetch-y,mult,store-y,push-1,fetch-x,sub,store-x)])]
    -> loop c1 c2 will have: c1=[push-1,store-y,loop([push-1,fetch-x,eq,neg] , c2 = [fetch-x,fetch-y,mult,store-y,push-1,fetch-x,sub,store-x)]
-}

module ImperativeLanguage where
import MachineStructures


-- We use this  new data type because of Equ on Bexp can used on Aexp ( IntaVal) or Bexp ( TT or FF)
data CompExpr = 
    AEXPR !Aexp 
    | BEXPR !Bexp 
    deriving (Eq, Show)


data Aexp = INTVAL !Integer --Terminal term
          | VAR !String -- Terminal term: when we have a variable we need to fetch it from the stack
          | ADD !Aexp !Aexp 
          | SUB !Aexp !Aexp
          | MULT !Aexp !Aexp
          deriving (Eq, Show)

data Bexp = TRU -- Terminal term
          | FALS -- Terminal term
          | EQU !CompExpr !CompExpr  
          | LE !Aexp !Aexp
          | NEG !Bexp
          | AND !Bexp !Bexp
          deriving (Eq, Show)


-- Our SEQ Term needs to ensure  (instr1 ; instr2) ( using parenthesis with sequence of statements):Eg: "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
data Stm = ASSIGN !String !CompExpr -- Terminal terms from Aexp and Bexp
         | SEQ ![Stm]
         | IF !Bexp !Stm !Stm
         | WHILE !Bexp !Stm
         deriving (Eq, Show)

type Program = [Stm] -- A program is a list of statements

--type Program = [Stm] -- A program is a list of statements