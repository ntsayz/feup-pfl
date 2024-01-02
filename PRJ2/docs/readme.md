# INTRODUÇÃO

Neste projeto temos como objectivo a implementação de um interpretador para uma linguagem de programação imperativa.

O trabalho encontra-se dividido em duas partes. Na primeira parte, implementamos um pequeno assembler, que serve como base para o nosso interpretador. Este assembler processa uma série de instruções e interage com uma pilha de avaliação e um armazenamento para executar operações de baixo nível.

Na segunda parte deste projeto, avançamos para a criação de um Compiler para a linguagem imperativa, juntamente com um Lexer, Parser. Esta fase consistiu em traduzir estruturas de alto nível, como expressões aritméticas e booleanas, instruções de controle de fluxo e loops, para um conjunto de instruções de baixo nível compreensíveis pelo nosso Assembler.



## IMPLEMENTAÇÃO - PARTE 1

`Inst`: Este é um tipo de dados algébrico que representa as instruções da máquina. Cada construtor no Inst encapsula uma operação ou comando distinto que a máquina pode executar.

~~~hs
data Inst = Push !Integer | Add | Mult | Sub | Tru | Fals | Equ ...
~~~


`Stack` : Uma lista de `StackValue`, onde o valor pode ser um número inteiro `(IntVal)` ou um booleano `(TT, FF)` . 

~~~hs
type Stack = [StackValue]
~~~

`State`: Definido como  `(Map.Map String StackValue)`. É essencialmente um map dos nomes das variáveis para seus valores, permitindo que a máquina armazene e recupere dados.

~~~
    STATE   {x=30;y=10}
~~~


##  Execução 

`exec`: Esta função recebe um tuplo `(Code, Stack, State)` e aplica a primeira instrução da lista à stack e ao state atuais.

~~~hs
exec :: (Code, Stack, State) -> Either String (Code, Stack, State)
~~~

`run`: Função recursiva que itera sobre o código, atualizando a stack e o state.
~~~hs
run ([], stack, state) = ([], stack, state)  
run (code, stack, state) =
    case exec (code, stack, state) of
        Right result -> run result
        Left errorMsg -> error "Run-time error"
~~~



##  Exemplos 

~~~hs
    -- Stack: [], State: {}
    Push 5,      -- [5]     {}
    Push 3,      -- [3, 5]  {}
    Add,         -- [8]     {}   (3 + 5)
    Store "x",   -- [],     {"x": 8}  
    Push 2,      -- [2, 8]  {"x": 8}
    Mult,        -- [16]    {"x": 8} (2 * 8)
    Store "y"    -- []      {"x": 8, "y": 16} 
~~~



## IMPLEMENTAÇÃO - PARTE 2

A segunda parte deste projeto envolve a implementação de um compilador para uma linguagem de programação imperativa, que traduz programas de alto nível para uma sequência de instruções de máquina. Esta fase é subdividida em quatro componentes principais: a definição da linguagem imperativa (`ImperativeLanguage`), a análise léxica (`Lexer`), a análise sintática (`Parser`) e o o processo de compilação (`Compiler`).


### ImperativeLanguage
Define uma Árvore Sintática Abstrata (AST) para a linguagem imperativa

~~~hs
data Aexp = INTVAL !Integer | VAR !String | ADD !Aexp !Aexp | ... -- Expressões Aritméticas
data Bexp = TRU | FALS | EQUINT !Aexp !Aexp | ... -- Expressões Booleanas
data Stm = ASSIGN !String !CompExpr | SEQ ![Stm] | IF !Bexp !Stm !Stm | ... -- Instruções

~~~

### Lexer

O Lexer é responsável por realizar a análise léxica do source code, transforma uma string de entrada em uma sequência de tokens. 

Cada token representa um elemento sintático  da linguagem, como literais numéricos, identificadores, keywords, operadores e delimitadores.

~~~hs
    -- Literals
    IntLit !Integer
    ...
    -- Identifiers
    | VarName !String
    .... 
    -- Keywords
    | KWIf
    ...
    -- Operators
    | OpAdd
    | OpSub
    ...
    -- Delimiters and Punctuators
    | Semicolon
    ...
~~~



~~~hs
lexer :: String -> [Token]
 

lexer "23 + 4 * 421" 
-- Resultado da Tokenização:
[ IntLit 23, OpAdd, IntLit 4, OpMult, IntLit 421 ]
~~~


### Parser

Com base nos tokens fornecidos pelo Lexer, o Parser constrói uma  AST. Esta AST reflete a estrutura gramatical do programa, organizando os tokens de forma hierárquica que facilita a compreensão do fluxo e da lógica do programa.



~~~hs
lexer "3 * (4 + 5)" 
-- [IntLit 3, OpMult, OpenParen, IntLit 4, OpAdd, IntLit 5, CloseParen]
parseAexp [IntLit 3, OpMult, OpenParen, IntLit 4, OpAdd, IntLit 5, CloseParen]
-- Resultado: MULT (INTVAL 3) (ADD (INTVAL 4) (INTVAL 5))

~~~


### Compiler

O  Compiler transforma um programa escrito na linguagem imperativa em uma sequência de instruções de máquina, conforme definido na Parte I.


- `compA` e `compB` são utilizadas para compilar expressões aritméticas e booleanas, respectivamente. Por exemplo, compA (ADD x y) gera uma sequência de instruções que somam os valores de x e y.

~~~hs
compA :: Aexp -> Code
compA (ADD x y) = compA y ++ compA x ++ [Add]
compA (SUB x y ) = compA y ++ compA x ++ [Sub]


compB :: Bexp -> Code
compB (EQUINT x y) = compA y ++ compA x ++ [Equ]
compB (LE x y) = compA y ++ compA x ++ [Le]
~~~

- A função `compile` itera sobre uma lista de instruções `(Stm)`, convertendo cada uma em código de máquina. 

~~~hs
-- Instrução: if x == 1 then y := 10 else y := 20
compile [IF (EQUINT (VAR "x") (INTVAL 1)) 
             (ASSIGN "y" (AEXPR (INTVAL 10))) 
             (ASSIGN "y" (AEXPR (INTVAL 20)))]
-- Resultado da Compilação:
[
    Fetch "x",
    Push 1,
    Equ,
    Branch [Push 10, Store "y"] [Push 20, Store "y"]
]

~~~

## Exemplo 

~~~hs
sourceCode :: String
sourceCode = "if x == 1 then y := 10 else y := 20"


-- Análise Léxica: Transformação da string em tokens

tokens :: [Token]
tokens = lexer sourceCode 
-- [KWIf, VarName "x", OpEqInt, IntLit 1, KWThen, VarName "y", OpAssign, IntLit 10, KWElse, VarName "y", OpAssign, IntLit 20]



-- Análise Sintática: Construção da AST

ast :: Stm
ast = parseStms tokens
-- IF (EQUINT (VAR "x") (INTVAL 1)) (ASSIGN "y" (AEXPR (INTVAL 10))) (ASSIGN "y" (AEXPR (INTVAL 20)))


-- Compilação: Conversão da AST em instruções de máquina

machineCode :: Code
machineCode = compile [ast]
-- [Fetch "x", Push 1, Equ, Branch [Push 10, Store "y"] [Push 20, Store "y"]]

~~~

## Testes
No desenvolvimento deste projeto, adotámos a metodologia Test Driven Development (TDD), realizando testes rigorosos antes mesmo de desenvolver as funcionalidades. Esta abordagem permitiu-nos identificar e corrigir falhas precocemente, garantindo o funcionamento adequado de todas as partes do interpretador. A implementação do TDD revelou-se uma estratégia eficaz, contribuindo significativamente para a robustez e confiabilidade do sistema final. 



#### Exemplos

~~~hs
--  =========     TESTES UNITÁRIOS     =========

-- verifica a tokenização de números inteiros 
...
testLexerIntLit :: Test
testLexerIntLit = TestCase $ do
    assertEqual "Tokenização de '123'" [IntLit 123] (lexer "123")
    assertEqual "Tokenização de '456'" [IntLit 456] (lexer "456")
...

-- verifica a tokenização de operadores
testLexerOperators :: Test
testLexerOperators = TestCase $ do
    assertEqual "Tokenização de '+'" [OpAdd] (lexer "+")
    assertEqual "Tokenização de '*'" [OpMult] (lexer "*")

--  =========     TESTES DE INTEGRAÇÃO     =========

testParse :: Test
testParse = TestList [
    ...
    TestCase $ assertEqual "Operações booleanas nested na condição if" ("", "x=2") (testParserV2 "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"),
TestCase $ assertEqual "Atribuições com operações aritméticas" ("", "x=2,y=-10,z=6") (testParserV2 "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"),
TestCase $ assertEqual "Laço while para cálculo de fatorial" ("", "fact=3628800,i=1") (testParserV2 "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")

]


--  =========     TESTES DE EXCEÇÃO     =========

-- testa lançamento de erros de execução quando ações inválidas são realizadas
testAssemblerExceptions :: Test
testAssemblerExceptions = TestList [
    TestCase $ assertThrows (evaluate $ testAssemblerV2 [Push 1, Push 2, And]) "Run-time error",
    TestCase $ assertThrows (evaluate $ testAssemblerV2 [Tru, Tru, Store "y", Fetch "x", Tru]) "Run-time error"
    ]

~~~


