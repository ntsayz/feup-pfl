# INTRODUÇÃO


Neste projeto temos como objectivo a implementação de um interpretador para uma linguagem de programação imperativa.

O trabalho encontra-se dividido em duas partes. 

Na primeira parte, implementamos um **assembler**, que serve como base para o nosso interpretador. Onde para isso, para a sua implementação foram criados os módulos: `Assembler.hs` e `MachineStructures` e para testar utilzando testes unitários (biblioteca Test.HUnit), foram criados os módulos: `AssemblerTests.hs` e `MachineStructuresTests.hs`.

Este assembler processa uma série de instruções e interage com uma pilha de avaliação e um armazenamento ( onde se guarda valores booleanos e inteiros associados a uma determinada variável) para executar operações de baixo nível.

Na segunda parte deste projeto, avançamos para a criação de um Compiler para a linguagem imperativa, definindo também a linguagem e criando um Lexer e Parser. 

Para este efeito, foram criados os módulos: `ImperativeLanguage.hs`, `Lexer.hs`, `Parser.hs` e `Compiler.hs` e para testar foram criados os módulos: `ImperativeLanguageTests.hs`, `LexerTests.hs`, `ParserTests.hs` e `CompilerTests.hs`. Tendo também sido criado o módulo `MainUniTests.hs` que tem acesso a todos os outros módulos e tem as funções testAssembler, TestParser e respectivos testes dados pelos professores usando testes unitários. 

Já o módulo `Main.hs` tem acesso a todos os outros módulos e tem as funções testAssembler, TestParser e respectivos testes dados os testes no formato dado inicialmente pelos professores. No módulo IntregrationTests temos os testes de integração dados pelos professores ao qual acedemos utilizando o comando `cabal test IntegrationTests` ou `cabal test ` que corre todos os testes unitários e de integração no diretório Tests.

Esta fase consistiu em traduzir estruturas de alto nível ( em código ), como expressões aritméticas e booleanas, instruções de controle de fluxo e loops, para um conjunto de instruções de baixo nível compreensíveis pelo nosso Assembler.

De seguida, dividindo em parte 1 e parte 2, abordaremos referindo os módulos criados, a implementação das principais componentes do projecto, dando exemplos concretos e mostrando alguns testes unitários .

**Para utilização correcta do projecto as seguintes informações são relevantes:**

- O projecto foi desenvolvido em Haskell, utilizando a ferramenta Cabal para gerir as dependências e compilar o código.
- Também permite não utilizar a ferramenta Cabal ( no caso de haver problemas com dependencias), bastando para isso, executar o comando `ghci` no directorio do projecto **src** e importar os módulos que se pretende utilizar, nomeadamente fazendo load do módulo Main.hs que tem acesso a todos os outros módulos e tem as funções testAssembler, TestParser e respectivos testes dados pelos professores.
- No terminal ghci fazer: ghci `:load Main.hs` -> ghci `main` -> Todos os testes correm com sucesso e temos acesso a todas as funções e módulos.

**Utilizando cabal:**

- Se utilizar **cabal**, tem uma versão da **Main**, módulo **MainUniTests.hs** que para além de ter acesso a tudo acima referido, tem também acesso  a uma versão de testes unitários dos testes facultados pelos professores com informação pertinente sobre os mesmos. 
- Primeiro para garantir que tem as dependências necessárias, dentro do directorio **src** execute o comando `cabal update` e de seguida `cabal install` .
- Fazer `cabal build` para compilar o código.
- Pode então fazer ghci `:load MainUniTests.hs` -> ghci `main` -> Todos os testes correm com sucesso e temos acesso a todas as funções e módulos.
- Fazer `cabal test` para correr todos os testes unitários dentro do directório Tests.
- Podemos também executar conjuntos de testes individuais (do diretório Tests) especificando o nome, como por exemplo: ```cabal test Integration Tests``` (UnitTests usando testes dados pelos professores).
- Os testes para o Lexer, como é suposto, têm exatamente **5 "Run-time errors"** ao testarmos o lexer com código inválido e que o lexer deve rejeitar.

- O log dos teste unitários costuma estar em: ```OurPCPath/src/dist-newstyle/build/x86_64-linux/ghc-9.2.5/PRJ2-0.1.0.0/t/IntegrationTests/test/file_with_tests.log```

## IMPLEMENTAÇÃO - PARTE I

No módulo `MachineStructures` definimos as estruturas de dados que representam a máquina de pilha e o seu estado: 

`Inst`: Este é um tipo de dados algébrico que representa as instruções da máquina. Cada construtor no Inst encapsula uma operação ou comando distinto que a máquina pode executar.

~~~hs
data Inst = Push !Integer | Add | Mult | Sub | Tru | Fals | Equ ...

type Code = [Inst]
~~~


`Stack` : Uma lista de `StackValue`, onde o valor pode ser um número inteiro `(IntVal)` ou um booleano `(TT, FF)` . 

~~~hs
data StackValue where
  IntVal :: !Integer -> StackValue
  TT :: StackValue
  FF :: StackValue
type Stack = [StackValue]
~~~

`State`: Definido como  `(Map.Map String StackValue)`. É essencialmente um map dos nomes das variáveis para seus valores, permitindo que a máquina armazene e recupere dados.

~~~
    STATE   {x=30, y=10, z=TT,...}
~~~


Ainda no módulo `MachineStructures` definimos funções auxiliares para definir e manipular uma stack, funções stack2Str , state2Str entre outras:

~~~hs
type Stack = [StackValue]
-- Here we define operations on the stack ( LIFO)
push :: StackValue -> Stack -> Stack
push x ys = x:ys
    ...
stack2Str :: Stack -> String
stack2Str = intercalate "," . map show
    ...

~~~
Módulo `Assembler`:

Aqui é onde o nosso interpretador é implementado utilizando o acima referido no módulo `MachineStructures`.

Várias funções auxiliares foram definidas para facilitar a implementação do interpretador, nomeadamente:

~~~hs
fetch :: String -> Stack -> State -> Either String Stack
fetch x stack state = case getVarValue x state of
                        Right val -> Right $ push val stack
                        Left errorMsg -> Left errorMsg

store :: String -> Stack -> State -> Either String (Stack, State)
store x stack state = case stack of
    value:rest -> Right (rest, insertOrUpdate x value state)

    _ -> Left "Run-time error: Invalid operation for store."



true :: Stack -> Either String Stack
true stack = case stack of
    stack -> Right (push TT stack)
    _ -> Left "Run-time error: Invalid operation for Tru."

false :: Stack -> Either String Stack
false stack = case stack of
    stack -> Right (push FF stack)
    _ -> Left "Run-time error: Invalid operation for Fals"
add :: Stack -> Either String Stack
add stack = case stack of
    (IntVal x):IntVal y:rest -> Right $ push (IntVal (x + y)) rest
    _ -> Left "Run-time error: insufficient values or type error for 'add'"

sub :: Stack -> Either String Stack
sub stack = case stack of  
    (IntVal x):IntVal y:rest -> Right $ push (IntVal (x - y)) rest
    _ -> Left "Run-time error: insufficient values or type error for for 'sub
~~~
Que depois são utilizadas para construir as funções `exec` e posteriormente `run`:
~~~hs
exec :: (Code, Stack, State) -> Either String (Code, Stack, State)

exec (instr:code, stack, state)
    | not (isValidCode (instr:code))   = Left "Invalid Code detected."
    | not (isValidStack stack) = Left "Invalid Stack state detected."
    | not (isValidState state) = Left "Invalid State/Store detected."
    | otherwise = 
    case instr of
        Push n -> Right (code, push (IntVal n) stack, state)
        Fetch x -> case fetch x stack state of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Store x -> case store x stack state of
              Right (newStack, newState) -> Right (code, newStack, newState)
              Left errMsg -> Left errMsg
        Tru -> case true stack of
              Right newStack -> Right (code, newStack, state)
              Left errMsg -> Left errMsg
        Fals -> case false stack of
              Right newStack -> Right (code, newStack, state)

                                ...

~~~

Onde  `exec`, para cada caso de instrução, se verifica se o código, a stack e o state são válidos e executa a instrução correspondente.

##  Execução 

`exec`: Esta função recebe um tuplo `(Code, Stack, State)` e aplica a primeira instrução da lista à stack e ao state ( o nosso store que guarda o valor das variáveis) atuais.

~~~hs
exec :: (Code, Stack, State) -> Either String (Code, Stack, State)
~~~

`run`: Função recursiva que itera sobre o código, atualizando a stack e o state.
~~~hs
run :: (Code, Stack, State) -> (Code, Stack, State)
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

~~~hs
"Testing basic arithmetic operations" ("-10",""): 
(testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
~~~

## IMPLEMENTAÇÃO - PARTE 2

A segunda parte deste projeto envolve a implementação de um compilador para uma linguagem de programação imperativa, que traduz programas de alto nível para uma sequência de instruções de máquina. Esta fase é subdividida em quatro componentes principais: a definição da linguagem imperativa (ImperativeLanguage), a **análise léxica (Lexer)**, a **análise sintática (Parser)** e o processo de **compilação** propriamente dito **(Compiler)** facilmente identificados pelos módulos com os mesmos nomes.


`Módulo ImperativeLanguage: Imperative Language`



~~~hs
data Aexp = INTVAL !Integer | VAR !String | ADD !Aexp !Aexp | ... -- Expressões Aritméticas
data Bexp = TRU | FALS | EQUINT !Aexp !Aexp | EQUBOOL !Bexp !Bexp| AEXPRBOOL !Aexp -- Expressões Booleanas
data Stm = ASSIGNBOOL !String !Bexp | ASSIGNINT !String !Aexp | SEQ ![Stm] | IF !Bexp !Stm !Stm | ... -- Instruções

type Program = [Stm] -- A program is a list of statements
                ... 
~~~


`Módulo Lexer: Lexer  `

O Lexer é responsável por realizar a análise léxica do source code, transforma uma string de entrada em uma sequência de tokens. 

Cada token representa um elemento sintático  da linguagem, como literais numéricos, identificadores, keywords, operadores e delimitadores.

~~~hs

data Token =
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
lexer (c:cs)
    | c == ' ' || c == '\n' || c == '\t' || c == '\r' = lexer cs -- new line, tab, space, etc.  are ignored
    | c == ';' = Semicolon : lexer cs
    | c == '(' = OpenParen : lexer cs
    | c == ')' = CloseParen : lexer cs
                ...

     | "True" `isPrefixOf` (c:cs) = BoolLit True : lexer (drop 4 (c:cs))
    | "False" `isPrefixOf` (c:cs) = BoolLit False : lexer (drop 5 (c:cs))
    | c == '=' = OpEqBool : lexer cs -- Equality for boolean expressions
    | c == ':' && not (null cs) && head cs == '=' = OpAssign : lexer (tail cs)
    | "not" `isPrefixOf` (c:cs) = OpNot : lexer (drop 3 (c:cs))
    | "while" `isPrefixOf` (c:cs) = KWWhile : lexer (drop 5 (c:cs))
                        ... 
 

lexer "23 + 4 * 421" 

-- Resultado da Tokenização:
[ IntLit 23, OpAdd, IntLit 4, OpMult, IntLit 421 ]

lexer "if x < 10 then y := True; else y := False;"
-- Resultado da Tokenização:
[KWIf, VarName "x", OpLe, IntLit 10, KWThen, VarName "y", OpAssign, BoolLit True, Semicolon, KWElse, VarName "y", OpAssign, BoolLit False, Semicolon]
~~~


`Módulo Parser: Parser`

Com base nos tokens fornecidos pelo Lexer, o Parser constrói uma Árvore Sintática Abstrata (AST). Esta AST reflete a estrutura gramatical do programa, organizando os tokens de forma hierárquica que facilita a compreensão do fluxo e da lógica do programa e respeita as regras de precedência dos operadores dadas da linguagem e que são requisitos do projecto.


Temos vários parsers auxiliares que são utilizados para construir o parser principal, nomeadamente:

 - Para **expressões aritméticas**: temos o parser principal `parseAexp` que utiliza outros parsers auxiliares tais como parseTerm, parseSum, parseProduct. 
 - A precedência dos operadores é respeitada, onde os vários parsers se chamam recursivamente numa determinada ordem, de forma a construir a AST que respeita a precedência dos operadores.


 ~~~hs

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
    case parseSum tokens of
        Just (aexp, []) -> Just (aexp, [])
        Just (aexp, restAfterAexp) -> Just (aexp, restAfterAexp)
        _incompleteParsing -> Nothing  -- Handle incomplete parse or error
                ...
parseSum :: [Token] -> Maybe (Aexp, [Token])
parseSum tokens =
    case parseProduct tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseSum' leftTerm restAfterLeftTerm
        Nothing -> Nothing
         ...

parseProduct :: [Token] -> Maybe (Aexp, [Token])
parseProduct tokens =
    case parseTerm tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseProduct' leftTerm restAfterLeftTerm
        Nothing -> Nothing

~~~

- Para **expressões booleanas**: temos o parser principal `parseBexp` que utiliza outros parsers auxiliares tais como parseBoolLiteral, parseBexpTerm, parseLe, parseEquality, parseNot parseEqBool e parseAnd .
- Tal como nas expressões aritméticas, a precedência dos operadores é respeitada, onde os vários parsers se chamam recursivamente e numa determinada ordem, de forma a construir a AST que respeita a precedência dos operadores booleanos:

 ~~~hs
    - Function that is the entry point for parsing Bexp expressions
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens =
   case parseAnd tokens of
        Just (bexp, []) -> Just (bexp, [])
        Just (bexp, restAfterBexp) -> Just (bexp, restAfterBexp)
        _incompleteParsing -> Nothing  -- Handle incomplete parse or error
                            ...
    
    parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
    case parseEqBool tokens of
        Just (leftTerm, restAfterLeftTerm) -> parseAnd' leftTerm restAfterLeftTerm
        Nothing -> Nothing
        

                    ...
                    ....

parseLe' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseLe' leftTerm (OpLe : tokens) =
    case parseBexpTerm tokens of
        Just (rightTerm, restAfterRightTerm) ->
            case (leftTerm, rightTerm) of
                (AEXPRBOOL leftAexp, AEXPRBOOL rightAexp) ->
                    parseLe' (LE leftAexp rightAexp) restAfterRightTerm
                noAEXPRBOOL_ -> Nothing
        Nothing -> Nothing


                ... 


parseBexpTerm :: [Token] -> Maybe (Bexp, [Token])
parseBexpTerm (OpenParen : restTokensAfterOpenParen) =
    case parseBexp restTokensAfterOpenParen of
        Just (bexp, CloseParen : restAfterBexp) -> Just (bexp, restAfterBexp)
        _noBexp -> Nothing

parseBexpTerm tokens =
    case parseAexp tokens of
        Just (aexp, restAfterAexp) -> Just (AEXPRBOOL aexp, restAfterAexp)
        Nothing -> parseSimpleBexp tokens



  ~~~

Tanto `parseAexp como parseBexp` são utilizados no parser principal `parseStm` que é responsável por construir a AST para as instruções da linguagem imperativa. Ambos conseguem lidar totalmente com expressões com e sem parênteses, respeitando a precedência dos operadores e os requisitos do projecto.


- Por fim, para **instruções ("statements")** temos o parser principal `parseStm` que lida com expressões Stm e é responsável por construir a AST final para as instruções da linguagem imperativa que depois é utilizada pelo compilador. 
- Este parser utiliza os parsers auxiliares `parseAssignment`, `parseIf`, `parseWhile`, `parseSeq`,`parseAexp` e `parseBexp`  .

Temos assim : 


~~~hs

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens =
    parseSeq tokens  <|> parseIf tokens <|> parseWhile tokens <|> parseAssignment tokens 
    
            ... 

-- parser for while loops
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile (KWWhile : tokens) = do
    (condExpr, restAfterCond) <- parseBexp tokens
    case restAfterCond of
        KWDo : restAfterDo -> do
            (loopBody, restAfterLoopBody) <- parseStm restAfterDo
            return (WHILE condExpr loopBody, restAfterLoopBody)
        _notDo -> Nothing -- Error handling if 'do' is missing
parseWhile _ = Nothing

            ... 

parseSeq :: [Token] -> Maybe (Stm, [Token])
parseSeq (OpenParen : tokens) = parseSeq' [] tokens
parseSeq _ = Nothing

parseSeq' :: [Stm] -> [Token] -> Maybe (Stm, [Token])
parseSeq' acc (CloseParen : Semicolon : restTokens) =  Just (SEQ (reverse acc), restTokens)
parseSeq' acc (CloseParen : KWElse: restTokens) = Just (SEQ (reverse acc), KWElse: restTokens) -- for ) with no ; after then
parseSeq' acc tokens = 
    case parseStm tokens of
        Just (stm, restAfterStm) -> parseSeq' (stm : acc) restAfterStm
        _ -> Nothing


~~~



**Integrando todos os parsers**, no final, temos o parser, `parse` que é a composição de `buildData` e `lexer` como é pedido no projecto:





~~~hs

buildData :: [Token] -> Program
buildData tokens = fst $ parseStms tokens

parseStms :: [Token] -> (Program, [Token])
parseStms [] = ([], [])
parseStms tokens =
    case parseStm tokens of
        Just (stm, restTokens) -> 
            let (stms, remaining) = parseStms restTokens
            in (stm : stms, remaining)
        Nothing -> ([], tokens)  




parse :: String -> Program
parse = buildData . lexer -- parse is the composition of builData and lexer has we have on project instructions




~~~

Este parser **cumpre todos os requistos** e regras do projecto, conseguindo lidar com todo o tipo de statements, expressões aritméticas e booleanas, respeitando a precedência dos operadores com e sem parênteses. Inclusive, fazer parsing de "nested" while loops, com if statements e ou com sequências de instruções ( SEQ [Stm]) que é de seguida correctamente compilado pelo nosso **Compilador** para linguagem de máquina que o **Assembler** consegue interpretar e executar.

### Exemplos

~~~hs


lexer "3 * (4 + 5)" 
-- [IntLit 3, OpMult, OpenParen, IntLit 4, OpAdd, IntLit 5, CloseParen]
parseAexp [IntLit 3, OpMult, OpenParen, IntLit 4, OpAdd, IntLit 5, CloseParen]
-- Resultado: MULT (INTVAL 3) (ADD (INTVAL 4) (INTVAL 5))


 parse "while (not(a == b)) do ( a := a + 1; b := b - 1; result := a + b; );"
 -- Resultado do Parsing:
 WHILE (NEG (EQUINT (VAR "a") (VAR "b")))
        (SEQ [ASSIGNINT "a" (ADD (VAR "a") (INTVAL 1)),
              ASSIGNINT "b" (SUB (VAR "b") (INTVAL 1)),
              ASSIGNINT "result" (ADD (VAR "a") (VAR "b"))])

parse "while (not(x == 5) and y = True) do ( while (z <= 20) do z := z + 1; x := x - 1; );"
-- Resultado do Parsing:
WHILE (AND (NEG (EQUINT (VAR "x") (INTVAL 5))) (EQUBOOL (AEXPRBOOL (VAR "y")) TRU))
                    (SEQ [WHILE (LE (VAR "z") (INTVAL 20)) (ASSIGNINT "z" (ADD (VAR "z") (INTVAL 1))), ASSIGNINT "x" (SUB (VAR "x") (INTVAL 1))])

TestCase $ assertEqual "While loop for factorial calculation" ("", "fact=3628800,i=1") (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")
-- Resultado é True
   
~~~


`Módulo Compiler: Compiler`


O  Compiler transforma um programa escrito na linguagem imperativa em uma sequência de instruções de máquina, conforme definido na Parte 1. O compilador utiliza funções auxiliares para lidar com expressões aritméticas e booleanas e "statements" convertendo-as em código executável pela máquina.


- compA e compB são utilizadas para compilar expressões aritméticas e booleanas, respectivamente. Por exemplo, compA (ADD x y) gera uma sequência de instruções que somam os valores de x e y.


- A função `compile` itera sobre uma lista de "statements" (Stm), convertendo cada uma em código de máquina. Dependendo do tipo de instrução, diferentes funções auxiliares são chamadas.


~~~hs

compA :: Aexp -> Code
compA (INTVAL  x) = [Push x]
compA (VAR x) = [Fetch x]
compA (ADD x y) = compA y ++ compA x ++ [Add]


            ...

compB :: Bexp -> Code
compB TRU = [Tru]
compB FALS = [Fals]
compB (EQUINT x y) = compA y ++ compA x ++ [Equ]
compB (AEXPRBOOL x) = compA x


        ... 

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


~~~


### Exemplos




~~~hs
-- Instrução: if x == 1 then y := 10 else y := 20
compile [IF (EQUINT (VAR "x") (INTVAL 1)) 
             (ASSIGN "y" (AEXPR (INTVAL 10))) 
             (ASSIGN "y" (AEXPR (INTVAL 20)))]
-- Resultado da Compilação:
[Fetch "x", Push 1, Equ, Branch [Push 10, Store "y"] [Push 20, Store "y"]]

TestCase $ assertEqual "While loop" [Loop [Push 10,Fetch "i",Le] [Push 1,Fetch "i",Sub,Store "i"]] (compile [WHILE (LE (VAR "i") (INTVAL 10)) (ASSIGNINT "i" (SUB (VAR "i") (INTVAL 1)))])
--Test é True:  Resultado da Compilação : 
[Loop [Push 10,Fetch "i",Le] [Push 1,Fetch "i",Sub,Store "i"]]

TestCase $ assertEqual "Nested sequence" [Push 5, Store "x", Push 6, Store "y", Push 1, Store "z"] (compile [SEQ [ASSIGNINT "x" (INTVAL 5), SEQ [ASSIGNINT "y" (INTVAL 6), ASSIGNINT "z" (INTVAL 1)]]])
--TEste é true : Resultado da Compilação :
[Push 5, Store "x", Push 6, Store "y", Push 1, Store "z"]

~~~


## Resultado final, integração de todos os módulos:

Utilizando todas as componentes explanadas acima, conseguimos compilar e executar programas escritos na linguagem imperativa, como por exemplo usando as funções `testAssembler` e `testParser` que são fornecidas pelos professores:

~~~hs
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
~~~

Temos:

~~~hs

testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


~~~


## Testes 

No desenvolvimento deste projeto, adotámos a metodologia Test Driven Development (TDD), realizando testes rigorosos por vezes antes mesmo de desenvolver as funcionalidades. Esta abordagem permitiu-nos identificar e corrigir falhas precocemente, garantindo o funcionamento adequado de todas as partes do interpretador/assembler, parser, lexer e compiler . A implementação do TDD revelou-se uma estratégia eficaz, contribuindo significativamente para a robustez e confiabilidade do sistema final.



#### Exemplos

~~~hs
--  =========     TESTES UNITÁRIOS     =========


-- ==================  TESTES DE Lexer  ==================
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

TestCase (assertEqual "Complex expression" [VarName "x",OpAssign,IntLit 42,Semicolon,KWIf,VarName "x",OpLe,IntLit 43,KWThen,VarName "x",OpAssign,IntLit 1,Semicolon,KWElse,OpenParen,VarName "x",OpAssign,IntLit 33,Semicolon,VarName "x",OpAssign,VarName "x",OpAdd,IntLit 1,Semicolon,CloseParen] (lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)")),
    TestCase (assertEqual "Complex expression" [VarName "x",OpAssign,IntLit 2,Semicolon,VarName "y",OpAssign,OpenParen,VarName "x",OpSub,IntLit 3,CloseParen,OpMult,OpenParen,IntLit 4,OpAdd,IntLit 2,OpMult,IntLit 3,CloseParen,Semicolon,VarName "z",OpAssign,VarName "x",OpAdd,VarName "x",OpMult,OpenParen,IntLit 2,CloseParen,Semicolon] (lexer "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"))
    





--  =========     TESTES DE INTEGRAÇÃO     =========

-- ==================  TESTES DE Parser  ==================
testParse :: Test
testParse = TestList [
    ...
    TestCase $ assertEqual "Operações booleanas nested na condição if" ("", "x=2") (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"),
    TestCase $ assertEqual "Atribuições com operações aritméticas" ("", "x=2,y=-10,z=6") (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"),
    TestCase $ assertEqual "Laço while para cálculo de fatorial" ("", "fact=3628800,i=1") (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")

]
-- =========     TESTES DE ASSEMBLER     =========
TestLabel "Arithmetic with Store and Fetch Test" $ TestCase (assertEqual "Testing arithmetic with store and fetch" ("","x=4") (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"])),
    TestLabel "Loop for Factorial Calculation Test" $ TestCase (assertEqual "Testing loop for factorial calculation" ("","fact=3628800,i=1") (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]))


-- =========     TESTES DE COMPILADOR     =========
TestCase $ assertEqual "Simple assignment (bool)" [Fals, Store "y"] (compile [ASSIGNBOOL "y" FALS]),
TestCase $ assertEqual "Sequence of assignments" [Push 5, Store "x", Push 10, Store "y"] (compile [ASSIGNINT "x" (INTVAL 5), ASSIGNINT "y" (INTVAL 10)]),



--  =========     TESTES DE EXCEÇÃO     =========

-- testa lançamento de erros de execução quando ações inválidas são realizadas
testAssemblerExceptions :: Test
testAssemblerExceptions = TestList [
    TestCase $ assertThrows (evaluate $ testAssembler [Push 1, Push 2, And]) "Run-time error",
    TestCase $ assertThrows (evaluate $ testAssembler [Tru, Tru, Store "y", Fetch "x", Tru]) "Run-time error"
   ]
~~~


#### Resultado de dos testes unitários e de integração

Informações nos ficheiros log dos testes unitários e de integração.

`MachineStructuresTests`:



~~~hs




Cases: 8  Tried: 8  Errors: 0  Failures: 0
Test suite MachineStructuresTests: PASS



~~~



`LexerTests`:

Como já referimos, é suposto o lexer rejeitar código inválido e lançar 5 erros de execução.

~~~hs


Cases: 32  Tried: 32  Errors: 5  Failures: 0
Test suite LexerTests: FAIL


### Error in:   0:14
lexer: unexpected character or string out of our imperative language
CallStack (from HasCallStack):
  error, called at Lexer.hs:105:19 in PRJ2-0.1.0.0-inplace:Lexer

Cases: 32  Tried: 15  Errors: 1  Failures: 0
                                            
### Error in:   0:15
lexer: unexpected character or string out of our imperative language
CallStack (from HasCallStack):
  error, called at Lexer.hs:105:19 in PRJ2-0.1.0.0-inplace:Lexer

Cases: 32  Tried: 16  Errors: 2  Failures: 0
                                            
### Error in:   0:16
lexer: unexpected character or string out of our imperative language
CallStack (from HasCallStack):
  error, called at Lexer.hs:105:19 in PRJ2-0.1.0.0-inplace:Lexer

Cases: 32  Tried: 17  Errors: 3  Failures: 0
                                            
### Error in:   0:17
lexer: unexpected character or string out of our imperative language
CallStack (from HasCallStack):
  error, called at Lexer.hs:105:19 in PRJ2-0.1.0.0-inplace:Lexer

~~~

`ParserTests`:

Aqui salientamos, que para todo o conjunto de funções que resultam no parser final `parse`, efetuamos 134 testes.

~~~hs

Cases: 134  Tried: 134  Errors: 0  Failures: 0
Test suite ParserTests: PASS

~~~

`CompilerTests`:

~~~hs

Cases: 18  Tried: 18  Errors: 0  Failures: 0
Test suite CompilerTests: PASS

~~~


`AssemblerTests`:

~~~hs

Cases: 37  Tried: 37  Errors: 0  Failures: 0
Test suite AssemblerTests: PASS

~~~

`IntegrationTests`:


~~~hs


Cases: 23  Tried: 23  Errors: 0  Failures: 0
Test suite IntegrationTests: PASS


~~~

## Conclusão

Este projeto foi uma oportunidade para aplicar os conhecimentos adquiridos na cadeira de Programação Funcional e Lógica, nomeadamente sobre a linguagem Haskell, ASTs, parsers, interpretação e compilação de programas.

A implementação do projeto foi um desafio, mas também uma oportunidade para aprofundar a forma como é possível definir e implementar linguagens de programação, bem como a sua interpretação e compilação. 

A implementação do TDD revelou-se uma estratégia eficaz, contribuindo significativamente para a robustez e confiabilidade do sistema final. Já que só avançávamos para a próxima fase do projeto quando todos os testes unitários e de integração passavam com sucesso. Por exemplo, só quando os parsers `parseAexp e parseBexp` e suas funções auxiliares passaram todos os testes unitários é que avançamos para o parser parseStm e só quando este passou todos os testes unitários é que avançamos para o parser parse.

É de salientar, que no total, para todos os módulos e contando com os testes de integração dados pelos professores, realizamos **252** testes unitários e de integração ( 134 só para o parser), que nos permitiram garantir o funcionamento adequado de todas as partes do interpretador/assembler, parser, lexer e compiler. 

Todas as funcionalidades, restrições e regras referentes às duas partes do projecto foram implementadas com sucesso, tendo sido testadas exaustivamente .

Tivemos alguma dificuldade em perceber como implementar o parser para expressões aritméticas e booleanas, mas após alguma pesquisa e consulta das aulas teóricas, percebemos como os vários parsers constintuintes dos parsers principais para expressões aritméticas e booleanas têm de se chamar recursivamente e numa determinada ordem, de forma a construir a AST que respeita a precedência dos operadores e os requisitos do projecto.