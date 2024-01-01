# INTRODUÇÃO

> Introducao do projecto

> Explicação da divisão entre módulos e o que cada um faz, high level explanation nothing much

> Grafico com a interação entre os mesmos

## IMPLEMENTAÇÃO - PARTE 1

> objetivos

#### Módulo `MachineStructures.hs`


O módulo `MachineStructures` define os elementos fundamentais e os tipos de dados utilizados durante o funcionamento do Interpreter.


`Inst`: Este é um tipo de dados algébrico que representa as instruções da máquina, como operações aritméticas (Add, Sub, Mult), operações lógicas (And, Neg), fluxo de controlo (Branch, Loop), manipulação da Stack e State (Push, Fetch, Store) entre outras. Cada construtor no Inst encapsula uma operação ou comando distinto que a máquina pode executar.

~~~hs
data Inst = Push !Integer | Add | Mult | Sub | Tru | Fals | Equ ...
~~~

~~~hs
type Code = [Inst]
~~~
> O `Code` é uma lista de `Inst`



`Stack` : Uma lista de `StackValue`, onde o valor pode ser um número inteiro `(IntVal)` ou um booleano `(TT, FF)` . É utilizada para avaliar expressões e armazenar temporariamente valores durante a execução.

~~~hs
type Stack = [StackValue]
~~~
> A `Stack` é uma lista de `StackValues`

`State`: Definido como  `(Map.Map String StackValue)`, representa o armazenamento ou a memória da máquina. É essencialmente um map dos nomes das variáveis para seus valores, permitindo que a máquina armazene e recupere dados.

~~~
    STATE   [x=30;y=10]
~~~


#### 2. Módulo `Assembler.hs`


O módulo Assembler é onde se encontra a funcionalidade central do interpretador. Processa uma lista de instruções (Code) juntamente com uma Stack e o State, executando as operações fornecidas. 



Lógica de execução 

`exec`: Esta função recebe uma tuple `(Code, Stack, State)` e aplica a primeira instrução da lista à Stack e ao State atuais.

~~~hs
type Stack = [StackValue]
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

