# Issues to depurate/debug
- Simplificar código
- Representação de board: Perguntar professor se é a melhor
- Modificar board a cada jogada dentro dos limites e regras do jogo
- Tem de puder aumentar tamanho do tabuleiro consoante a escolha dos jogadores! E com maior tamanho, aumentar o número de peças. -> Criar regra para isso !
- Formas diferentes
- Ver difference lists, pode ser útil?
- ver árvores de estados do jogo/minimax
- check syntactic sugar
- check this : CLPFD Library - Constraint Programming!!!!!
- State pontuation of player game: 
    - numero de peças em zonas centrais do tabuleiro
    - jogadas onde maior numero de peças está longe de zonas out
    - Jogadas que cercam peças do adversário
    - procurar mais jogadas/situações vantajosas

- Check this to implement possible minimax or other AI:
        SICStus has several (~55) libraries, with different purposes
    • Providing common data structures, such as sets and ordered sets,
    bags, queues, association lists, trees, or graphs, among others


## Info professor
Move usando between
findall(x-y,move(GameState,X-Y, NewGameState),NewGameState)

random_select para AI arbitrário

Uso de setof(V-X-Y,(move(GM,X-Y,NGS)), Value(NGS,V))
Selecionar jogada que tem o melhor valor, para valores iguais ter uma regra de desempate aleatória?

Melhorar verificação de células usando between ?!? Generalizar board para vários tamanhos etc