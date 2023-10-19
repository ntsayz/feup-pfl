

%set_anchor(+Cell,+Player)
%Determina que peça localizada em Cell:Row-Coll é marcada com a âncora pertencente ao Player
%Aqui teremos de usar assert?


%cell_belongs_board(+Cell)
%Verificar se coordenadas de uma determinada célula estão dentro dos limites do tabuleiro
%cell_belongs_board(Row-Col):-


%empty_cell(+Cell)
%Verifica se célula Cell:Row-Col no tabuleiro está ou não vazia.






%valid_move(+Orig,+Dest)
%Verifica se mover peça na célula do tabuleiro identificada por Orig:Row1-Cell1 é possivel mover para Dest:Row2-Cell2 


%valid_push(+Orig, +Dest)
%Verifica se peça na célula Orig:Row1-Coll2 pode efetuar um movimento push a peça/peças que estão horizontal/vertical com peça em Orig 


%verify_anchor(+Cell)
%Verifica se peça na célula Cell:Row-Col tem âncora ou não

%cell_has_piece_player(+Cell,+Player)
%Verifica se a célula Cell:Row-Col possui peça e pertence ao jogador Player


%push(+Orig,+Dest):-