
:- use_module(library(lists)).
:- use_module(library(between)).



player_pieces(player1, w_square).
player_pieces(player1, w_round).
player_pieces(player2, b_square).
player_pieces(player2, b_round).

change_player(player1,player2).
change_player(player2,player1).


%anchor_piece(+Player,+Board,+CellPiece)/3
%Determinar que peça do Player no Board em CellPiece:Row-Col fica com âncora
%anchor_piece(Player,Board,Row-Col):-

%Size of Board/Matrix will always be quadratic axb ( different number of rows and Cols and every row have the same number of Col)
%size of the board maybe can change by options ?
:- dynamic size_row/1.
:- dynamic size_col/1.
:- dynamic anchor_piece/2 .

anchor_piece(4-2, player1).

size_row(6).
size_col(10).


%set_anchor(+Cell,+Player,+Board)
%Determina que peça de um Player no Board localizada em Cell:Row-Coll é marcada com a âncora 
%Aqui teremos de usar assert/assertz?

%cell_belongs_board(+Cell)
%Verificar se coordenadas de uma determinada célula estão dentro dos limites do tabuleiro
cell_belongs_board(Board,Row-Col):-
    length(Board,SizeRows),
    SizeRows>0,
    nth0(Row,Board,BoardRow), %get one of the rows/list of the board
    length(BoardRow,SizeCols),
    SizeCols>0,

    Row >=0, 
    Row < SizeRows, 
    Col >=0, 
    Col < SizeCols.
%cell_belongs_to_playable_board(+Board, +Position:Row-Col)
%Verifica se Position pertence ao Board representado pela matriz, mas também se Position é uma posição do Board que fica dentro da área do Board onde as peças se podem movimentar
cell_belongs_to_playable_board(Board, Row-Col):-
    cell_belongs_board(Board, Row-Col),
   ( empty_cell(Board,Row-Col) ); (cell_has_player_piece(Board,_Player,Row-Col,_Piece)).

%board_element(+Board,+Cell,-Element)
%Obter a Piece no Board numa determinada posição: Row-Col
board_element(Board,Row-Col,Element):-
    cell_belongs_board(Board,Row-Col),

    nth0(Row,Board,BoardRow),
    nth0(Col,BoardRow,Element).

%empty_cell(+Cell,+Board)
%Verifica se célula Cell:Row-Col no Board está ou não vazia.
empty_cell(_Board,Row-Col):-

    board_element(_Board,Row-Col,empty) .
    

%cell_has_player_piece(+Board,+Player,+CellPiece,?Piece)
%Verifica se a célula CellPiece:Row-Col no Board possui uma peça e pertence ao jogador Player
cell_has_player_piece(Board,Player,Row-Col,Piece):-

    board_element(Board,Row-Col,Piece),
    player_pieces(Player,Piece) .

%player_square_piece(+Board,+Player,+CellPiece)
%Verifica se peça em Cell:Row-Col é uma square piece do Player
player_square_piece(Board,player1,Row-Col):-
    cell_has_player_piece(Board,player1,Row-Col,w_square).

player_square_piece(Board,player2,Row-Col):-
    cell_has_player_piece(Board,player2,Row-Col,b_square).


%player_round_piece(+Board,+Player,+CellPiece)
%Verifica se peça em Cell:Row-Col é uma square piece do Player
player_round_piece(Board,player1,Row-Col):-
    cell_has_player_piece(Board,player1,Row-Col,w_round).

player_round_piece(Board, player2, Row-Col):-
    cell_has_player_piece(Board,player2,Row-Col,b_round).



piece_is_anchored(Board, Player, PieceRow-PieceCol):-
    player_square_piece(Board,Player,PieceRow-PieceCol),
    anchor_piece(PieceRow-PieceCol, Player).




%before

%possible_move(+Board,+CurrentPosition,+NewPosition,-MoveType)
%Determina possíveis posições partindo de CurrentPosition para NewPosition tendo em conta os movimentos possíveis do Push-Fight ( up,down,left,Right from a given cell to other cell)
possible_move(Board, CurrRow-CurrCol,NewRow-NewCol, up):-

    NewRow is CurrRow-1, NewCol is CurrCol, %up move
    cell_belongs_board(Board,NewRow-NewCol).
possible_move(Board, CurrRow-CurrCol,NewRow-NewCol, down):- %down move

    NewRow is CurrRow+1, NewCol is CurrCol,
    cell_belongs_board(Board,NewRow-NewCol).

possible_move(Board, CurrRow-CurrCol,NewRow-NewCol, left):- %left move

    NewRow is CurrRow, NewCol is CurrCol-1,
    cell_belongs_board(Board,NewRow-NewCol).

possible_move(Board, CurrRow-CurrCol,NewRow-NewCol, right):- %right move

    NewRow is CurrRow, NewCol is CurrCol+1,
    cell_belongs_board(Board,NewRow-NewCol).
%valid_move(+Board,+CurrentPosition,+DestPosition,?Visited)
%Verifica se mover uma possível peça em CurrentPosition para DestPosition(destination position) é um movimento válido

%base case Current Position = DestPosition

valid_move(_Board, _Player, CurrRow-CurrCol, CurrRow-CurrCol, _Visited).

valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, Visited):-
    possible_move(Board, CurrRow-CurrCol, NewRow-NewCol,_MoveType),
    % Every recursive call, we Verify if the move is valid to a new possible empty cell destination
    empty_cell(Board, NewRow-NewCol),
    \+ member(NewRow-NewCol, Visited),  % Ensure the current cell hasn't been visited
    valid_move(Board, Player, NewRow-NewCol, DestRow-DestCol, [CurrRow-CurrCol|Visited]).


% valid_move(+Board,+CurrentPosition,+DestPosition)
% Ponto de entrada sem acumulador, para verificar se movimento é válido
valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol) :-
    empty_cell(Board, DestRow-DestCol), %DestinationPosition needs to be an empty cell
    cell_has_player_piece(Board, Player,CurrRow-CurrCol,_Piece), %célula de onde movimento parte, tem de ter uma peça de um Player

    %Chamada recursiva utilizando acumulador, para evitar recursão infinita / evitar voltar a tentar células já tentadas anteriormente
    valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, []).


check_win(Board, Player, CurrRow-CurrCol, FinalRow-FinalCol, win):-
    board_element(Board, FinalRow-FinalCol, Element),
    Element == out,
    change_player(Player,Opponent),
    cell_has_player_piece(Board, Opponent, CurrRow-CurrCol, _Piece).

check_win(Board, _Player, _CurrRow-_CurrCol, FinalRow-FinalCol, no_win):-
    board_element(Board, FinalRow-FinalCol, Element),
    Element \== out.

%check_push_row_col(+Board, +CurrentPosition, FinalPosition, +MoveType, ?Visited )
%predicate to check if was row or col that changed in the possible vertical/horizontal move above
%base case
check_push_row_col(Board, _Player, CurrRow-CurrCol, _MoveType, Visited, Visited):-
    board_element(Board, CurrRow-CurrCol, Element),
    (Element == out ; Element == empty), ! . % to only check for the first empty or out that is checked to finish check_push_row_col


   
%Versão do predicado com chamada recursiva, para valores de sr, ao tentar continuar recursão possible_move falha ( vai além-do board após algum sr)
check_push_row_col(Board, Player, CurrRow-CurrCol, MoveType, Visited, ResultCells):-
    possible_move(Board, CurrRow-CurrCol, NextRow-NextCol, MoveType), %
    change_player(Player, Opponent),
    \+ piece_is_anchored(Board, NextRow-NextCol, Opponent),
    \+ member(NextRow-NextCol, Visited),  % Ensure the cell hasn't been visited
   
    check_push_row_col(Board, Player, NextRow-NextCol, MoveType, [NextRow-NextCol|Visited], ResultCells).

valid_push(Board, Player, CurrRow-CurrCol, PushRow-PushCol, ResultCells):-
    possible_move(Board, CurrRow-CurrCol, PushRow-PushCol, MoveType),
    player_square_piece(Board, Player, CurrRow-CurrCol),
    cell_has_player_piece(Board, _AnyPlayer, PushRow-PushCol, _Piece),
    change_player(Player, Opponent),
    \+ piece_is_anchored(Board, PushRow-PushCol, Opponent),
    check_push_row_col(Board, Player, PushRow-PushCol, MoveType, [PushRow-PushCol], ResultList),
    reverse(ResultList, ResultCells).
    

% change_board_value(+Board, +Player,+CurrentPosition, +Value, -NewBoard)
% Predicado para alterar valor de uma célula do board
change_board_value(Board,CurrRow-CurrCol,Value, NewBoard):-
    %get Row From the board and get new Row with replaced Value
    nth0(CurrRow, Board, BoardRow),
    nth0(CurrCol, BoardRow, _Elem1, Rest1),
    nth0(CurrCol, ResultRow1, Value, Rest1),
    %Change the row on the board, with the new ResultRow1 already with the replaced Value and get the NewGameState
    nth0(CurrRow, Board, _Elem2, Rest2),
    nth0(CurrRow, NewBoard, ResultRow1, Rest2).
    


% make_move(+Board, +Player, +PiecePosition, +DestinationPosition, -NewGameState)/5
% Predicado para obter NewGameState em relação a um movimento de deslocamento de uma peça do Player pelo Board
make_move(Board, Player, PieceRow-PieceCol, DestRow-DestCol, NewGameState):-
    cell_belongs_to_playable_board(Board, PieceRow-PieceCol ),
    cell_belongs_to_playable_board(Board, DestRow-DestCol),
    valid_move(Board, Player, PieceRow-PieceCol, DestRow-DestCol),
    cell_has_player_piece(Board,Player,PieceRow-PieceCol,Piece), %save the piece to use later
    change_board_value(Board, PieceRow-PieceCol, empty, NewBoard),
    change_board_value(NewBoard, DestRow-DestCol, Piece, NewGameState).
    


replace_push_move(Board, Piece, [LastElement | []], FinalPushGameState):-
    board_element(Board,LastElement,Element ),
    
    (Element == empty ; Element == out ),
    change_board_value(Board, LastElement, Piece, FinalPushGameState ) .
    



replace_push_move(Board, Piece, [H1 | CellsToReplace], FinalPushGameState):-
    
    cell_has_player_piece(Board,_AnyPlayer,H1,NextPiece),

    change_board_value(Board, H1, Piece, NewGameState),
    replace_push_move(NewGameState, NextPiece, CellsToReplace, FinalPushGameState).
    



% make_push(+Board, +Player, +PiecePosition, +DestinationPosition, -NewBoard):-
% Predicado para obter NewGameState em relação a um movimento de push de uma peça do Player no Board
make_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, FinalPushGameState):-
    cell_belongs_to_playable_board(Board, PieceRow-PieceCol ),
    cell_belongs_to_playable_board(Board, PushRow-PushCol), 
    valid_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, ResultPushCells),
    cell_has_player_piece(Board,Player,PieceRow-PieceCol,Piece),
    change_board_value(Board, PieceRow-PieceCol, empty, NewGameState),
    
    replace_push_move(NewGameState, Piece, ResultPushCells, FinalPushGameState ).
    



% TODO
%predicate to check if a player can do any push_move, if not he looses, if or either players cant push after any move, is a draw and game finish