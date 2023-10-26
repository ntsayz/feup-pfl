
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


%collumn(+Letter,-Number)
%Traduz letras utilizadas para identificar colunas no board graficamente em Number de Colunas para trabalhar com matrizes e listas
collumn('a',0).
collumn('b',1).
collumn('c',2).
collumn('d',3).
collumn('e',4).
collumn('f',5).
collumn('g',6).
collumn('h',7).
collumn('i',8).
collumn('j',9).

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

%Efetuar movimento, mudando peça de lugar e atualizando o Board
%move(Player, CurrentPosition, FinalCell,?Board)

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
check_push_row_col(Board, _Player, CurrRow-CurrCol, FinalRow-FinalCol, _MoveType, _Visited):-
     board_element(Board, CurrRow-CurrCol, Element),
     (Element == empty ; Element == out),

    FinalRow = CurrRow, FinalCol = CurrCol.
   
%Versão do predicado com chamada recursive, para valores de sr, ao tentar continuar recursão possible_move falha ( vai além-do board após algum sr)
check_push_row_col(Board, Player, CurrRow-CurrCol, _FinalRow-_FinalCol, MoveType, Visited):-
    possible_move(Board, CurrRow-CurrCol, NextRow-NextCol, MoveType), %
    change_player(Player, Opponent),
    \+ piece_is_anchored(Board, NextRow-NextCol, Opponent),
    \+ member(NextRow-NextCol, Visited),  % Ensure the cell hasn't been visited
   
    check_push_row_col(Board, Player, NextRow-NextCol, NextRow-NextCol, MoveType, [NextRow-NextCol|Visited]).

valid_push(Board, Player, CurrRow-CurrCol, PushRow-PushCol, FinalRow-FinalCol):-
    possible_move(Board, CurrRow-CurrCol, PushRow-PushCol, MoveType),
    player_square_piece(Board, Player, CurrRow-CurrCol),
    cell_has_player_piece(Board, _AnyPlayer, PushRow-PushCol, _Piece),
    change_player(Player, Opponent),
    \+ piece_is_anchored(Board, PushRow-PushCol, Opponent),
    check_push_row_col(Board, Player, PushRow-PushCol, FinalRow-FinalCol, MoveType, [PushRow-PushCol]).
    %check sr cells that stop the push movement,
    %check piece with anchor that stop the push movement,
%valid_push(+Board, +Orig, +Dest)
%Verifica se peça na célula Orig:Row1-Coll2 pode efetuar um movimento push a peça/peças que estão horizontal/vertical com peça em Orig no Board
%Em push vertical temos de ver se push não empurra alguma peça nessa coluna contra um side rail (sr)
% Push horizontal, não existem side rails
% Em todos, temos de verificar se não estamos empurrar linha/coluna de peças onde uma das peça tem âncora
% valid_push(Board,Player ,CurrRow-CurrCol, DestRow-DestCol):-
%     possible_push(Board,Player ,CurrRow-CurrCol, DestRow-DestCol),

%verify_anchor(+Cell,+Board)
%Verifica se peça na célula Cell:Row-Col do Board tem âncora ou não


%push(Board?, +Orig,+Dest)
%Efetua o movimento push da peça Orig:Row-Col para Dest:Row-Col no Board