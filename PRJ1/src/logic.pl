
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(aggregate)).

%      PIECE OWNERSHIP

% PVP
player_pieces(player1, w_square).
player_pieces(player1, w_round).
player_pieces(player2, b_square).
player_pieces(player2, b_round).

% PVC
player_pieces(player1, w_square).
player_pieces(player1, w_round).
player_pieces(ai, b_square).
player_pieces(ai, b_round).

% CVC
player_pieces(ai1, b_square).
player_pieces(ai1, b_round).
player_pieces(ai2, w_square).
player_pieces(ai2, w_round).


change_player(player1,player2).
change_player(player2,player1).

change_player(player1, ai).
change_player(ai, player1).

change_player(ai1, ai2).
change_player(ai2, ai1).




%anchor_piece(+Player,+Board,+CellPiece)/3
%Determinar que peça do Player no Board em CellPiece:Row-Col fica com âncora
%anchor_piece(Player,Board,Row-Col):-

%Size of Board/Matrix will always be quadratic axb ( different number of rows and Cols and every row have the same number of Col)
%size of the board maybe can change by options ?
:- dynamic size_row/1.
:- dynamic size_col/1.
:- dynamic anchor_piece/2 .

anchor_piece(null-null, noplayer).
change_anchor_piece(NewRow-NewCol, Player):-
    retractall(anchor_piece(_,_)),
    assertz(anchor_piece(NewRow-NewCol, Player)).

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




count_pieces(_, _, [], Count, Count).
%Predicate to count pieces from a list of positions of possible Player pieces
count_pieces(Board, Player, [Row-Col | RestCells], CurrentCount, FinalCount):-
    (cell_has_player_piece(Board, Player, Row-Col,_Piece) ->
        NewCount is CurrentCount +1;
        NewCount is CurrentCount
    ),
    count_pieces(Board, Player, RestCells, NewCount, FinalCount).
    

count_player_pieces(Board, Player,  Count):-

    findall(Row-Col, cell_belongs_to_playable_board(Board, Row-Col), PlayablePositions),
    count_pieces(Board, Player, PlayablePositions, 0, Count).




check_win(Board, CurrentPlayer):-
    change_player(CurrentPlayer, Opponent),
    count_player_pieces(Board, Opponent, NumbPieces), NumbPieces < 6.
    

check_trapped(Board, Player):-
    \+cant_push(Board, Player).


% Check that none of the cells in ResultCells are anchor pieces for Opponent
check_no_anchor_pieces(ResultCells, Opponent) :-
  
    forall(member(Cell, ResultCells), \+ anchor_piece(Cell, Opponent)).
   


%check_push_row_col(+Board, +CurrentPosition, FinalPosition, +MoveType, ?Visited )
%predicate to check if was row or col that changed in the possible vertical/horizontal move above
%base case
check_push_row_col(Board, _Player, CurrRow-CurrCol, _MoveType, Visited, Visited):-
    board_element(Board, CurrRow-CurrCol, Element),
    (Element == out ; Element == empty), ! . % to only check for the first empty or out that is checked to finish check_push_row_col


   
%Versão do predicado com chamada recursiva, para valores de sr, ao tentar continuar recursão possible_move falha ( vai além-do board após algum sr)
check_push_row_col(Board, Player, CurrRow-CurrCol, MoveType, Visited, ResultCells):-
    possible_move(Board, CurrRow-CurrCol, NextRow-NextCol, MoveType), %
    
    \+ member(NextRow-NextCol, Visited),  % Ensure the cell hasn't been visited
   
    check_push_row_col(Board, Player, NextRow-NextCol, MoveType, [NextRow-NextCol|Visited], ResultCells).

valid_push(Board, Player, CurrRow-CurrCol, PushRow-PushCol, ResultCells):-
    possible_move(Board, CurrRow-CurrCol, PushRow-PushCol, MoveType),
    player_square_piece(Board, Player, CurrRow-CurrCol),
    cell_has_player_piece(Board, _AnyPlayer, PushRow-PushCol, _Piece),
    change_player(Player, Opponent),
    check_push_row_col(Board, Player, PushRow-PushCol, MoveType, [PushRow-PushCol], ResultList),
    reverse(ResultList, ResultCells),
    
    check_no_anchor_pieces(ResultCells, Opponent).

    

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
    
    (Element == empty -> 
        change_board_value(Board, LastElement, Piece, FinalPushGameState ); 
        Element == out , FinalPushGameState = Board %  Element==Piece goes out of Board, so it disappeared and the Board is already in the final state
        ).
    
    



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
   
    replace_push_move(NewGameState, Piece, ResultPushCells,FinalPushGameState ),
    change_anchor_piece(PushRow-PushCol, Player). %change the anchor to the new piece-Player that push on PushRow-PushCol position/Opponent piece
    



% TODO
%predicate to check if a player can do any push_move, if not he looses, if or either players cant push after any move, is a draw and game finish


% predicate to do an arbitrary move from a set of "good moves"

%Heuristic because, we are given assumptions of "better" moves/strategies, so we dont need to evaluate every possible turn : moves and responses . We "cut" complexity
/* Explanation to simplify
    We prove that the game is:
        PSPACE-hard to decide who will win from a given position, even for simple (almost rectangular)
        hole-free boards. We also analyze the mate-in-1 problem: can the player win in a single turn?
        One turn in Push Fight consists of up to two “moves” followed by a mandatory “push”. With
        these rules, or generalizing the number of allowed moves to any constant, we show mate-in-1 can
        be solved in polynomial time. If, however, the number of moves per turn is part of the input, the
        problem becomes NP-complete. On the other hand, without any limit on the number of moves
        per turn, the problem becomes polynomially solvable again.
*/

%predicate/heuristics to evaluate values of moves, gameStates etc. 
% predicate to evaluate the Value of a given gameState w.r.t. a given Player1/2 . For GameStates with same value, use real random choose algorithm to avoid Players detect any pattern on the choosen moves
%Heuristic algorithm: predicate to do an AI that chooses the "best" 0, 1 or 2 moves + push move from a set of possible moves

% Heuristic algorithm predicate to do an AI that simulates n (We chooses this) "Optimal" GameStates for a given Player(Him)
% where he also simulate the responses for every simulated "Optimal " GameState from 1... n
