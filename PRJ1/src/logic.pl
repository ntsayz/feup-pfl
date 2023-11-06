
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(aggregate)).
:- use_module(library(random)).

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
:- dynamic number_pieces/1 .

anchor_piece(null-null, noplayer).
change_anchor_piece(NewRow-NewCol, Player):-
    retractall(anchor_piece(_,_)),
    assertz(anchor_piece(NewRow-NewCol, Player)).

%Default size Row and Col of a normal Push-Fight Game
size_row(6).
size_col(10).

%change_size_board(+SizeRows, +SizeCols)
%Change the size of the board to SizeRows x SizeCols
change_size_board(SizeRow, SizeCol):-
    retract(size_row(_)),
    assertz(size_row(SizeRow)),
    retract(size_col(_)),
    assertz(size_col(SizeCol)).

number_pieces_player(5). % default game number of Pieces a Player need to have to continue playing

%change_number_pieces(+NumberOfPieces)
%Change the Number Of pieces that player must have in order to play again
change_number_pieces(NumberOfPieces):-
    retractall(number_pieces_player(_)),
    assertz(number_pieces_player(NumberOfPieces)).





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
    cell_belongs_to_playable_board(Board, CurrRow-CurrCol ),
    cell_belongs_to_playable_board(Board, DestRow-DestCol),
    cell_has_player_piece(Board, Player,CurrRow-CurrCol,_Piece), %célula de onde movimento parte, tem de ter uma peça de um Player
    %Chamada recursiva utilizando acumulador, para evitar recursão infinita / evitar voltar a tentar células já tentadas anteriormente
    empty_cell(Board, DestRow-DestCol), %DestinationPosition needs to be an empty cell
    once(valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, [])).

% Find all unique valid moves and remove duplicates
find_unique_valid_moves(Board, Player, CurrRow-CurrCol, DestRow-DestCol, UniqueMoves) :-
    findall(
        Move,
        valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, Move),
        Moves
    ),
    remove_duplicates(Moves, UniqueMoves).

% Remove duplicates from a list of moves
remove_duplicates([], []).
remove_duplicates([Head|Tail], UniqueList) :-
    member(Head, Tail),
    !,
    remove_duplicates(Tail, UniqueList).
remove_duplicates([Head|Tail], [Head|UniqueList]) :-
    \+ member(Head, Tail),
    remove_duplicates(Tail, UniqueList).
   

% Check that none of the cells in ResultCells are anchor pieces for Opponent
check_no_anchor_pieces(ResultCells, Opponent) :-
  
    forall(member(Cell, ResultCells), \+ anchor_piece(Cell, Opponent)).
   


% make_move(+Board, +Player, +PiecePosition, +DestinationPosition, -NewGameState)/5
% Predicado para obter NewGameState em relação a um movimento de deslocamento de uma peça do Player pelo Board
make_move(Board, Player, PieceRow-PieceCol, DestRow-DestCol, NewGameState):-
    valid_move(Board, Player, PieceRow-PieceCol, DestRow-DestCol),
    cell_has_player_piece(Board,Player,PieceRow-PieceCol,Piece), %save the piece to use later
    change_board_value(Board, PieceRow-PieceCol, empty, NewBoard),
    change_board_value(NewBoard, DestRow-DestCol, Piece, NewGameState).
    
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
    cell_belongs_to_playable_board(Board, CurrRow-CurrCol ),
    cell_belongs_to_playable_board(Board, PushRow-PushCol), 
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
    valid_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, ResultPushCells),
    cell_has_player_piece(Board,Player,PieceRow-PieceCol,Piece), % to get piece to use later
    change_board_value(Board, PieceRow-PieceCol, empty, NewGameState),
    replace_push_move(NewGameState, Piece, ResultPushCells,FinalPushGameState ),
    change_anchor_piece(PushRow-PushCol, Player). %change the anchor to the new piece-Player that push on PushRow-PushCol position/Opponent piece
    


% Iterate over each row and column of the board
iterate_over_board(Board, Row, Col) :-
    length(Board, NumRows),
    between(0, NumRows, Row),
    nth1(Row, Board, CurrentRow),
    length(CurrentRow, NumCols),
    between(0, NumCols, Col).



% get_player_pieces_lists(+Board, +Player, -ListOfPlayerSquares, -ListOfPlayerRounds)
% This predicate will return all square and round pieces for a given player on a given board.

get_player_pieces_lists(Board, Player, ListOfPlayerSquares, ListOfPlayersRounds):-
    findall(Row-Col, (iterate_over_board(Board, Row, Col), player_square_piece(Board, Player, Row-Col)), ListOfPlayerSquares),
    findall(Row-Col, (iterate_over_board(Board, Row, Col), player_round_piece(Board, Player, Row-Col)), ListOfPlayersRounds).

%player_loose(+Board, +Player)
%Predicate to check if Player have lost the game
player_lost_game(Board, Player):-
    get_player_pieces_lists(Board, Player, ListOfPlayerSquares, ListOfPlayerRounds),
    length(ListOfPlayerSquares, L1), length(ListOfPlayerRounds, L2), Count is L1 + L2 , \+ number_pieces_player(Count).



find_valid_push_moves(Board, Player, ValidPushMoves) :-
    get_player_pieces_lists(Board, Player, ListOfPlayerSquares, _ListOfPlayerRounds),
    findall(
        [PieceRow-PieceCol, PushRow-PushCol, ResultPushCells],
        (
            member(PieceRow-PieceCol, ListOfPlayerSquares),
            valid_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, ResultPushCells)
        ),
        ValidPushMoves
    ).

%Auxilliar predicate to unify List of Pieces
get_all_player_pieces(Board, Player,ListOfPlayerPositionsPieces ):-
    get_player_pieces_lists(Board, Player, ListOfPlayerSquares, ListOfPlayerRounds),
    append(ListOfPlayerSquares, ListOfPlayerRounds, ListOfPlayerPositionsPieces).

%get_all_empty_cells(+Board, -EmptyCellCoordinates)
%This function returns list with coordinates of all empty cells in the board
get_all_empty_cells(Board, EmptyCellCoordinates):-
    findall(Row-Col, (iterate_over_board(Board, Row, Col), empty_cell(Board,Row-Col)), EmptyCellCoordinates).

%
find_valid_moves(Board, Player, ValidMoves):-

    get_all_player_pieces(Board, Player,ListOfPlayerPositionsPieces ),
    get_all_empty_cells(Board, ListOfEmpty),

    findall(CurrRow-CurrCol-DestRow-DestCol,
    (   
        member(CurrRow-CurrCol, ListOfPlayerPositionsPieces),
        member(DestRow-DestCol, ListOfEmpty),
        valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol)



    ), ValidMoves
    ) .

find_valid_round_moves(Board, Player, ValidRoundMoves):-

    get_player_pieces_lists(Board, Player, _ListOfPlayerSquares, ListOfPlayersRounds),
    get_all_empty_cells(Board, ListOfEmpty),
    findall(CurrRow-CurrCol-DestRow-DestCol,
    (   
        member(CurrRow-CurrCol, ListOfPlayersRounds),
        member(DestRow-DestCol, ListOfEmpty),
        valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol)



    ), ValidRoundMoves
    ) .

%find_move_game_states(+Board, +Player, +ListOfNewGameStates)
%Predicado para obter todos os GameStates resultantes para todos os ValidMoves possíves
find_move_game_states(Board, Player, ListOfNewGameStates):-
    find_valid_moves(Board, Player, ValidMoves),
    findall(BoardGameState,
    (
        member(CurrRow-CurrCol-DestRow-DestCol, ValidMoves),
        make_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, BoardGameState),
        \+player_cant_push(BoardGameState-Player) % assegurar que não são escolhidos valid_moves que levam a BoardGameState


    ), ListOfNewGameStates
    
    
    ).


%find_push_game_states(+Board, +Player, +ListOfNewGameStates)
%Predicado para obter todos os GameStates resultantes para todos os ValidPushMoves possíves
find_push_game_states(Board, Player, ListOfNewGameStates):-
    find_valid_push_moves(Board, Player, ValidPushMoves),
    findall(BoardGameState,

    (
        member([PieceRow-PieceCol, PushRow-PushCol, _ResultPushCells], ValidPushMoves),
        make_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, BoardGameState),
        \+ player_lost_game(BoardGameState, Player) %AI will not choose moves after push , the player push his own piece out of the board

    ), ListOfNewGameStates

    ).

    
%player_trapped(Board,Player) 
%Predicate that checks if all players are trapped ( cant push ) and has no way out for every piece
%Este predicado não é utilizado no jogo, já que mesmo que o Player atual a jogar esteja trapped e o Oponnent também, o Oponnent ainda teria 2 moves possíveis para ficar em posição de fazer push
players_cant_push_draw(Board,Player):-
    find_valid_push_moves(Board, Player, []), % No possible push
    change_player(Player, Opponent),
    find_valid_push_moves(Board, Opponent, []). %No possible push

%predicate to check, after move phase, if before push phase, Player cant push, if not he lose the game
player_cant_push(Board-Player):-
    find_valid_push_moves(Board, Player, []).

% game_over(+GameState, -Winner)

game_over(Board-Player, Winner):-
    player_lost_game(Board, Player),
    change_player(Player, Winner).

game_over(Board-Player, Winner):-
    player_lost_game(Board, Player),
    player_cant_push(Board-Player),
    change_player(Player, Winner).

%case when Player push a piece out of the Board, the winner is Player !
game_over(Board-Player, Winner):- 
    change_player(Player, Opponent),
    player_lost_game(Board, Opponent),
    change_player(Opponent, Winner).
   
   
   


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

% give different weights for different heuristic evaluation above ?
%number of possible push moves: less possible push moves for the opponent, less value The GameState has, better for Player
%Write more from: https://www.abstractgames.org/pushfight.html
% For a given, GameState, if opponent squares have less possible push_moves, Val is less
%For given GameState, if opponent round_pieces, have less valid_moves, this gives less pontuation to this GameState then others
%Rounde_pieces cant push, so they are more vulnerable to be pushing out
%More Round_pieces next to edge locations, GameState has less points
%THe more roundPieces are separated from the SquarePieces of the samePlayer, less value has the GameState for the Opponnent
%The more roundPieces are far from the centre of the board, less value have the GameState for the Opponent that Player wants to minimize
%For given GameState, square_pieces next to edge locations give less points ( but less negative then Round_pieces)
%For given GameState, less roundPiece that have squares between cloosest edge and circle, don have the good defenders squares, so GameState have less Value for Opponent


evaluate_push_mobility(GameState, Player, Value):-
    find_valid_push_moves(GameState, Player, ValidPushMoves),
    length(ValidPushMoves, LengthOfValidPushMoves), number_pieces_player(N),
    Value is 4*(LengthOfValidPushMoves // (N // 2 +1) ) .


evaluate_move_mobility(GameState, Player, Value):-
    find_valid_moves(GameState, Player, ValidMoves),
    length(ValidMoves, LengthOfValidPushMoves), number_pieces_player(N),
    Value is LengthOfValidPushMoves // N .


evaluate_round_pieces_mobility(GameState, Player, Value):-
    find_valid_round_moves(GameState, Player, ValidRoundMoves),
    length(ValidRoundMoves, LengthOfValidRoundMoves), number_pieces_player(N),
    Value is 3* (LengthOfValidRoundMoves // N ) .


%Add mais mobility evaluations

evaluate_mobility(GameState, Player, Value):-
    
    evaluate_push_mobility(GameState, Player, Value1),
    evaluate_move_mobility(GameState, Player, Value2),
    evaluate_round_pieces_mobility(GameState, Player, Value3),
    Value is Value1 + Value2 + Value3.


%value(+GameState, +Player, -Value):-
value(GameState, Player, Value):-
    evaluate_mobility(GameState,Player,Value).
% Heuristic algorithm predicate to do an AI that simulates n (We chooses this) "Optimal" GameStates for a given Player(Him)
% where he also simulate the responses for every simulated "Optimal " GameState from 1... n


evaluate_game_state_list(ListGameStates, Player, SortedListGameStateValue):-
    findall(Value-GameState,(
        member(GameState, ListGameStates),
        value(GameState, Player, Value)
    ),ListGameStateValue
    ), keysort(ListGameStateValue, SortedListGameStateValue).
    
% Helper predicate to check if the value matches the best value
equal_value(BestValue, Value-_) :-
    BestValue == Value.

ai_move_game_state(GameState, Player, Val-RandomBestGameState):-
    find_move_game_states(GameState, Player, ListOfNewGameStates),
    change_player(Player, Opponent),
   
    evaluate_game_state_list(ListOfNewGameStates, Opponent, SortedListGameStateValue),
    
    SortedListGameStateValue = [BestValue-_|_],
    
    include(equal_value(BestValue), SortedListGameStateValue, BestGameStates),
    
    random_member(Val-RandomBestGameState, BestGameStates).
    %check also mora Val-FinalGameState with same Val and choose using random choice


count_moves(Val0, Val1, Val2, MoveCount):-
    (Val0 =:= Val1, Val1 =:= Val2 -> MoveCount = 0;
     Val0 =\= Val1, Val1 =:= Val2 -> MoveCount = 1;
     Val0 =\= Val1, Val1 =\= Val2 -> MoveCount = 2).

ai_move_turn(GameState, Player, Val-FinalGameState,MoveCount):-
    evaluate_game_state_list([GameState], Player, [Val0-GameState]),

    ai_move_game_state(GameState, Player, Val1-FirstMoveGameState),
    ai_move_game_state(FirstMoveGameState, Player, Val2-SecondGameState),
    keysort([Val0-GameState, Val1-FirstMoveGameState, Val2-SecondGameState ], ListTurnMoves),
    ListTurnMoves = [BestValue-_|_],
    include(equal_value(BestValue), ListTurnMoves, BestMoveGameStates),
    random_member(Val-FinalGameState, BestMoveGameStates),
    count_moves(Val0, Val1, Val2, MoveCount).
