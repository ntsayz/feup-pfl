:-use_module(library(lists)).
:-use_module(library(random)).
start_game :-
    initial_board(Board),
    display_board(Board),
    game_loop(Board, p1).



game_loop(Board, CurrentPlayer) :-
    write('Player: '), write(CurrentPlayer), nl,
    player_moves(Board, CurrentPlayer, 1, UpdatedBoard),
    switch_turn(CurrentPlayer, NextPlayer),
    game_loop(UpdatedBoard, NextPlayer).

player_moves(Board, Player, MoveNum, UpdatedBoard) :-
    MoveNum =< 3,
    write('Move number: '), write(MoveNum), nl,
    (MoveNum < 3 ->
        read_user_input(Move),
        (Move = xxxx -> % SINCE MOVES ARE NOT OBLIGATORY, IF THE USER INPUTS xxxx, THE MOVE IS SKIPPED
            NextMoveNum is MoveNum + 1,
            player_moves(Board, Player, NextMoveNum, UpdatedBoard);
            convert_to_index(Board, Move, [CurrRow,CurrCol, DestRow,DestCol], Pieces),
        valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, Visited) -> % determine if move is valid
            move_piece(Board, Indexes, TempBoard),
            display_board(TempBoard), % if valid, display board
            NextMoveNum is MoveNum + 1, % increment move number
            player_moves(TempBoard, Player, NextMoveNum, UpdatedBoard);
        write('Invalid move! Try again.'), nl, % if invalid, display error message
        player_moves(Board, Player, MoveNum, UpdatedBoard));
    make_push(Board, Player, UpdatedBoard)).  



% predicate to switch players
switch_turn(p1, p2).
switch_turn(p2, p1).

%valid_move1(Board, Player, Move, TempBoard):-
%    true.


read_user_input(Move):-
    read(Input),    %d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, ListOfMoves).


parse_input([X1, Y1, X2, Y2], Move, ListOfMoves) :-
    (   [X1, Y1, X2, Y2] = ['x', 'x', 'x', 'x'] ->
        Move = xxxx
    ;   char_code(X1, X1Code),
        char_code(Y1, Y1Code),
        char_code(X2, X2Code),
        char_code(Y2, Y2Code),
        X1Code >= 97,
        X1Code =< 104,
        Y1Code >= 49,
        Y1Code =< 56,
        X2Code >= 97,
        X2Code =< 104,
        Y2Code >= 49,
        Y2Code =< 56,
        Move = [X1Code, Y1Code, X2Code, Y2Code]
    ).

convert_to_index(Board, Move, Indexes, Pieces) :-
    Move = [X1, Y1, X2, Y2],
    
    % debugging
    write('Move: '), write(Move), nl,
    
    % converting char to index with adjustments
    X1Index is X1 - 97 + 1, 
    Y1Index is Y1 - 48,
    X2Index is X2 - 97 + 1, 
    Y2Index is Y2 - 48, 
    
    % debugging
    write('Indexes: '), write([X1Index, Y1Index, X2Index, Y2Index]), nl,
    
    % getting pieces
    nth0(Y1Index, Board, Row1),
    nth0(X1Index, Row1, Piece1),
    nth0(Y2Index, Board, Row2),
    nth0(X2Index, Row2, Piece2),
    
    % debugging
    write('Pieces: '), write([Piece1, Piece2]), nl,
    

    Indexes = [X1Index, Y1Index, X2Index, Y2Index],
    Pieces = [Piece1, Piece2].


move_piece(Board, [X1, Y1, X2, Y2], UpdatedBoard) :-
    nth0(Y1, Board, Row1),
    nth0(X1, Row1, Piece),
    update_board(Board, X1, Y1, empty, TempBoard),
    update_board(TempBoard, X2, Y2, Piece, UpdatedBoard).

update_board(Board, X, Y, Value, UpdatedBoard) :-
    nth0(Y, Board, Row),
    replace_element(Row, X, Value, UpdatedRow),
    replace_element(Board, Y, UpdatedRow, UpdatedBoard).

replace_element(List, Index, Value, UpdatedList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, UpdatedList, Value, Rest).

push(Board, Player, UpdatedBoard) :-
    write('Mandatory push!'), nl,
    read_user_input(Move),
    (valid_push(Board, Player, Move, TempBoard) ->
        display_board(TempBoard),
        UpdatedBoard = TempBoard;
    write('Invalid push! Try again.'), nl,
    mandatory_push(Board, Player, UpdatedBoard)).

% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4
% ==============================================================================


piece_owner(p1, w_square).
piece_owner(p1, w_round).
piece_owner(p2, b_square).
piece_owner(p2, b_round).
