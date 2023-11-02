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
        (Move = xxxx ->
            NextMoveNum is MoveNum + 1,
            player_moves(Board, Player, NextMoveNum, UpdatedBoard);
        valid_move(Board, Player, Move, TempBoard) ->
            display_board(TempBoard),
            NextMoveNum is MoveNum + 1,
            player_moves(TempBoard, Player, NextMoveNum, UpdatedBoard);
        write('Invalid move! Try again.'), nl,
        player_moves(Board, Player, MoveNum, UpdatedBoard));
    mandatory_push(Board, Player, UpdatedBoard)).




% predicate to switch players
switch_turn(p1, p2).
switch_turn(p2, p1).



read_user_input(Move):-
    read(Input),    %d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, ListOfMoves).

% predicate to parse user input sictus
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

    %Move = [X1Code, Y1Code, X2Code, Y2Code],
    %Move = valid_move(), If valid move returns true ; update board 
    %ListOfMoves = [Move]
    

% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4
% ==============================================================================
replace_nth0(Index, NewElem, List, UpdatedList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewElem, UpdatedList, Rest).


mandatory_push(Board, Player, UpdatedBoard) :-
    write('Mandatory push!'), nl,
    read_user_input(Move),
    (valid_push(Board, Player, Move, TempBoard) ->
        display_board(TempBoard),
        UpdatedBoard = TempBoard;
    write('Invalid push! Try again.'), nl,
    mandatory_push(Board, Player, UpdatedBoard)).


valid_move(Board, Player, Move, UpdatedBoard) :-
    % Extracting the coordinates of the move
    Move = [X1, Y1, X2, Y2],
    
    % Print Move for debugging
    write('Move: '), write(Move), nl,
    
    % Converting char to index with adjustments
    X1Index is X1 - 97 + 1, 
    Y1Index is Y1 - 48,
    X2Index is X2 - 97 + 1, 
    Y2Index is Y2 - 48, 
    
    % Print indexes for debugging
    write('Indexes: '), write([X1Index, Y1Index, X2Index, Y2Index]), nl,
    
    % Accessing the pieces from the board
    nth0(Y1Index, Board, Row1),
    nth0(X1Index, Row1, Piece1),
    nth0(Y2Index, Board, Row2),
    nth0(X2Index, Row2, Piece2),
    
    % Print pieces for debugging
    write('Pieces: '), write([Piece1, Piece2]), nl,
    
    % Checking if the move is valid
    valid_piece_move(Player, Piece1, X1Index, Y1Index, X2Index, Y2Index, Board),
    
    % Print after valid_piece_move
    write('After valid_piece_move'), nl,
    
    % Updating the board
    update_board(Board, X1Index, Y1Index, X2Index, Y2Index, UpdatedBoard).




valid_piece_move(Player, Piece, X1, Y1, X2, Y2, Board) :-
    % Check if the piece belongs to the player
    piece_owner(Player, Piece),
    % Check if the destination cell is empty
    nth0(Y2, Board, Row2),
    nth0(X2, Row2, empty),
    % Additional rules for specific piece types can be added here
    true.


piece_owner(p1, w_square).
piece_owner(p1, w_round).
piece_owner(p2, b_square).
piece_owner(p2, b_round).

update_board(Board, X1, Y1, X2, Y2, UpdatedBoard) :-
    % Accessing the piece
    nth0(Y1, Board, Row1),
    nth0(X1, Row1, Piece),
    % Updating the destination cell
    replace(Board, Y2, X2, Piece, TempBoard),
    % Clearing the source cell
    replace(TempBoard, Y1, X1, empty, UpdatedBoard).

replace(Board, RowIndex, ColIndex, NewElement, UpdatedBoard) :-
    nth0(RowIndex, Board, Row),
    replace_nth0(ColIndex, NewElement, Row, UpdatedRow),
    replace_nth0(RowIndex, UpdatedRow, Board, UpdatedBoard).

