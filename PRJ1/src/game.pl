:-use_module(library(lists)).
:-use_module(library(random)).
start_game :-
    initial_board(Board),
    display_board(Board),
    game_loop(Board, p1).



game_loop(Board, CurrentPlayer) :-
    
    read_user_input(Move),
    

    % TODO: update board.
    clear_screen,
    % display board.
    display_board(Board),

    % TODO: Win condition.

    % Switch to the next player turn
    game_loop(Board, CurrentPlayer).


% predicate to switch players
switch_turn(p1, p2).
switch_turn(p2, p1).



read_user_input(Move):-
    read(Input),    %d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, ListOfMoves).

% predicate to parse user input sictus
parse_input([X1, Y1, X2, Y2], Move, ListOfMoves) :-
    char_code(X1, X1Code),
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
    Y2Code =< 56.
    %Move = [X1Code, Y1Code, X2Code, Y2Code],
    %Move = valid_move(), If valid move returns true ; update board 
    %ListOfMoves = [Move]
    

% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4

