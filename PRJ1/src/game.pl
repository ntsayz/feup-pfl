:-use_module(library(lists)).
:-use_module(library(random)).
start_game :-
    initial_board(Board),
    display_board(Board),
    game_loop(Board, p1).

game_loop(Board, CurrentPlayer) :-
    get_single_char(Choice),
    (Choice = 'q' ; Choice = 'Q') ->
        nl
    ;
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

% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4

