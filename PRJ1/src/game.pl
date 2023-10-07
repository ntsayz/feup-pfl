initial_board([[empty, empty, empty],
               [empty, p1, empty],
               [empty, p2, empty]]).

display_board(Board) :-
    initial_board(Board). %  TODO : remove this
    % TODO : display rows and wtv



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

    % display board.
    display_board(Board),

    % TODO: Win condition.

    % Switch to the next player turn
    game_loop(Board, NextPlayer).



% predicate to switch players
switch_turn(p1, p2).
switch_turn(p2, p1).