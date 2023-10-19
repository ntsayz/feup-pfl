initial_board([
                [out,out,sr,sr,sr,sr,sr,out,out],
                [out,out,out,b_square,b_square,b_square,b_round,b_round,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out, w_square,w_square,w_square,w_round,w_round,out,out],
                [v,sr,sr,sr,sr,sr,v,v]]).
               

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