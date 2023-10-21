

initial_board([
                [out,out,out,sr,sr,sr,sr,sr,out,out],
                [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
                [out,out,sr,sr,sr,sr,sr,out,out,out]]).
               

display_board(Board) :-
    initial_board(Board). %  TODO : remove this
    % TODO : display rows and wtv



display_board([]).
display_board([Row|Rows]) :-
    display_row(Row),
    display_board(Rows).

display_row([]) :-
    nl.
display_row([Cell|Cells]) :-
    display_cell(Cell),
    display_row(Cells).


display_cell(Cell) :-
    cell_char(Cell, Char),
    write(Char),
    write(' ').

cell_char(ws, 'WS').
cell_char(sa, 'SA').
cell_char(empty, 'E').
cell_char(null, 'N').

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