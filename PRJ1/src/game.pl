
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