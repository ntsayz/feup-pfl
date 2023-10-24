
% ============================= BOARD MANIPULATION ============================








%  ============================= BOARD DISPLAY =================================
display_board([]).
% predicate to display each row of the board
display_board([Row|Rows]) :-
    % current row.
    display_row(Row),
    %  remaining rows.
    display_board(Rows).


display_row([]) :-
    nl.

display_row([Cell|Cells]) :-
    % current cell.
    display_cell(Cell),
    % remaining cells in the row.
    display_row(Cells).


display_cell(Cell) :-
    % the cell value to its matching character.
    cell_char(Cell, Char),
    write(Char),
    write(' | ').


cell_char('1', '1 ').
cell_char('2', '2 ').
cell_char('3', '3 ').
cell_char('4', '4 ').
cell_char('5', '5 ').
cell_char('6', '6 ').
cell_char('7', '7 ').
cell_char('8', '8 ').
cell_char('A', 'A ').
cell_char('B', 'B ').
cell_char('C', 'C ').
cell_char('D', 'D ').
cell_char('E', 'E ').
cell_char('F', 'F ').
cell_char('G', 'G ').
cell_char('H', 'H ').
cell_char('I', 'I ').
cell_char('J', 'J ').
cell_char('K', 'K ').
cell_char('L', 'L ').
cell_char('M', 'M ').
cell_char('N', 'N ').
cell_char(w_round, 'WR').
cell_char(w_square, 'WS').
cell_char(b_square, 'BA').
cell_char(b_round, 'BR').
cell_char(empty, '  ').
cell_char(corner, '  ').
cell_char(np, '  ').
cell_char(horizontal_bar, '  ').

cell_char(sr, '--').
cell_char(out, '::').

initial_board([
                [corner, 'A', 'B', 'C', 'D', 'E', 'F','G','H','I','J', corner],
                ['1', out, out, out, sr, sr, sr, sr, sr, np, np],
                ['2', out, out, out,empty,empty,empty,empty,empty, out],
                ['3', out, empty, b_square, b_square, b_square, b_round, b_round, empty,empty, out],
                ['4', out, empty, empty, empty, empty, empty, empty, empty,empty, out],
                ['5', out, out, w_square,w_square, w_square, w_round, w_round, out, out, np],
                ['6', out,out, sr, sr, sr, sr, sr, np, np, np]]).



test_board([
                [corner, 'A', 'B', 'C', 'D', 'E', 'F','G','H','I','J', corner],
                [horizontal_bar, horizontal-bar, horizontal-bar, horizontal-bar, horizontal-bar, horizontal-bar, horizontal-bar,horizontal-bar,horizontal-bar,horizontal-bar,horizontal-bar, corner],
                ['1', out, out, out, sr, sr, sr, sr, sr, np, np],
                ['2', out, out, out,empty,empty,empty,empty,empty, out],
                ['3', out, empty, b_square, b_square, b_square, b_round, b_round, empty,empty, out],
                ['4', out, empty, empty, empty, empty, empty, empty, empty,empty, out],
                ['5', out, out, w_square,w_square, w_square, w_round, w_round, out, out, np],
                ['6', out,out, sr, sr, sr, sr, sr, np, np, np]]).



