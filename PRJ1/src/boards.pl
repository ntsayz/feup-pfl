
% ============================= BOARD States to use ============================


initial_board2([
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,b_round,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ] ).



%  ============================= BOARD DISPLAY =================================

% cell_code(?CellValue, ?Code)
% Tradução do valor de uma célula para o código ASCII decimal da sua representação no ecrã.
cell_code(w_round, 119).
cell_code(w_square, 87).
cell_code(b_square, 66).
cell_code(b_round, 98).
cell_code(empty,32 ).
cell_code(sr, 45).
cell_code(out, 58).




% display_board(+Board)
% Predicado para vizualizar o Board tendo em conta tamanho de linhas e colunas escolhidas do mesmo e display da Âncora
display_board([H| BoardTail]) :-
  length(H, SizeCollumns),
  display_board_header(SizeCollumns), nl,
  S is SizeCollumns*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  display_board_lines([H| BoardTail], 1), nl, nl.

% display_board_header(+Size)
% Visualização do cabeçalho do tabuleiro (numeração das colunas).
display_board_header(Size) :-
  write('     '),
  display_columns(Size, 65). %65



%second version

% display_board_lines(+Board, +LineNumber)
% Visualização das linhas que compõe o tabuleiro.
display_board_lines([], _).
display_board_lines([Line | T], N) :-
    RowDisplay is N,
    write(' '), write(RowDisplay), write(' |'), display_line(Line,RowDisplay), nl,
    length(Line, Size), S is Size*4 + 4,
    write('   '), format('~`-t~*|~n', [S]),
    N1 is N + 1,
    display_board_lines(T, N1).



  % display_line(+BoardLine, +RowNumber)
% Visualização de uma linha do tabuleiro com informação da linha atual.
display_line(BoardLine, RowNumber) :-
    display_line(BoardLine, RowNumber, 0).

% display_line(+BoardLine, +RowNumber, +ColNumber)
% Visualização de uma linha do tabuleiro com informação da linha e coluna atual.
%Caso em que é para fazer display da peça especial âncora
display_line([], _, _).
display_line([_Cell | T], RowNumber, ColNumber) :-
    AnchorRow is RowNumber -1,
    anchor_piece(AnchorRow-ColNumber,_AnyPlayer),

    write(' '), 
    put_code(64), write(' |'), 
    NextColNumber is ColNumber + 1,
    display_line(T, RowNumber, NextColNumber).
%Caso geral para fazer display do board e peças no mesmo
display_line([Cell | T], RowNumber, ColNumber) :-

    write(' '), cell_code(Cell, Code),
    put_code(Code), write(' |'), 
    NextColNumber is ColNumber + 1,
    display_line(T, RowNumber, NextColNumber).


%draw banner for the game 




% Predicate to draw a square banner with a title and two stick figures.
draw_banner(Columns) :-
    Title = "Push-Fight Game !  ",
    BannerHeight is Columns,  % Calculate the height of the banner
    StickManHeight is BannerHeight // 2,  % Calculate the height of the
    Middle is Columns // 2 + 6,
    % Calculate padding for title and stick figures
    length(Title, TitleLength),
    TitlePad is (Columns - TitleLength) // 2,
    % Draw the banner
    draw_top_bottom(Columns),
    draw_centered_text(Title, Columns, TitlePad),
    draw_empty_line(Columns),
    print_stick_man_line(Middle, Columns,StickManHeight),
    draw_top_bottom(Columns).

% Helper predicate to draw the top and bottom border of the banner.
draw_top_bottom(Columns) :-
    fill_line(':', Columns),
    nl.

% Helper predicate to draw an empty line with colons on the sides.
draw_empty_line(Columns) :-
    write(':'),
    fill_line(' ', Columns - 2),
    write(':'),
    nl.

% Helper predicate to draw centered text within the banner.
draw_centered_text(Text, Columns, _Pad) :-
    length(Text, TextLength),
    SidePad is (Columns - TextLength) // 2 - 1,
    write(':'),
    fill_line(' ', SidePad), 
    format('~s~n', [Text]), 
    fill_line(' ', SidePad), 
    write(':'), 
    nl.

% Helper predicate to fill a line with a specific character.
fill_line(Char, Length) :-
    ( Length > 0 ->
        write(Char),
        NewLength is Length - 1,
        fill_line(Char, NewLength)
    ; true
    ).

% print_stick_men(Columns, StickManHeight) :-
%     Middle is Columns // 2,
%     print_stick_man_line(Middle, StickManHeight), nl,
%     print_push_fight_game(Middle), nl,
%     print_stick_man_line(Middle, StickManHeight), nl.

print_stick_man_line(_Middle, _Collumns,0).
print_stick_man_line(Middle, Collumns,StickManHeight) :-
    StickManHeight > 0,
    Spaces is Middle - 3,  % Adjust for stick figure width
    print_row(' ', Spaces),
    write('O  O'),
    print_row(' ', Spaces), nl,
    print_row(' ', Spaces),
    write('/|\\/|\\'),
    print_row(' ', Spaces), nl,
    print_row(' ', Spaces),
    write('/ \\/ \\'),
    print_row(' ', Spaces), nl,
    draw_top_bottom(Collumns), nl,
    NewStickManHeight is StickManHeight - 3,
    NewMidle is Middle -3,
    print_stick_man_line(NewMidle, Collumns,NewStickManHeight).






print_row(Char, 0) :- 
write(Char).

print_row(Char, Length) :-
    Length > 0,
    write(Char),
    NewLength is Length - 1,
    print_row(' ', NewLength).