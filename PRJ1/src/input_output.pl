

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
% Predicado para vizualizar o Board tendo em conta tamanho de linhas e colunas escolhidas do mesmo 
display_boardv2([H| BoardTail]) :-
  length(H, SizeCollumns),
  display_board_header(SizeCollumns), nl,
  S is SizeCollumns*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  display_board_lines2([H| BoardTail], 1), nl, nl.

% display_board_header(+Size)
% Visualização do cabeçalho do tabuleiro (numeração das colunas).
display_board_header(Size) :-
  write('     '),
  display_columns(Size, 65). %65



  % display_columns(+Size, +Code)
% Computação e visualização das colunas do tabuleiro de acordo com o seu tamanho.
display_columns(0, _):-!.
display_columns(Size, Code) :- Size > 0,
  put_code(Code), write('   '),
  S is Size-1, C is Code+1,
  display_columns(S, C).



  % display_board_lines(+Board, +LineNumber)
% Visualização das linhas que compõe o tabuleiro.
display_board_lines([], _).
display_board_lines([Line | T], N) :-
  write(' '), write(N), write(' |'), display_line(Line), nl,
  length(Line, Size), S is Size*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  N1 is N + 1,
  display_board_lines(T, N1).

% display_line(+BoardLine)
% Visualização de uma linha do tabuleiro.
display_line([]).
display_line([Cell | T]) :-
  write(' '), cell_code(Cell, Code),
  put_code(Code), write(' |'), 
  display_line(T).

%second version

% display_board_lines(+Board, +LineNumber)
% Visualização das linhas que compõe o tabuleiro.
display_board_lines2([], _).
display_board_lines2([Line | T], N) :-
    RowDisplay is N,
    write(' '), write(RowDisplay), write(' |'), display_line2(Line,RowDisplay), nl,
    length(Line, Size), S is Size*4 + 4,
    write('   '), format('~`-t~*|~n', [S]),
    N1 is N + 1,
    display_board_lines2(T, N1).



  % display_line(+BoardLine, +RowNumber)
% Visualização de uma linha do tabuleiro com informação da linha atual.
display_line2(BoardLine, RowNumber) :-
    display_line2(BoardLine, RowNumber, 0).

% display_line(+BoardLine, +RowNumber, +ColNumber)
% Visualização de uma linha do tabuleiro com informação da linha e coluna atual.
display_line2([], _, _).
display_line2([_Cell | T], RowNumber, ColNumber) :-
    AnchorRow is RowNumber -1,
    anchor_piece(AnchorRow-ColNumber,_AnyPlayer),

    write(' '), 
    put_code(64), write(' |'), % Here you can add the row and column information if needed
    NextColNumber is ColNumber + 1,
    display_line2(T, RowNumber, NextColNumber).
display_line2([Cell | T], RowNumber, ColNumber) :-

    write(' '), cell_code(Cell, Code),
    put_code(Code), write(' |'), % Here you can add the row and column information if needed
    NextColNumber is ColNumber + 1,
    display_line2(T, RowNumber, NextColNumber).