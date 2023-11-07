
% ============================= BOARD States to use ============================


initial_board1([
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,empty,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,empty,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ] ).

initial_board2([
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,empty,b_square,w_square,empty,empty,out,out],
    [out,empty,empty,b_round,empty,b_square,w_round,w_square,empty,out],
    [out,empty,empty,empty,b_square,w_square,empty,empty,empty,out],
    [out,out,out,empty,b_round,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ] ).  

% ============================= Predicates to manipulate and create different Boards with different sizes, number of Pieces etc ============================
%Predicate to create board with given SizeRow and SizeCol


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

% display_columns(+Size, +Code)
% Computação e visualização das colunas do tabuleiro de acordo com o seu tamanho.
display_columns(0, _):-!.
display_columns(Size, Code) :- Size > 0,
  put_code(Code), write('   '),
  S is Size-1, C is Code+1,
  display_columns(S, C).
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

