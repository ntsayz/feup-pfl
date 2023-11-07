
% ============================= BOARD States to use ============================


initial_board2([
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,empty,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,empty,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]).

% ============================= Predicates to manipulate and create different Boards with different sizes, number of Pieces etc ============================
%Predicate to create board with given SizeRow and SizeCol


%Predicate to fill board of  given SizeRow and SizeCol with out, sr, empty, b_round,b_square w_round,w_square
% Choose simple rules to add more or less pieces type of lines and number of sr to given board of Size SizeRows x SizeCols
%Rules: For NewSizeRow X NewSizeCol -> NewNumbOfPieces = (NewSizeRow X NewSizeCol X 12 )/ 60 
% 12 = DefaultNumbOfPieces e 60 = 8 X 4 ( lines * max playable Cols ) ( default board size) -> 
% Change change_size_board and number_pieces : -> Para board com 8 x 16, teremos (16*8*12 )/ 24 = 128*12 / 24 = 64 pieces for the new board
% Decide how the lines with sr, and the shorter and longer lines will be
% sr : NumbOfSr= ((NewCol/DefaultCol)*5 ), NumbOfShortLinesEmptyCells = ((NewCol/DefaultCol)*5 ) andNumberOfLargeLinesEmptyCells= ((NewCol/DefaultCol)*8 )
% 5 is the number of sr and empty cells in the shorter lines , 8 number of empty on larger lines in default game board of 6 X 10, lines wir sr with equal number of sr then shortlines
% Use always On default values of board game: + 4 of 6 Row and + 8 of 10 Col  -> Eg: 6+4 X 10+8 = NewBoard = 10 X 18 etc ...
% From 10 Rows, only 8 are playable = 4 short lines and 4 long lines, short lines with 10 empty cells ( 5*2) and long lines with ( 8*2) empty cells
% The lines with sr will have: 5*2 side rails (rl)  
% Number of short lines = NewSizeRow * 2 / 6, Long lines = NewSizeRow * 2 / 6
% Eg: 12 X 20 -> ShorLines = 4, LongLines = 4, two lines with sr
%Allign sr lines with shor lines, and the longlines also aliggned, then out cells will fill untill: we have a board with 20 Col ( with 16 playable collumns max)
% And the number of Rows will be 4 * n + 2 always, in this example: 6+4 = 4*2 + 2

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

