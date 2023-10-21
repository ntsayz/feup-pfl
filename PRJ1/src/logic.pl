
:- use_module(library(lists)).
:- use_module(library(between)).



player_pieces(player1, w_square).
player_pieces(player1, w_round).
player_pieces(player2, b_square).
player_pieces(player2, b_round).


%anchor_piece(+Player,+Board,+CellPiece)/3
%Determinar que peça do Player no Board em CellPiece:Row-Col fica com âncora
%anchor_piece(Player,Board,Row-Col):-

%Size of Board/Matrix will always be quadratic axb ( different number of rows and Cols and every row have the same number of Col)
%size of the board maybe can change by options ?
:- dynamic size_row/1.
:- dynamic size_col/1.

size_row(6).
size_col(10).


%collumn(+Letter,-Number)
%Traduz letras utilizadas para identificar colunas no board graficamente em Number de Colunas para trabalhar com matrizes e listas
collumn('a',0).
collumn('b',1).
collumn('c',2).
collumn('d',3).
collumn('e',4).
collumn('f',5).
collumn('g',6).
collumn('h',7).
collumn('i',8).
collumn('j',9).

%set_anchor(+Cell,+Player,+Board)
%Determina que peça de um Player no Board localizada em Cell:Row-Coll é marcada com a âncora 
%Aqui teremos de usar assert/assertz?

%cell_belongs_board(+Cell)
%Verificar se coordenadas de uma determinada célula estão dentro dos limites do tabuleiro
cell_belongs_board(Board,Row-Col):-
    length(Board,SizeRows),
    SizeRows>0,
    nth0(Row,Board,BoardRow), %get one of the rows/list of the board
    length(BoardRow,SizeCols),
    SizeCols>0,

    Row >=0, 
    Row < SizeRows, 
    Col >=0, 
    Col < SizeCols.

%board_element(+Board,+Cell,-Element)
%Obter o Element no Board numa determinada posição: Row-Col
board_element(Board,Row-Col,Element):-
    cell_belongs_board(Board,Row-Col),

    nth0(Row,Board,BoardRow),
    nth0(Col,BoardRow,Element).


%empty_cell(+Cell,+Board)
%Verifica se célula Cell:Row-Col no Board está ou não vazia.
empty_cell(Board,Row-Col):-

    board_element(Board,Row-Col,empty).
    

%cell_has_player_piece(+Board,+Player,+CellPiece,Piece)
%Verifica se a célula CellPiece:Row-Col no Board possui uma peça e pertence ao jogador Player
cell_has_player_piece(Board,Player,Row-Col,Piece):-

    board_element(Board,Row-Col,Piece),
    player_pieces(Player,Piece).

%player_square_piece(+Board,+Player,+Row-Col)
%Verifica se peça em Cell:Row-Col é uma square piece do Player
player_square_piece(Board,Player,Row-Col):-
    cell_has_player_piece(Board,Player,Row-Col,Piece),

    player_pieces(Player,Piece),
    (Piece = b_square ; Piece = w_square ).

%player_round_piece(+Board,+Player,+Row-Col)
%Verifica se peça em Cell:Row-Col é uma square piece do Player
player_round_piece(Board,Player,Row-Col):-
    cell_has_player_piece(Board,Player,Row-Col,Piece),

    player_pieces(Player,Piece),
    (Piece = b_round ; Piece = w_round ) .


%valid_player_round_piece(+Cell,+Player,+Board)
%Verifica se peça em Cell:Row-Col é uma round piece do Player no Board


%valid_move(+Orig,+Dest,+Board)
%Verifica se mover peça na célula do Board identificada por Orig:Row1-Cell1 é possivel mover para Dest:Row2-Cell2 


%valid_push(+Orig, +Dest,+Board)
%Verifica se peça na célula Orig:Row1-Coll2 pode efetuar um movimento push a peça/peças que estão horizontal/vertical com peça em Orig no Board


%verify_anchor(+Cell,+Board)
%Verifica se peça na célula Cell:Row-Col do Board tem âncora ou não



%push(+Orig,+Dest,?Board)
%Efetua o movimento push da peça Orig:Row-Col para Dest:Row-Col no Board