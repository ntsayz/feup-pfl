% Move format 
generate_move_format(SrcRow, SrcCol, DestRow, DestCol, MoveFormat) :-
    MoveFormat = [SrcRow, SrcCol, DestRow, DestCol].

%  generate all possible moves for a piece at a specific position
generate_moves_for_piece(Board, Player, Row, Col, Moves) :-
    findall(
        MoveFormat,
        (
            between(0, 5, DestRow),  
            between(0, 9, DestCol),  
            empty_cell(Board, DestRow-DestCol),  % Check if the destination cell is empty
            valid_move(Board, Player, Row-Col, DestRow-DestCol),  
            generate_move_format(Row, Col, DestRow, DestCol, MoveFormat)  
        ),
        Moves
    ).

% check if a position is within the bounds of the board
within_bounds(Board, Row, Col) :-
    length(Board, NumRows),
    (NumRows > 0 -> nth0(0, Board, FirstRow), length(FirstRow, NumCols); NumCols = 0),
    between(0, NumRows - 1, Row),
    between(0, NumCols - 1, Col).

%  generate all possible moves for the AI player
generate_possible_moves(Board, AIPlayer, PossibleMoves) :-
    findall(
        Move,
        (
            between(0, 5, Row), 
            between(0, 9, Col),  
            cell_has_player_piece(Board, AIPlayer, Row-Col, _Piece),
            generate_moves_for_piece(Board, AIPlayer, Row, Col, PieceMoves),
            member(Move, PieceMoves)
        ),
        PossibleMoves
    ).


% helper - filter out invalid moves
is_valid_move(Board, Player, Move) :-
    Move = [CurrRow-CurrCol, DestRow-DestCol],
    valid_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol).

%  filter out invalid moves
filter_valid_moves(Board, Player, PossibleMoves, ValidMoves) :-
    include(is_valid_move(Board, Player), PossibleMoves, ValidMoves).

%  randomly select a move from a list of valid moves
random_move(Moves, RandomMove) :-
    length(Moves, NumMoves),
    NumMoves > 0,
    random(0, NumMoves, Index),
    nth0(Index, Moves, RandomMove).





