:-use_module(library(lists)).
:-use_module(library(random)).
start_game :-
    initial_board2(Board),
    game_loop(Board, player1).


% predicate to switch players
switch_turn(CurrentPlayer, NextPlayer):-
    change_player(CurrentPlayer, NextPlayer).

game_loop(Board, CurrentPlayer) :-
    player_turn(Board, CurrentPlayer, 1, BoardAfterMoves),
   
    push(BoardAfterMoves, CurrentPlayer, FinalTurnBoard),
 
    switch_turn(CurrentPlayer, NextPlayer),
    game_loop(FinalTurnBoard, NextPlayer).

%  deals with player movement
player_turn(Board, Player, MoveNum, FinalTurnBoard) :-
    MoveNum =< 3,
    display_board(Board),
    (MoveNum ==3 -> 
        write('Player Turn: '), write(Player), nl, 
        write('Push Phase: '), nl,nl
      ;
        write('Player Turn: '), write(Player), nl,
        write('MoveNumber: '), write(MoveNum), nl,
        write('Optional Move!'),nl,nl
       ),

    (MoveNum < 3 ->
        read_user_input(Move), nl,
        (Move = xxxx -> % SINCE MOVES ARE NOT OBLIGATORY, IF THE USER INPUTS xxxx, THE MOVE IS SKIPPED
            NextMoveNum is MoveNum + 1,
            player_turn(Board, Player, NextMoveNum, FinalTurnBoard);
            
            convert_to_index(Board, Move, [CurrCol,CurrRow, DestCol,DestRow], _Pieces),
            
            make_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, NewGameState) -> % determine if move is valid
        
                NextMoveNum is MoveNum + 1, % increment move number
                player_turn(NewGameState, Player, NextMoveNum, FinalTurnBoard);
            write('Invalid move! Try again.'), nl,nl, % if invalid, display error message
            player_turn(Board, Player, MoveNum, FinalTurnBoard))
            ; FinalTurnBoard= Board 
        ).
            
        




%valid_move1(Board, Player, Move, TempBoard):-
%    true.


read_user_input(Move):-
    read(Input),    %d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, _ListOfMoves).


parse_input([X1, Y1, X2, Y2], Move, _ListOfMoves) :-
    (   [X1, Y1, X2, Y2] = ['x', 'x', 'x', 'x'] ->
        Move = xxxx
    ;   char_code(X1, X1Code),
        char_code(Y1, Y1Code),
        char_code(X2, X2Code),
        char_code(Y2, Y2Code),
        % verifica se o input é válido. se a char for um número ou uma letra dependendo da posição
        X1Code >= 97,
        X1Code =< 104,
        Y1Code >= 49,
        Y1Code =< 56,
        X2Code >= 97,
        X2Code =< 104,
        Y2Code >= 49,
        Y2Code =< 56,
        Move = [X1Code, Y1Code, X2Code, Y2Code]
    ).

convert_to_index(Board, Move, Indexes, Pieces) :-
    Move = [X1, Y1, X2, Y2],
    
    % % debugging
    % write('Move: '), write(Move), nl,
    
    % converting char to index para account pelo offset do header
    X1Index is X1 - 97 , 
    Y1Index is Y1 - 48 - 1,
    X2Index is X2 - 97 , 
    Y2Index is Y2 - 48 - 1, 
    
    % % debugging
    % write('Real Indexes Row-Col: '), write([Y1Index, X1Index, Y2Index, X2Index]), nl,
    
    % getting pieces
    nth0(Y1Index, Board, Row1),
    nth0(X1Index, Row1, Piece1),
    nth0(Y2Index, Board, Row2),
    nth0(X2Index, Row2, Piece2),
    
    % Info for Start Position: Element/Piece to Final Position: Element/Piece
    write('Elements/Pieces: Piece -> Piece|Element : '), write(Piece1),write(' -> '), write(Piece2), nl, nl,
    

    Indexes = [X1Index, Y1Index, X2Index, Y2Index],
    Pieces = [Piece1, Piece2].

% temporario para testar
move_piece(Board, [X1, Y1, X2, Y2], UpdatedBoard) :-
    nth0(Y1, Board, Row1),
    nth0(X1, Row1, Piece),
    update_board(Board, X1, Y1, empty, TempBoard),
    update_board(TempBoard, X2, Y2, Piece, UpdatedBoard).

update_board(Board, X, Y, Value, UpdatedBoard) :-
    nth0(Y, Board, Row),
    replace_element(Row, X, Value, UpdatedRow),
    replace_element(Board, Y, UpdatedRow, UpdatedBoard).

replace_element(List, Index, Value, UpdatedList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, UpdatedList, Value, Rest).

push(Board, Player, FinalPushGameState):-
    display_board(Board),
    write('Player Turn: '), write(Player), nl,
    write('Push-Move number: '), write(3), nl,
    write('Mandatory push!'), nl,nl,
   
    read_user_input(Move), nl,
    convert_to_index(Board, Move, [PieceCol,PieceRow, PushCol,PushRow], _Pieces),
   
    (make_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, FinalPushGameState) ->
        !
        ;
        
        write('Invalid push! Try again.'), nl,nl,
        
        push(Board, Player, FinalPushGameState)
        ).

% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4
% ==============================================================================


