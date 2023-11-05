:-use_module(library(lists)).
:-use_module(library(random)).

% PvP
start_game(1) :-
    initial_board2(Board),
    game_loop(Board, player1).

% PvC
start_game(2) :-
    initial_board3(Board),
    game_loop_pvc(Board, player1).

% CvC
start_game(3) :-
    initial_board2(Board),
    game_loop_cvc(Board, ai1).


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
            %check if Player can Push any Piece, if not he looses the game

        % push(Board, Player, FinalTurnBoard)).  




game_loop_pvc(Board, CurrentPlayer) :-
    (CurrentPlayer == player1 -> 
        player_turn(Board, CurrentPlayer, 1, NewBoard);  % Player's turn
        ai_turn(Board, CurrentPlayer, NewBoard)  % AI's turn
    ),
    push(NewBoard, CurrentPlayer, FinalTurnBoard),  % Handle the push phase
    switch_turn(CurrentPlayer, NextPlayer),
    game_loop_pvc(FinalTurnBoard, NextPlayer).

game_loop_cvc(Board, CurrentPlayer) :-
    ai_turn(Board, CurrentPlayer, NewBoard),  % AI's turn
    push(NewBoard, CurrentPlayer, FinalTurnBoard),  % Handle the push phase
    switch_turn(CurrentPlayer, NextPlayer),
    game_loop_cvc(FinalTurnBoard, NextPlayer).


%  handle the AI's turn
ai_turn(Board, AIPlayer, NewBoard) :-
    display_board(Board),
    generate_possible_moves(Board, AIPlayer, PossibleMoves),
    filter_valid_moves(Board, AIPlayer, PossibleMoves, ValidMoves),
    random_move(ValidMoves, SelectedMove),

    convert_to_format(SelectedMove, FormattedMove),
    print_chosen_move(FormattedMove),

    apply_move(Board, AIPlayer, SelectedMove, NewBoard).







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
    ( Player == ai -> 
        % If the player is AI, skip the push phase for now
        write('AI push phase skipped for now.'), nl,
        FinalPushGameState = Board  % Assuming you just want to keep the board state unchanged for now
        ;
        % If the player is not AI, proceed with the push phase as usual
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
        )
    ).


% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4
% ==============================================================================


