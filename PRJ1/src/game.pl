:-use_module(library(lists)).
:-use_module(library(random)).
:- consult('menu.pl').

% PvP
start_game(1) :-
    initial_board2(Board),
    game_loop(Board, player1).

% PvC
start_game(2) :-
    initial_board3(Board),
    game_loop_pvc_random(Board, player1).

% CvC
start_game(3) :-
    initial_board2(Board),
    game_loop_cvc(Board, ai1).


% predicate to switch players
switch_turn(CurrentPlayer, NextPlayer):-
    change_player(CurrentPlayer, NextPlayer).


game_loop(Board, CurrentPlayer) :-
    player_turn(Board, CurrentPlayer, 1, BoardAfterMoves),
    (   game_over(BoardAfterMoves-CurrentPlayer, Winner) ->
        % If the game is over after player_turn, announce the winner and return to the main menu.
        announce_winner(Winner),
        main_menu
    ;   % Otherwise, attempt to push and continue the game.
        (   push(BoardAfterMoves, CurrentPlayer, FinalTurnBoard),
            % If push is successful, check for game over condition again.
            (   game_over(FinalTurnBoard-CurrentPlayer, Winner) ->
                % If the game is over after push, announce the winner and return to the main menu.
                announce_winner(Winner),
                main_menu
            ;   % If the game is not over, switch players and continue the game loop.
                switch_turn(CurrentPlayer, NextPlayer),
                game_loop(FinalTurnBoard, NextPlayer)
            )
        ;   % If push fails, it means the current player cannot move. The game is over.
            % You need to define how to handle this case. Maybe the other player wins, or it's a draw.
            handle_no_push_possible(CurrentPlayer)
        )
    ).

announce_winner(Winner) :-
    format('#~`#t~*|#~n', [43]),
    format('Game over! The winner is ~w !!!!~n', [Winner]), nl,
    write('Another Game for revenge ?!?'),nl,
    format('#~`#t~*|#~n', [43]), nl.

handle_no_push_possible(CurrentPlayer) :-
    change_player(CurrentPlayer, Opponent),
    
    announce_winner(Opponent),
    main_menu.


    

%  Predicado que lida com input e as duas jogadas opcionais
player_turn(Board, Player, MoveNum, FinalTurnBoard) :-
    MoveNum =< 3,
    display_board(Board),
    display_turn_info(Player, MoveNum),
    (MoveNum < 3 -> handle_move(Board, Player, MoveNum, FinalTurnBoard) ; FinalTurnBoard = Board).

display_turn_info(Player, 3) :-
    write('Player Turn: '), write(Player), nl, 
    write('Push Phase: '), nl, nl.

display_turn_info(Player, MoveNum) :-
    MoveNum < 3,
    write('Player Turn: '), write(Player), nl,
    write('MoveNumber: '), write(MoveNum), nl,
    write('Optional Move!'), nl, nl.

handle_move(Board, Player, MoveNum, FinalTurnBoard) :-
    read_user_input(Move), nl,
    (Move = xxxx -> 
        NextMoveNum is MoveNum + 1,
        player_turn(Board, Player, NextMoveNum, FinalTurnBoard)
    ;
        convert_to_index(Board, Move, [CurrCol, CurrRow, DestCol, DestRow], _Pieces),
        (make_move(Board, Player, CurrRow-CurrCol, DestRow-DestCol, NewGameState) ->
            NextMoveNum is MoveNum + 1,
            player_turn(NewGameState, Player, NextMoveNum, FinalTurnBoard)
        ;
            write('Invalid move! Try again.'), nl, nl,
            player_turn(Board, Player, MoveNum, FinalTurnBoard)
        )
    ).

            
        



game_loop_pvc_random(Board, CurrentPlayer) :-
    (CurrentPlayer == player1 -> 
        player_turn(Board, CurrentPlayer, 1, NewBoard);  % Player's turn
        ai_turn(Board, CurrentPlayer, NewBoard)  % AI's turn
    ),
    push(NewBoard, CurrentPlayer, FinalTurnBoard),  % Handle the push phase
    switch_turn(CurrentPlayer, NextPlayer),
    game_loop_pvc_random(FinalTurnBoard, NextPlayer).

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


