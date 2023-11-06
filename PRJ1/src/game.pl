:-use_module(library(lists)).
:-use_module(library(random)).
:- consult('menu.pl').

% PvP
start_game(1) :-
    initial_board2(Board),
    game_loop(Board, player1).

% PvCR
start_game(2):-
    initial_board2(Board),
    game_loop(Board, player1, ai2_rand).

% CvC
start_game(3) :-
    initial_board2(Board),
    game_loop_cvc(Board, ai1).


% predicate to switch players
switch_turn(CurrentPlayer, NextPlayer):-
    change_player(CurrentPlayer, NextPlayer).

%Para Player vs Player
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


game_loop(Board, player1, ai2_rand):-
    player_turn(Board, player1, 1, BoardAfterMoves),
    (   game_over(BoardAfterMoves-player1, Winner) ->
        
        announce_winner(Winner),
        main_menu
        ;   % Otherwise, attempt to push and continue the game.
        (push(BoardAfterMoves, player1, FinalTurnBoard),
            %Check if player after move, can push
            (   game_over(FinalTurnBoard-player1, Winner) ->
                
                announce_winner(Winner),
                main_menu
            ;   % If the game is not over, switch players and continue the game loop.
                game_loop(FinalTurnBoard, ai2_rand, player1)
            )
            ;   
            
            handle_no_push_possible(player1)
        )
    ).




game_loop(Board, ai2_rand, player1):-

    ai_random_move_turn(Board, player2, RandomGameState,MoveCount),
    display_board(RandomGameState),
    display_turn_info(ai2_rand, MoveCount),
    (   game_over(RandomGameState-player2, Winner) ->
        announce_winner(Winner),
        main_menu
        ;   % Otherwise, attempt to push and continue the game.
       ( ai_random_push(RandomGameState, player2, FinalTurnBoard),
            display_board(FinalTurnBoard),
            display_turn_info(ai2_rand, 3),
            
            %Check if player after move, can push
        (   game_over(FinalTurnBoard-player2, Winner) ->
            announce_winner(Winner),
            main_menu
        ;   % If the game is not over, switch players and continue the game loop.
            game_loop(FinalTurnBoard,player1, ai2_rand)

        )
        ;
        handle_no_push_possible(player2)

       )
    ).



announce_winner(Winner) :-
    change_anchor_piece(null-null, noplayer),
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
            write('Invalid move! Try again.'), nl, nl, nl,nl,nl,nl,nl,nl,
            player_turn(Board, Player, MoveNum, FinalTurnBoard)
        )
    ).


push(Board, Player, FinalPushGameState):-
    
    display_board(Board),
    write('Player Turn: '), write(Player), nl,
    write('Push-Move number: '), write(3), nl,
    write('Mandatory push!'), nl,nl,nl,nl,nl,nl,

    read_user_input(Move), nl,
    convert_to_index(Board, Move, [PieceCol,PieceRow, PushCol,PushRow], _Pieces),

    (make_push(Board, Player, PieceRow-PieceCol, PushRow-PushCol, FinalPushGameState) ->
        true
        ;
        
        write('Invalid push! Try again.'), nl,nl,
        
        push(Board, Player, FinalPushGameState)
        ).


% update_board(+Board, +Player, +MoveType, -UpdatedBoard)/4
% ==============================================================================


