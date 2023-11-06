

%Random Ai : Contudo, não por default, nunca irá escolher jogadas que deliberadamente o fariam perder ou não conseguir fazer push

ai_random_move_turn(GameState, Player, RandomGameState,MoveCount):-
    find_move_game_states(GameState, Player, ListOfNewGameStates1),
    random_member(RandGameState1,  ListOfNewGameStates1),
    find_move_game_states(RandGameState1, Player, ListOfNewGameStates2), %2ª jogada
    random_member(RandGameState2, ListOfNewGameStates2),
    random_member(MoveCount-RandomGameState,[0-GameState,1-RandGameState1,2-RandGameState2]).

    
ai_random_push(GameState, Player, RandPushGameState):-
    find_push_game_states(GameState, Player, ListOfNewGameStates),
    random_member(RandPushGameState, ListOfNewGameStates).


%Advanced Ai
equal_value(BestValue, Value-_) :-
    BestValue == Value.

ai_move_game_state(GameState, Player, Val-FinalGameStateWithRand):-
    find_move_game_states(GameState, Player, ListOfNewGameStates),
    change_player(Player, Opponent),
   
    evaluate_game_state_list(ListOfNewGameStates, Opponent, SortedListGameStateValue),
    
    SortedListGameStateValue = [BestValue-_|_],
    
    include(equal_value(BestValue), SortedListGameStateValue, BestGameStates),
    
    random_member(Val-FinalGameStateWithRand, BestGameStates).
    %check also mora Val-FinalGameState with same Val and choose using random choice


count_moves(Val0, Val1, Val2, MoveCount):-
    (Val0 =:= Val1, Val1 =:= Val2 -> MoveCount = 0;
     Val0 =\= Val1, Val1 =:= Val2 -> MoveCount = 1;
     Val0 =\= Val1, Val1 =\= Val2 -> MoveCount = 2).

ai_move_turn(GameState, Player, Val-FinalGameState,MoveCount):-
    evaluate_game_state_list([GameState], Player, [Val0-GameState]),

    ai_move_game_state(GameState, Player, Val1-FirstMoveGameState),
    ai_move_game_state(FirstMoveGameState, Player, Val2-SecondGameState),
    keysort([Val0-GameState, Val1-FirstMoveGameState, Val2-SecondGameState ], ListTurnMoves),
    ListTurnMoves = [BestValue-_|_],
    include(equal_value(BestValue), ListTurnMoves, BestMoveGameStates),
    random_member(Val-FinalGameState, BestMoveGameStates),
    count_moves(Val0, Val1, Val2, MoveCount).
%Predicate of advanced ai to do a push_move
ai_push_move(GameState, Player, Val-FinalGameState):-

    find_push_game_states(GameState, Player, ListOfNewGameStates),
    change_player(Player, Opponent),
    evaluate_game_state_list(ListOfNewGameStates, Opponent, SortedListGameStateValue),
    SortedListGameStateValue = [BestValue-_|_],
    include(equal_value(BestValue), SortedListGameStateValue, BestPushGameStates),
    random_member(Val-FinalGameState, BestPushGameStates).







