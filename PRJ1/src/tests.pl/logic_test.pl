

%Test logic.pl predicates
:- consult('../logic.pl').

board_logic_test(logic1,
    [[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]).

board_logic_test(standard_initial_positions, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,empty,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
).
board_logic_test(standard_initial_positions2, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,empty,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,empty,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
).
board_logic_test(trapped_players_draw, [
    [out, out, out, sr, sr, sr, sr, sr, out, out],
    [out, out, out, w_round, b_round, b_square, empty, empty, out, out],
    [out, b_round, b_square, b_round, b_square, w_square, empty, empty, empty, out],
    [out, empty, empty, w_round, w_square, b_square, empty, empty, empty, out],
    [out, out, out, empty, w_square, w_round, b_round, empty, out, out],
    [out, out, sr, sr, sr, sr, sr, out, out, out]
]).

board_logic_test(game_over,
    [[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,empty,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]).

%newstate after move on test board logic1: 1-3 -> 2-3
board_logic_test(make_move_state1,[[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,empty,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]
).

board_logic_test(make_move_state2,[[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, b_square,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]
).
board_logic_test(make_move_final_state,[[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,empty,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, b_square,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]
).

board_logic_test(possible_push_positions, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,w_round,b_round,b_square,empty,empty,out,out],
    [out,b_round,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
).
board_logic_test(possible_push_positionsNewStatePush, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,w_round,b_round,b_square,empty,empty,empty,out,out],
    [out,b_round,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
).
board_logic_test(possible_push_positionsNewStatePush2, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,w_round,b_round,b_square,empty,empty,empty,out,out],
    [out,empty,b_square,b_round,b_square,w_square,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
).

board_logic_test(cant_push_board, [
    [out, out, out, sr, sr, sr, sr, sr, out, out],
    [out, out, out, w_round, b_round, b_square, empty, empty, out, out],
    [out, empty, b_square, b_round, b_square, empty, empty, w_square, empty, out],
    [out, w_round, empty, empty, w_square, empty, empty, empty, empty, out],
    [out, out, out, empty, w_square, empty, empty, empty, out, out],
    [out, out, sr, sr, sr, sr, sr, out, out, out]
]

).


% Lida com caso em que negamos resultado de predicados que "falham para determinados casos corretamente"
run_test(\+ NegatedCall) :-
    NegatedCall =.. [Functor|Args],
    length(Args, Arity), 
    format('Test ~w/~d: ', [Functor, Arity]),
    (   call(\+ NegatedCall)
    ->  write('passed'), nl
    ;   write('failed'), nl
    ).

% Caso em que predicado returna simplesmente True
run_test(PredicateCall) :-
    PredicateCall =.. [Functor|Args],
    length(Args, Arity), 
    format('Test ~w/~d: ', [Functor,Arity]),
    (   call(PredicateCall)
    ->  write('passed'), nl
    ;   write('failed'), nl
    ).




test_board_basic_predicates :-
    board_logic_test(logic1,Board), 

    run_test(board_element(Board, 1-3,b_square)),
    run_test(board_element(Board, 0-9,out)),
    run_test(\+ board_element(Board, 0-10,out)), %board_element fails as inteded, so it passed the test, that's why we use negation

    run_test(\+ cell_belongs_board(Board, 0-11)),
    run_test(cell_belongs_board(Board, 0-0)),
    run_test(cell_belongs_board(Board, 5-9)),

    run_test(empty_cell(Board, 2-1)),
    run_test(\+ empty_cell(Board, 1-3)).

test_board_player_predicates :-
    
    board_logic_test(logic1,Board),

    run_test(cell_has_player_piece(Board,player2, 1-4, _Piece1)), %need different argument for ?Piece arguments or will have errors testing
    run_test(cell_has_player_piece(Board,player1, 4-2, _Piece2)),
    run_test(\+ cell_has_player_piece(Board,player1, 3-5, _Piece3)), % dont have piece of any player
    run_test(\+ cell_has_player_piece(Board,player1, 0-4, _Piece4)),


    run_test(player_square_piece(Board,player1, 4-2)),
    run_test(player_square_piece(Board,player2, 1-3)),
    run_test(\+ player_square_piece(Board,player1, 3-5)),

    run_test(player_round_piece(Board,player1, 4-5)),
    run_test(player_round_piece(Board,player2, 1-6)),
    run_test(\+ player_round_piece(Board,player1, 3-7)).

test_player_moves_predicates:- 
    
    board_logic_test(logic1,Board),

    %Testing the 4 possible moves from a given cell to another cell, moving only one cell 
    run_test(possible_move(Board,1-3, 2-3, down)), % down
    run_test(possible_move(Board,1-3, 1-4, right)), % right
    run_test(possible_move(Board, 1-4, 1-3, left)), % left
    run_test(possible_move(Board, 2-3, 1-3, up)), % up
    %Testing if a move from CurrentPos to DestPosition, using more then one cell is valid
    run_test(\+valid_move(Board, player2, 1-3, 1-3)), %we assume move 0 number of cells is not a valid_move
    run_test(\+valid_move(Board, player2, 1-3, 4-3)), % 4-3 have a piece of player1 (w_square)
    run_test(valid_move(Board, player2, 1-3, 3-5)), % right-down move
    run_test(valid_move(Board, player2, 1-7, 3-3)), % left-down move
    run_test(valid_move(Board, player1, 4-3, 3-5)), % right-up move
    run_test(valid_move(Board, player1, 4-6, 3-3)), % left-up move
    run_test(\+valid_move(Board, player2, 3-3, 1-5)). % no piece to move in 3-3 and 1-5 have a piece

test_player_complete_moves_predicates:-
    
    board_logic_test(logic1, Board), 
    board_logic_test(make_move_state1, Board2),
    board_logic_test(make_move_state2, Board3),
    board_logic_test(make_move_final_state, FinalBoardState),
    board_logic_test(possible_push_positions,Board4),

    run_test(change_board_value(Board, 1-3, empty, Board2)),
    run_test(change_board_value(Board, 2-3, b_square, Board3)),
    run_test(make_move(Board, player2, 1-3, 2-3, FinalBoardState)),
    run_test(make_move(Board4, player2, 2-1, 3-1, _FinalBoardState2)).
    
test_player_make_push_predicates:-
    board_logic_test(logic1, Board), 
    board_logic_test(standard_initial_positions, Board2),
    board_logic_test(possible_push_positions, BoardPush), 
    board_logic_test(possible_push_positionsNewStatePush, BoardNewStatePush),
    board_logic_test(possible_push_positionsNewStatePush2, NewGameState2),

    run_test(valid_push(Board,player2,1-5, 1-6, _ResultCells1 ) ),
    run_test(\+ valid_push(Board2,player1,3-4, 4-4,  _ResultCells2)), %push fails with sr
    run_test(\+ valid_push(Board2,player1,2-4, 1-4 ,_ResultCells3)) , %push fails with sr

    run_test(valid_push(Board2,player1,2-5, 2-4, _ResultCells4)) , %push multiple different pieces


    run_test(valid_push(BoardPush,player2,1-5, 1-4, _ResultCells5) ) ,

    run_test(make_push(BoardPush, player2, 1-5, 1-4, BoardNewStatePush)),
    run_test(make_push(BoardNewStatePush, player2, 1-4, 1-3, _BoardNewStatePush2)),
    run_test(\+make_push(BoardPush, player2, 2-4, 1-4, _NewBoardState)), %this is impossible : in 0-4 we have a side rail (sr) and acnhor is on piece of player2 position 1-4
    change_anchor_piece(2-4, player2),
    run_test(\+make_push(BoardPush, player1, 2-5, 2-4, NewGameState2)),
    run_test(make_push(Board2, player2, 1-5, 1-4, _Board2NewStateGame)).


test_aux_predicates_game_ai:-
    board_logic_test(game_over, BoardGameOver),
    board_logic_test(standard_initial_positions, BoardAi),
    board_logic_test(possible_push_positions, BoardPush), 
    % % board_logic_test(logic1, Board),
    board_logic_test(cant_push_board, BoardCantPush),
    board_logic_test(standard_initial_positions2,NoGameOver),
    run_test(get_player_pieces_lists(BoardAi, player1, _ListOfPlayerSquares, _ListOfPlayerRounds)),
    run_test(get_player_pieces_lists(BoardAi, player2, _ListOfPlayerSquares2, _ListOfPlayerRounds2)),
    run_test(player_lost_game(BoardGameOver, player1)), % 6 is the default game number of Pieces a Player need to have to continue playing
    run_test(find_valid_push_moves(BoardAi, player1, _ValidPushMoves)), 
    run_test(find_valid_moves(BoardAi, player1, _ValidMoves)),
    run_test(find_move_game_states(BoardAi, player1, _ListOfNewGameStates)),
    run_test(find_push_game_states(BoardAi, player1, _ListOfNewGameStates2)),
    run_test(game_over(BoardGameOver-player1, player2)), %BoardGameOver have 5 pieces for player1 , to test this quickly
    run_test(\+ player_cant_push(BoardAi-player1)),
    run_test(player_cant_push(BoardCantPush-player1)),
    run_test(game_over(BoardCantPush-player1, player2)), % check if game_over, also checks if player cant push
    run_test(\+game_over(NoGameOver-player1, player2)),
    run_test(player_lost_game(BoardGameOver, player1)),
    run_test(player_cant_push(BoardCantPush-player1)),
    run_test(\+ player_cant_push(BoardPush-player1)),
    run_test(ai_move_game_state(NoGameOver, player1, _Elem)).

run_all_tests:-
    
    % test_board_basic_predicates,

    % test_board_player_predicates,
    % test_player_moves_predicates,
    % test_player_complete_moves_predicates,
    % test_player_make_push_predicates,
    test_aux_predicates_game_ai.


