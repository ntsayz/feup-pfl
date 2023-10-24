

%Test logic.pl predicates
:- consult('../logic.pl').

board_logic_test(logic1,
    [[out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
    [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out]]).
board_logic_test(standard_intial_positions, [
    [out,out,out,sr,sr,sr,sr,sr,out,out],
    [out,out,out,empty,b_square,b_round,empty,empty,out,out],
    [out,empty,empty,b_round,b_square,empty,empty,empty,empty,out],
    [out,empty,empty,w_round,w_square,empty,empty,empty,empty,out],
    [out,out,out,empty,w_square,w_round,empty,empty,out,out],
    [out,out,sr,sr,sr,sr,sr,out,out,out] ]
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

test_player_move_predicates:- 
    board_logic_test(logic1,Board),


    %Testing the 4 possible moves from a given cell to another cell, moving only one cell 
    run_test(possible_move(1-3,2-3)), % down
    run_test(possible_move(1-3,1-4)), % right
    run_test(possible_move(1-4,1-3)), % left
    run_test(possible_move(2-3,1-3)), % up
    %Testing if a move from CurrentPos to DestPosition, using more then one cell is valid
    run_test(\+valid_move(Board, player2, 1-3, 1-3)), %we assume move 0 number of cells is not a valid_move
    run_test(\+valid_move(Board, player2, 1-3, 4-3)), % 4-3 have a piece of player1 (w_square)
    run_test(valid_move(Board, player2, 1-3, 3-5)), % right-down move
    run_test(valid_move(Board, player2, 1-7, 3-3)), % left-down move
    run_test(valid_move(Board, player1, 4-3, 3-5)), % right-up move
    run_test(valid_move(Board, player1, 4-6, 3-3)), % left-up move
    run_test(\+valid_move(Board, player2, 3-3, 1-5)). % no piece to move in 3-3 and 1-5 have a piece
run_all_tests :-

    test_board_basic_predicates,

    test_board_player_predicates,
    test_player_move_predicates.


