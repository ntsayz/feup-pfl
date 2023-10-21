

%Test logic.pl predicates
:- consult('../logic.pl').

board_logic_test(logic1,[[out,out,out,sr,sr,sr,sr,sr,out,out],
                [out,out,out,b_square,b_square,b_square,b_round,b_round,out,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out,empty, empty, empty,empty,empty,empty,empty,empty,out],
                [out, out, w_square,w_square,w_square,w_round,w_round,out,out,out],
                [out,out,sr,sr,sr,sr,sr,out,out,out]]).


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


run_all_tests :-
    % test_board_basic_predicates,

    test_board_player_predicates.


