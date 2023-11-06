
main_menu :-
    change_anchor_piece(null-null, noplayer),
    main_menu_op, 
    read(Choice),
    menu_choice(Choice).

menu_choice(0) :- % exit loop
    write(''), nl.

menu_choice(1) :-
    clear_screen,
    game_mode_choice,nl,
    main_menu.

menu_choice(2) :-
    rules,
    read(_Choice),
    main_menu.

menu_choice(_Other) :-
    write('Invalid choice. Please select a valid option.'), nl,
    main_menu.


game_mode_choice :-
    clear_screen,
    game_mode_menu,
    read(Choice),
    game_mode_op(Choice).

game_mode_op(0) :-
    clear_screen.

game_mode_op(1) :-
    clear_screen,
    start_game(1).

game_mode_op(2) :-
    clear_screen,
    start_game(2).


game_mode_op(3) :-
    clear_screen,
    start_game(3).

game_mode_op(4) :-
    clear_screen,
    start_game(4).

game_mode_op(5) :-
    clear_screen,
    start_game(5).

game_mode_op(6) :-
    clear_screen,
    start_game(6).


game_mode_op(7) :-
    clear_screen,
    start_game(7).

game_mode_op(_Other) :-
    write('Invalid choice. Please select a valid option.'), nl,
    game_mode_choice.