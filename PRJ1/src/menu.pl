% this is temporary
main_menu :-
    clear_screen,
    main_menu_op, % maybe change the name in assets.pl
    read(Choice),
    handle_menu_choice(Choice).

handle_menu_choice(0) :- % exit loop
    write(''), nl.

handle_menu_choice(1) :-
    write('Starting game'), 
    start_game,nl,
    main_menu.

handle_menu_choice(2) :-
    rules,
    read(Choice),
    main_menu.

handle_menu_choice(_Other) :-
    write('Invalid choice. Please select a valid option.'), nl,
    main_menu.
