% this is temporary
main_menu :-
    clear_screen,
    main_menu_op, % maybe change the name in assets.pl
    read(Choice),
    menu_choice(Choice).

menu_choice(0) :- % exit loop
    write(''), nl.

menu_choice(1) :-
    write('Starting game'), 
    start_game,nl,
    main_menu.

menu_choice(2) :-
    rules,
    read(Choice),
    main_menu.

menu_choice(_Other) :-
    write('Invalid choice. Please select a valid option.'), nl,
    main_menu.
