main_menu_op :- 
    title,
    write('1. Game'), nl,
    write('2. Rules'), nl, 
    write('0. Exit'), nl.

rules :-
    write('RULES'), nl,
    write('Rule'), nl,
    write('Any key to exit!'),
    nl.

game_mode_menu :-
    write('GAME MODE'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer - Random'), nl,
    write('3. Computer vs Computer - Random'), nl,
    write('0. Back'), nl.

title :-
    write(' ____  _____ '), nl,
    write('|  _ \\|  ___|'), nl,
    write('| |_) | |_   '), nl,
    write('|  __/|  _|  '), nl,
    write('|_|   |_|    '), nl,
    write('             '), nl.
