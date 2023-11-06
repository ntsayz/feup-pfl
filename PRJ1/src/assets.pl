main_menu_op :- 

    draw_banner(48), nl,
    write('        1. Game'), nl,
    write('        2. Rules'), nl, 
    write('        0. Exit'), nl.

rules :-
    write('RULES'), nl,
    write('Rule'), nl,
    write('Any key to exit!'),
    nl.


% Predicate to draw a square banner with a title and two stick figures.
draw_banner(Columns):-
    Title = "Push-Fight Game !  ",
    BannerHeight is Columns,  % Calculate the height of the banner
    StickManHeight is BannerHeight // 2,  % Calculate the height of the
    Middle is Columns // 2 + 6,
    % Calculate padding for title and stick figures
    length(Title, TitleLength),
    TitlePad is (Columns - TitleLength) // 2,
    % Draw the banner
    nl,
    draw_top_bottom(Columns),
    draw_centered_text(Title, Columns, TitlePad),
    % draw_empty_line(Columns),
    print_stick_man_line(Middle, Columns,StickManHeight),
    draw_top_bottom(Columns).

% Helper predicate to draw the top and bottom border of the banner.
draw_top_bottom(Columns) :-
    fill_line(':', Columns),
    nl.

% Helper predicate to draw an empty line with colons on the sides.
draw_empty_line(Columns) :-
    write(':'),
    fill_line(' ', Columns - 2),
    write(':'),
    nl.

% Helper predicate to draw centered text within the banner.
draw_centered_text(Text, Columns, _Pad) :-
    length(Text, TextLength),
    SidePad is (Columns - TextLength) // 2 +2,
    fill_line(' ', SidePad), 
    format('~s~n', [Text]), 
    fill_line(' ', SidePad), 
    nl.

% Helper predicate to fill a line with a specific character.
fill_line(Char, Length) :-
    ( Length > 0 ->
        write(Char),
        NewLength is Length - 1,
        fill_line(Char, NewLength)
    ; true
    ).


print_stick_man_line(_Middle, _Collumns,0).
print_stick_man_line(Middle, Collumns,StickManHeight) :-
    StickManHeight > 0,
    Spaces is Middle - 3,  % Adjust for stick figure width
    print_row(' ', Spaces),
    write('O  O'),
    print_row(' ', Spaces), nl,
    print_row(' ', Spaces),
    write('/|\\/|\\'),
    print_row(' ', Spaces), nl,
    print_row(' ', Spaces),
    write('/ \\/ \\'),
    print_row(' ', Spaces), nl,
    draw_top_bottom(Collumns), nl,
    NewStickManHeight is StickManHeight - 6,
    NewMidle is Middle -6,
    print_stick_man_line(NewMidle, Collumns,NewStickManHeight).

print_row(Char, 0) :- 
write(Char).

print_row(Char, Length) :-
    Length > 0,
    write(Char),
    NewLength is Length - 1,
    print_row(' ', NewLength).


game_mode_menu :-
    draw_banner(48), nl,
    write('GAME MODE'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer - Random'), nl,
    write('3. Player vs Computer - Advanced'), nl,
    write('4. Computer vs Player - Advanced'), nl,
    write('5. Computer vs Computer -  Advanced'), nl,
    write('6. Computer vs Computer - Advanced-Random'), nl,
    write('7. Computer vs Computer - Advanced'), nl,

    write('0. Back'), nl.

title :-
    write(' ____  _____ '), nl,
    write('|  _ \\|  ___|'), nl,
    write('| |_) | |_   '), nl,
    write('|  __/|  _|  '), nl,
    write('|_|   |_|    '), nl,
    write('             '), nl.
