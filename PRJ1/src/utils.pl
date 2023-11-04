clear_screen :- 
    write('\33\[2J').

% Read a single character from the input
get_single_char(Char) :-
    get_code(Code),
    get_code(_), % Consume the newline character (Enter key)

    % Convert the character code to a character
    char_code(Char, Code).


% Read a line of input from the user
read_line_input(Line) :-
    read_line_to_codes(user_input, Codes),
    atom_codes(Line, Codes).


