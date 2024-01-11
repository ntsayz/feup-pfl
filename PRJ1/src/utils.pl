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

convert_to_format([SrcRow-SrcCol, DestRow-DestCol], [X1, Y1, X2, Y2]) :-
    X1 is SrcCol + 97,
    Y1 is SrcRow + 1 + 48,
    X2 is DestCol + 97,
    Y2 is DestRow + 1 + 48.

print_chosen_move([X1, Y1, X2, Y2]) :-
    write('Chosen Move: '), 
    write(X1), write(Y1), write(' -> '), write(X2), write(Y2), nl, nl.


read_user_input(Move):-
    read(Input),    %d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, _ListOfMoves).


parse_input([X1, Y1, X2, Y2], Move, _ListOfMoves) :-
    (   [X1, Y1, X2, Y2] = ['x', 'x', 'x', 'x'] ->
        Move = xxxx
    ;   char_code(X1, X1Code),
        char_code(Y1, Y1Code),
        char_code(X2, X2Code),
        char_code(Y2, Y2Code),
        % verifica se o input é válido. se a char for um número ou uma letra dependendo da posição
        X1Code >= 97,
        X1Code =< 104,
        Y1Code >= 49,
        Y1Code =< 56,
        X2Code >= 97,
        X2Code =< 104,
        Y2Code >= 49,
        Y2Code =< 56,
        Move = [X1Code, Y1Code, X2Code, Y2Code]
    ).

convert_to_index(Board, Move, Indexes, Pieces) :-
    Move = [X1, Y1, X2, Y2],
    
    % % debugging
    % write('Move: '), write(Move), nl,
    
    % converting char to index para account pelo offset do header
    X1Index is X1 - 97 , 
    Y1Index is Y1 - 48 - 1,
    X2Index is X2 - 97 , 
    Y2Index is Y2 - 48 - 1, 
    
    % % debugging
    % write('Real Indexes Row-Col: '), write([Y1Index, X1Index, Y2Index, X2Index]), nl,
    
    % getting pieces
    nth0(Y1Index, Board, Row1),
    nth0(X1Index, Row1, Piece1),
    nth0(Y2Index, Board, Row2),
    nth0(X2Index, Row2, Piece2),
    
    % Info for Start Position: Element/Piece to Final Position: Element/Piece
    write('Elements/Pieces: Piece -> Piece|Element : '), write(Piece1),write(' -> '), write(Piece2), nl, nl,
    

    Indexes = [X1Index, Y1Index, X2Index, Y2Index],
    Pieces = [Piece1, Piece2].