:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').

% ----------------------
% STEP 1

% set_empty_board(+Size, -BoardOut)
% Binds BoardOut to an empty square board of given Size.
% ' ' = empty cell, '#' = wall, 'O' = light
set_empty_board(Size, BoardOut) :- 
    make_repeat_list(' ', Size, Row),
    make_repeat_list(Row, Size, BoardOut).

% set_pos(+BoardIn, +Pos, +Cell, -BoardOut)
% Helper predicate: returns BoardOut as BoardIn with Cell set at Pos
set_pos(BoardIn, (X, Y), Cell, BoardOut) :- 
    nth0(Y, BoardIn, Row),
    set_list_index(Row, X, Cell, NewRow),
    set_list_index(BoardIn, Y, NewRow, BoardOut).

% set_walls(+BoardIn, +Walls, -BoardOut)
% Binds BoardOut to the board after placing the Walls
set_walls(BoardIn, Walls, BoardOut) :-
    foldl(place_wall, Walls, BoardIn, BoardOut).

place_wall((Pos, Number), BoardIn, BoardOut) :-
    (number(Number) -> Cell = Number ; Cell = '#'),
    set_pos(BoardIn, Pos, Cell, BoardOut).

% get_board_size(+Board, -Size)
% Binds Size to the board side length
get_board_size(Board, Size) :-
    length(Board, Size).

% get_pos(+Board, +Pos, -Cell)
% Helper predicate: Binds Cell to the value at position Pos in Board
get_pos(Board, (X, Y), Cell) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Cell).

% is_wall(+Board, +Pos)
% True if Pos in Board is a wall (with or without number)
is_wall(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    (Cell = '#' ; number(Cell)).

% is_free(+Board, +Pos)
% True if Pos is free (not a wall or light)
is_free(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    Cell = ' ',
    \+ is_wall(Board, (X, Y)),
    \+ is_light(Board, (X, Y)).

% is_light(+Board, +Pos)
% True if Pos contains a light ('O')
is_light(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    Cell = 'O'.

% get_number(+Board, +Pos, -Number)
% Binds Number to the number on the wall at Pos (fails if not numbered wall)
get_number(Board, (X, Y), Number) :-
    get_pos(Board, (X, Y), Cell),
    number(Cell),
    Number = Cell.

% add_light(+BoardIn, +Pos, -BoardOut)
% Binds BoardOut to BoardIn with a light placed at Pos
add_light(BoardIn, Pos, BoardOut) :-
    set_pos(BoardIn, Pos, 'O', BoardOut).

% is_pos(+Board, ?Pos)
% True if Pos is a valid position on Board (X and Y between 0 and Size - 1)
% If Pos is unbound, generates all valid positions
is_pos(Board, (X, Y)) :-
    get_board_size(Board, Size),
    IndexMax is Size - 1,
    between(0, IndexMax, X),
    between(0, IndexMax, Y).

% is_lit(+Board, +Pos)
% True if Pos is lit by a light (on same row or column with no walls between)
is_lit(Board, (X, Y)) :-
    \+ is_wall(Board, (X, Y)),
    (
        is_light(Board, (X, Y));
        lit_in_line(Board, (X, Y));
        lit_in_column(Board, (X, Y))
    ).

% lit_in_line(+Board, +Pos)
% True if a light is on the same row with no wall between
lit_in_line(Board, (X, Y)) :-
    is_light(Board, (X1, Y)),
    sorted(X, X1, MinX, MaxX),
    \+ has_wall_between(Board, X, X1, Y, horizontal).

% lit_in_column(+Board, +Pos)
% True if a light is on the same column with no wall between
lit_in_column(Board, (X, Y)) :-
    is_light(Board, (X, Y1)),
    sorted(Y, Y1, MinY, MaxY),
    \+ has_wall_between(Board, Y, Y1, X, vertical).

% has_wall_between(+Board, +A, +B, +Index, +Direction)
% True if a wall exists between A and B on a line or column
has_wall_between(Board, A, B, Index, Direction) :-
    sorted(A, B, Min, Max),
    between(Min, Max, I),
    (
        Direction = horizontal, is_wall(Board, (I, Index));
        Direction = vertical,   is_wall(Board, (Index, I))
    ).
