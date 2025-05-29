
:- ensure_loaded('files.pl').

editall :- forall(member(File, [
                            util, testing, levels, checker, points1, points2, files, lightup
                         ]), edit(File)).


% lista de poosibile direcții
directions([n,e,s,w]).

% neighbor(+Pos, +Direction, -NeighborPos)
% Leagă NeighborPos la poziția vecină cu Pos, aflată către direcția Dir
% e.g. Pentru Pos = (5, 4) și Direction = s
% va lega NeighborPos la (5, 5)
neighbor((X, Y), Dir, (XN, YN)) :- delta(Dir, DX, DY), XN is X + DX, YN is Y + DY.
% neighborD(+Pos, +Direction, +Delta, -NextNeighborPos)
% Leagă NextNeighborPos la poziția la distanță de Delta față de Pos,
% aflată către direcția Dir. e.g. Pentru Pos = (5, 4), Direction = s,
% și Delta = 2 va lega NeighborPos la (5, 7)
neighbor2((X, Y), Dir, Delta, (XN, YN)) :- delta(Dir, DX, DY),
    XN is X + Delta * DX, YN is Y + Delta * DY.

delta(Dir, DX, DY) :- member((Dir, DX, DY),
                             [(n, 0, -1), (s, 0, 1), (e, 1, 0), (w, -1, 0)]).

% make_repeat_list/3
% make_repeat_list(+E, +Len, -L)
% Leagă L la o listă de lungime L, în care fiecare element este egal cu
% argumentul E.
make_repeat_list(E, Len, L) :- findall(E, between(1, Len, _), L).

% set_list_index/4
% set_list_index(+ListIn, +Index, +Element, -ListOut)
% Leagă ListOut la o listă identică cu ListIn, în afară de elementul de
% la indexul Index, care este înlocuit cu Element.
set_list_index(ListIn, Index, Element, ListOut) :-
    I1 is Index - 1, I2 is Index + 1, length(ListIn, L),
    Index >= 0, Index < L,
    findall(E, (between(0, I1, Idx), nth0(Idx, ListIn, E)), L1),
    findall(E, (between(I2, L, Idx), nth0(Idx, ListIn, E)), L2),
    append([L1, [Element], L2], ListOut).

% sorted/4
% sorted(+A, +B, -Min, -Max)
% Leagă Min și Max la minimul, respectiv maximul dintre valorile A și B.
sorted(A, B, A, B) :- A =< B, !.
sorted(A, B, B, A) :- A > B.

% print_board/1
% print_board(+B)
% Afișează o tablă de joc, folosind predicatele
% get_board_size, get_number, is_wall, is_light, is_lit
print_board(B) :-
    get_board_size(B, Size), Sz is Size - 1,
    findall(Row, (
        between(0, Sz, Y),
        findall(Cell, (
            between(0, Sz, X),
            cell_to_string(B, (X, Y), Cell)
        ), Cols),
        atomics_to_string(Cols, '', Row)
    ), Rows),
    atomics_to_string(Rows, '\n', String),
    writeln(String).

cell_to_string(B, Pos, P) :-
    (   get_number(B, Pos, N), !, P = N
    ;   is_wall(B, Pos), !, P = 'X'
    ;   is_light(B, Pos), !, P = '#'
    ;   is_lit(B, Pos), !, P = '*'
    ;   P = '.'
    ).

