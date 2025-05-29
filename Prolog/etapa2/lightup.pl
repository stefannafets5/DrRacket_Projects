:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').

% ETAPA 1

%% TODO
% set_empty_board/2
% set_empty_board(+Size, -BoardOut)
%
% Leagă Board la o tablă de joc goală, cu latura de dimensiune Size.
%
% Vedeți Hints din enunț legat de posibilitățile de reprezentare.

% nimic = ' '
% perete = #
% bec = O

set_empty_board(Size, BoardOut) :- 
    make_repeat_list(' ', Size, Row),
    make_repeat_list(Row, Size, BoardOut).

%% TODO, opțional
% set_pos/4
% set_pos(+BoardIn, +Pos, +Cell, -BoardOut)
%
% Aceasta este un predicat ajutător, opțional, care NU este testat
% și pe care nu este obligatoriu să îl implementați.
% Predicatul leagă BoardOut la o tablă identică cu BoardIn, dar care la
% poziția Pos conține informațiile date în Cell.
%
% Vedeți Hints din enunț legat de posibilitățile de reprezentare.
set_pos(BoardIn, (X, Y), Cell, BoardOut) :- 
    nth0(Y, BoardIn, Row),
    set_list_index(Row, X, Cell, NewRow),
    set_list_index(BoardIn, Y, NewRow, BoardOut).

%% TODO
% set_walls/3
% set_walls(+BoardIn, +Walls, -BoardOut)
%
% Leagă BoardOut la o tablă de joc care conține toate elementele din
% BoardIn, și de aceeași dimensiune, la care se adaugă toți pereții
% din Walls.
% Walls este o listă de perechi Poziție - Număr (e.g. (Pos, Number)),
% unde Number poate fi un număr sau valoarea ''.
% Pentru a ști dacă o valoare este număr, folosiți predicatul number/1.
% Puteți presupune că BoardIn este o tablă goală sau conține doar
% pereți, pe alte poziții decât cele specificate în Walls.

set_walls(BoardIn, Walls, BoardOut) :-
    foldl(place_wall, Walls, BoardIn, BoardOut).

place_wall((Pos, Number), BoardIn, BoardOut) :-
    (number(Number) -> Cell = Number ; Cell = '#'),
    set_pos(BoardIn, Pos, Cell, BoardOut).

%% TODO
% get_board_size(+Board, -Size)
% Leagă Size la dimensiunea laturii tablei Board.

get_board_size(Board, Size) :-
    length(Board, Size).

%% TODO, opțional
% get_pos/3
% get_pos(+Board, +Pos, -Cell)
%
% Aceasta este un predicat ajutător, opțional, care NU este testat
% și pe care nu este obligatoriu să îl implementați.
% Predicatul leagă Cell la informația legată de poziția Pos, în
% tabla BoardIn.
%
% Vedeți Hints din enunț legat de posibilitățile de reprezentare.
get_pos(Board, (X, Y), Cell) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Cell).

%% TODO
% is_wall/2
% is_wall(+Board, +Pos)
%
% Întoarce adevărat dacă la poziția Pos din tabla Board este un perete
% (indiferent dacă este cu număr sau fără). Întoarce fals dacă la
% poziția pos nu este un perete.

is_wall(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    (Cell = '#' ; number(Cell)).


%% TODO
% is_free/2
% is_free(+Board, +Pos)
%
% Întoarce adevărat dacă poziția Pos din tabla Board este liberă (nu
% este perete și nu este deja o lumină acolo). Întoarce fals dacă
% poziția nu este în interiorul tablei, dacă este un perete sau dacă
% este o lumină (un bec).

is_free(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    Cell = ' ',
    \+ is_wall(Board, (X, Y)),
    \+ is_light(Board, (X, Y)).

%% TODO
% is_light/2
% is_light(+Board, +Pos)
%
% Întoarce adevărat dacă la poziția Pos din tabla Board este un bec (o
% lumină)

is_light(Board, (X, Y)) :-
    get_pos(Board, (X, Y), Cell),
    Cell = 'O'.

%% TODO
% get_number/3
% get_number(+Board, +Pos, -Number)
% Leagă Number la numărul de pe peretele de la poziția Pos pe tabla
% Board.
% Întoarce false dacă la poziția Pos nu este un perete.
% Întoarce false dacă la poziția Pos este un perete fără număr.

get_number(Board, (X, Y), Number) :-
    get_pos(Board, (X, Y), Cell),
    number(Cell),
    Number = Cell.

%% TODO
% add_light/3
% add_light(+BoardIn, +Pos, -BoardOut)
%
% Leagă BoardOut la o tablă identică cu BoardIn, doar că la poziția Pos
% se adaugă un bec (o lumină).
% Nu sunt necesare verificări aici dacă Pos este liberă sau dacă este o
% poziție legală pentru un bec.

add_light(BoardIn, Pos, BoardOut) :-
    set_pos(BoardIn, Pos, 'O', BoardOut).

%% TODO
% is_pos/2
% is_pos(+Board, ?Pos)
%
% Dacă Pos este o variabilă legată la o pereche de coordonate, atunci
% predicatul întoarce adevărat dacă poziția este o poziție din
% interiorul hărții (X și Y sunt între 0 și Size - 1, inclusiv).
% Dacă Pos este o variabilă nelegată, atunci predicatul leagă Pos la o
% poziție validă de pe tablă. Soluții succesive vor lega Pos la toate
% pozițiile de pe tablă (Size * Size poziții).
%
% Hint: predicatul between/3 predefinit în prolog.

is_pos(Board, (X, Y)) :-
    get_board_size(Board, Size),
    IndexMax is Size - 1,
    between(0, IndexMax, X),
    between(0, IndexMax, Y).

%% TODO
% is_lit/2
% is_lit(+Board, +Pos)
%
% Întoarce adevărat dacă poziția Pos de pe tabla Board este luminată.
% O poziție Pos este luminată dacă se află pe acceași linie sau coloană
% cu un bec de pe hartă, și între Pos și bec nu se află niciun perete.
% Mai precis, nu există nicio poziție WP pe hartă, pe aceeași linie /
% coloană cu Pos și cu becul (două cazuri, aceeași linie și aceeași
% coloană), astfel încât poziția WP să fie între Pos și poziția becului,
% iar la poziția WP să fie un perete.
%
% Hint: predicatul sorted/4, din util.pl, și operatorul de
% negare din Prolog, \+

is_lit(Board, (X, Y)) :-
    \+ is_wall(Board, (X, Y)),
    (
        is_light(Board, (X, Y));
        lit_in_line(Board, (X, Y));
        lit_in_column(Board, (X, Y))
    ).

% Bec pe acelasi randd
lit_in_line(Board, (X, Y)) :-
    is_light(Board, (X1, Y)),
    sorted(X, X1, MinX, MaxX),
    \+ has_wall_between(Board, X, X1, Y, horizontal).

% Bec pe aceeasi coloana
lit_in_column(Board, (X, Y)) :-
    is_light(Board, (X, Y1)),
    sorted(Y, Y1, MinY, MaxY),
    \+ has_wall_between(Board, Y, Y1, X, vertical).

has_wall_between(Board, A, B, Index, Direction) :-
    sorted(A, B, Min, Max),
    between(Min, Max, I),
    (
        Direction = horizontal, is_wall(Board, (I, Index));
        Direction = vertical,   is_wall(Board, (Index, I))
    ).

%% TODO
% get_lights_around_wall/3
% get_lights_around_wall(+Board, +Pos, -N)
%
% Leagă N la numărul de lumini din jurul peretelui de la poziția Pos
% (doar pe direcții ortogonale (nord, sud, est, vest).
%
% Hint: predicatele din prolog member/2, findall/3 și predicatele din
% utils.pl directions/1, neighbor/3

get_lights_around_wall(Board, Pos, N) :-
    directions(Dirs),
    findall(NeighborPos,
        (
            member(Dir, Dirs),
            neighbor(Pos, Dir, NeighborPos),
            is_light(Board, NeighborPos)
        ),
        LightPositions
    ),
    length(LightPositions, N).


%% TODO
% can_add_light/2
% can_add_light(+Board, +Pos)
%
% Întoarce adevărat dacă
% - poziția nu este un perete
% - poziția nu este deja luminată
% - pentru toți pereții aflați pe poziții vecine (ortogonal), numărul de
% pe perete, dacă există, este mai mare decât numărul de lumini deja
% aflate în jurul peretelui.
%
% Hint: forall/2, directions/1, neighbor/3

can_add_light(Board, Pos) :-
    \+ is_wall(Board, Pos),
    \+ is_lit(Board, Pos),
    is_pos(Board, Pos),
    directions(Dirs),
    forall(
        (
            member(Dir, Dirs),
            neighbor(Pos, Dir, WallPos),
            is_wall(Board, WallPos),
            get_number(Board, WallPos, Num)
        ),
        (
            get_lights_around_wall(Board, WallPos, Lights),
            Lights < Num
        )
    ).


%% TODO
% is_valid_solution/1
% is_valid_solution(Board)
%
% Întoarce adevărat dacă Board este o soluție validă a problemei.
% Trebuie verificate 3 elemente:
%
% 1) Toate pozițiile de pe tablă, care nu sunt pereți, sunt luminate.
% 2) Pentru toți pereții cu numere, pe pozițiile lor vecine ortogonal se
% află exact numărul necesar de lumini (nici mai mult, nici mai puțin).
% 3) Toate becurile sunt pe poziții care, dacă nu ar fi fost acel bec
% acolo, nu ar fi fost luminate.
is_valid_solution(_) :- false.

is_valid_solution(Board) :-
    % Nu exista o pozitie Pos in Board care sa nu fie perete si sa nu fie luminata.
    \+ (is_pos(Board, Pos), \+ is_wall(Board, Pos), \+ is_lit(Board, Pos)),

    % Nu exista o pozitie Pos in Board care sa fie perete si sa aiba in jur un numar
    % diferit de becuri de cel specificat pe perete.
    \+ (
        is_pos(Board, Pos),
        get_number(Board, Pos, N),
        get_lights_around_wall(Board, Pos, Count),
        N \= Count
    ),

    % Nu exista o pozitie Pos in Board care e bec si ramane luminata
    % daca e inlocuita cu spatiu gol.
    \+ (
        is_pos(Board, Pos),
        is_light(Board, Pos),
        set_pos(Board, Pos, ' ', BoardTemp),
        is_lit(BoardTemp, Pos)
    ).


%% TODO
% solve/2
% solve(+Board, -Solution)
%
% Leagă Solution la o tablă cu aceeași pereți ca și Board, și cu lumini
% adăugate în așa fel încât Solution să fie o soluție a jocului
% (is_valid_solution să fie adevărat pentru Solution).
%
% Hint:
% - dacă Board este o tablă validă, atunci aceasta este soluția.
% - dacă găsesc o poziție (is_pos) în Board unde pot plasa o lumină, o
% plasez acolo și dau rezultatul unui nou apel de solve.
%
% Folosiți capacitatea generativă a lui is_pos, care să întoarcă
% posibile soluții de pe hartă la încercări succesive.

solve(Board, Board) :-
    is_valid_solution(Board), !.

solve(Board, Solution) :-
    is_pos(Board, Pos),
    can_add_light(Board, Pos),
    add_light(Board, Pos, BoardWithLight),
    solve(BoardWithLight, Solution).

%% TODO
% solve_plus/3
% solve_plus(+Board, -Solution, -Light_Order)
%
% La fel ca și solve/2, dar leagă Light_Order la o listă de poziții unde
% au fost plasate lumini pentru a rezolva tabla, în ordinea în care au
% fost adăugate (prima poziție din Light_Order este prima care a fost
% adăugată). Trebuie folosită o ordine optimă a adăugării, în scopul
% obținerii mai rapid a unei soluții. Astfel, la fiecare încercare de
% plasare a unei lumini se vor lua în considerare următoarele criterii:
%
% - Se vor plasa lumini în jurul pereților cu numere unde numărul de
% poziții disponibile vecine cu peretele, plus numărul de lumini deja
% plasate lângă perete, este egal cu numărul de pe perete. În aceste
% cazuri, se pot plasa toate luminile în aceeași iterație.
%
% - Se vor plasa lumini în spațiile de o celulă neluminată care nu are
% nicio altă poziție pe verticală sau pe orizontală unde ar putea fi
% pusă o lumină, care să lumineze celula.
%
% Pentru fiecare dintre cele două criterii, dacă sunt mai multe situații
% pe tablă care îndeplinesc un criteriu, se va rezolva prima situația
% cea mai de sus, și cea mai din stânga.
solve_plus(Board, Solution, Light_Order) :- false.