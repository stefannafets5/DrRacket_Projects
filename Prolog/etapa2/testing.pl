:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.

%% Decomentați linia de mai jos pentru testare mai detaliată.
%% ATENȚIE: pe vmchecker linia este comentată.
detailed_mode_disabled :- !, fail.

% quick ref:
% chk: verifică că este adevărat. ("check")
% uck: verifică că al doilea argument este fals. ("uncheck")
% ech: verifică că fiecare soluție pentru primul argument îndelinește
% condițiile din listă. ("each")
% nsl: verifică numărul de soluții pentru variabila din al doilea
% argument și interogarea din primul argument. ("n solutions")
% nSO: la fel ca mai sus, dar ignoră duplicatele (folosește setof)
% exp: verifică că pentru interogarea din primul argument, toate
% condițiile din listă sunt respectate.
% sSO: verifică că pentru interogarea din primul argument, soluțiile
% pentru variabila din al doilea argument sunt cele din lista dată.
% Soluțiile duplicate sunt ignorate. ("solutions set of")

tt(b0, [
       % a
       exp('set_empty_board(8, B)', [val('B')]), % Board is bound
       ech('set_empty_board(8, B), between(0, 4, X), between(0, 4, Y)',
           [
               '\\+ is_wall(B, (X, Y))',
               '\\+ is_light(B, (X, Y))',
               '\\+ get_number(B, (X, Y), N)',
               'is_free(B, (X, Y))'
           ])
   ]).

tt(wall, [
       % add one wall, check if wall, all other positions are free
       exp('ttSet(w1, B)',
          [cond('is_wall(B, (2, 1))'), cond('\\+ get_number(B, (2, 1), N)')]),
       ech('(ttSet(w1, B), between(0, 4, X), between(0, 4, Y), (X, Y) \\= (2, 1))',
           [
               '\\+ is_wall(B, (X, Y))',
               '\\+ is_light(B, (X, Y))',
               '\\+ get_number(B, (X, Y), N)',
               'is_free(B, (X, Y))'
           ]),
       % add one wall with number, same
       exp('ttSet(wn1, B)', [cond('is_wall(B, (3, 3))')]),
       exp('ttSet(wn1, B), get_number(B, (3, 3), N)',
           [val('N'), 'N', 3]),
       ech('ttSet(wn1, B), between(0, 4, X), between(0, 4, Y), (X, Y) \\= (3, 3)',
           [
               '\\+ is_wall(B, (X, Y))',
               '\\+ is_light(B, (X, Y))',
               '\\+ get_number(B, (X, Y), N)',
               'is_free(B, (X, Y))'
           ]),
       % add multiple walls, check walls, check all other positions are free

       % încărcăm nivelul w2 și verificăm pereții existenți
       exp('ttSet(w2, B)', [
               cond('is_wall(B, (3, 2))'),
               cond('is_wall(B, (2, 2))'),
               cond('is_wall(B, (1, 2))'),
               cond('is_wall(B, (2, 3))'),
               cond('is_wall(B, (2, 1))')
        ]),

       % adăugăm pereți suplimentari și verificăm dacă sunt adăugați corect
       exp('ttSet(w2, B0), set_walls(B0, [((1, 0), \'\'), ((1, 2), \'\'), ((4, 4), \'\')], B)',
           [cond('is_wall(B, (1, 0))'),
            cond('is_wall(B, (1, 2))'),
            cond('is_wall(B, (4, 4))'),
            cond('\\+ is_wall(B, (2, 0))'),
            cond('\\+ is_wall(B, (2, 4))'),
            cond('\\+get_number(B, (1, 0), N)'),
            cond('\\+get_number(B, (1, 2), N)'),
            cond('\\+get_number(B, (4, 4), N)')
           ]),

       % adăugăm pereți suplimentari cu număr și verificăm
       exp('ttSet(wn1, B0), set_walls(B0, [((0, 0), 1), ((4, 3), 3)], B)',
           [cond('is_wall(B, (0, 0))'),
            cond('is_wall(B, (4, 3))'),
            cond('get_number(B, (0, 0), N), N = 1'),
            cond('get_number(B, (4, 3), N), N = 3')
           ])
   ]).

tt(light, [
       % add one light, check all other positions remain the same, check
       % position with light is not free
       ech('ttSet(light_test, B), between(0, 4, X), between(0, 4, Y), (X, Y) \\= (0, 0)', [
               'is_light(B, (0, 0))',
               '\\+ is_free(B, (0, 0))',
               '\\+ is_light(B, (X, Y))',
               'is_free(B, (X, Y))'
           ]),

       % Adaugă mai multe lumini, verifică dacă sunt luminate și celelalte celule
       exp('ttSet(light_test, B), add_light(B, (1, 1), B1), add_light(B1, (3, 3), B2)', [
               cond('is_light(B, (0, 0))'),
               cond('is_light(B2, (1, 1))'),
               cond('is_light(B2, (3, 3))'),
               cond('\\+ is_free(B, (0, 0))'),
               cond('\\+ is_free(B2, (1, 1))'),
               cond('\\+ is_free(B2, (3, 3))')
           ])
   ]).

tt(lit, [
       % Adaugă o lumină, verifică celulele luminate
       exp('ttSet(light_test, B), add_light(B, (2, 2), B2)', [
               % Verifică că celulele luminate sunt corect luminate
               cond('is_lit(B2, (2, 3))'),
               cond('is_lit(B2, (2, 0))'),
               cond('is_lit(B2, (2, 4))'),
               cond('is_lit(B2, (0, 2))'),
               cond('is_lit(B2, (4, 2))'),
               cond('is_lit(B2, (0, 1))'),

               % Verifică că toate celelalte celule nu sunt luminate
               cond('\\+ is_lit(B2, (1, 1))'),
               cond('\\+ is_lit(B2, (1, 3))'),
               cond('\\+ is_lit(B2, (1, 4))'),
               cond('\\+ is_lit(B2, (3, 1))'),
               cond('\\+ is_lit(B2, (3, 3))'),
               cond('\\+ is_lit(B2, (3, 4))'),
               cond('\\+ is_lit(B2, (4, 1))'),
               cond('\\+ is_lit(B2, (4, 3))')
           ]),
       % add one light with wall, same
       exp('ttSet(lit_test, B)',
           [cond('is_lit(B, (0, 0))'),
            cond('is_lit(B, (0, 1))'),
            cond('is_lit(B, (0, 2))'),
            cond('is_lit(B, (0, 3))'),
            cond('is_lit(B, (1, 0))'),
            cond('is_lit(B, (1, 1))'),
            cond('is_lit(B, (2, 1))'),
            cond('is_lit(B, (2, 2))'),
            cond('is_lit(B, (4, 2))'),
            cond('is_lit(B, (4, 4))'),
            cond('\\+ is_lit(B, (2, 3))'),
            cond('\\+ is_lit(B, (4, 3))')
           ]),

       % Adăugăm mai multe pereți care opresc lumina
       exp('ttSet(lit_test2, B)',
           [cond('is_lit(B, (0, 2))'),
            cond('is_lit(B, (5, 2))'),
            cond('is_lit(B, (4, 3))'),
            cond('is_lit(B, (2, 4))'),
            cond('\\+ is_lit(B, (3, 2))'),
            cond('\\+ is_lit(B, (4, 0))'),
            cond('\\+ is_lit(B, (0, 4))'),
            cond('\\+ is_lit(B, (1, 6))')
        ]),
       ech('ttSet(lit_test3, B), between(0, 4, X), between(0, 4, Y)',
           [
               '(is_lit(B, (X, Y)) ; is_wall(B, (X, Y)))'
           ])
   ]).

tt(ispos, [
       % check generative power of is_pos
       ech('ttSet(lit_test3, B), between(0, 4, X), between(0, 4, Y)',
           ['is_pos(B, (X, Y))']),
       ech('ttSet(lit_test3, B), is_pos(B, Pos)',  ['nonvar(Pos)', 'Pos = (X, Y)']),
       ech('ttSet(lit_test3, B), is_pos(B, (X, Y))',
           ['nonvar(X)', 'nonvar(Y)', 'X >= 0', 'X < 5', 'Y >= 0', 'Y < 5']),
       nSO('(ttSet(lit_test3, B), is_pos(B, (X, Y)))', '(X, Y)', 25),
       sSO('(ttSet(lit_test3, B), is_pos(B, Pos))', 'Pos', [
                                 (0,0), (1,0), (2,0), (3,0), (4,0),
                                 (0,1), (1,1), (2,1), (3,1), (4,1),
                                 (0,2), (1,2), (2,2), (3,2), (4,2),
                                 (0,3), (1,3), (2,3), (3,3), (4,3),
                                 (0,4), (1,4), (2,4), (3,4), (4,4)
                             ])
]).


tt(lightsAround, [
       % a
       exp('ttSet(lit_test, B), get_lights_around_wall(B, (2, 3), N)', ['N', 1]),
       exp('ttSet(lit_test2, B), get_lights_around_wall(B, (2, 2), N)', ['N', 1]),
  0.5, exp('ttSet(lit_test, B), get_lights_around_wall(B, (3, 3), N)', ['N', 0]),
  0.5, exp('ttSet(lit_test2, B), get_lights_around_wall(B, (4, 2), N)', ['N', 0]),
       % e
       exp('ttSet(lit_test2, B0), add_light(B0, (3, 2), B), get_lights_around_wall(B, (2, 2), N)', ['N', 2]),
       exp('ttSet(lit_test2, B0), add_light(B0, (3, 2), B), get_lights_around_wall(B, (4, 2), N)', ['N', 1]),
       exp('ttSet(lights1, B), get_lights_around_wall(B, (0, 0), N)', ['N', 2]),
       exp('ttSet(lights1, B), get_lights_around_wall(B, (4, 1), N)', ['N', 4]),
       % i
       exp('ttSet(lights1, B), get_lights_around_wall(B, (0, 4), N)', ['N', 2]),
       exp('ttSet(lights1, B), get_lights_around_wall(B, (3, 4), N)', ['N', 3]),
       exp('ttSet(lights1, B), get_lights_around_wall(B, (4, 4), N)', ['N', 3])
   ]).
tt(canAdd, [
       % a
       ech('ttSet(lights2, B), member(X, [2,3,4,6])', ['can_add_light(B, (X, 0))']),
       ech('ttSet(lights2, B), member(X, [2,3,6])', ['can_add_light(B, (X, 6))']),
       exp('ttSet(lights2, B)', [cond('can_add_light(B, (3, 1))')]),
       ech('ttSet(lights2, B), between(0, 5, X), between(1, 5, Y), (X, Y) \\== (3, 1)', [
           '\\+ can_add_light(B, (X, Y))']),
       % e
       exp('ttSet(lights2, B0), add_light(B0, (3, 0), B)', [cond('\\+ can_add_light(B, (3, 1))')]),
       exp('ttSet(lights2, B)', [cond('\\+ can_add_light(B, (-1, 0))'),
                                 cond('\\+ can_add_light(B, (7, 6))'),
                                 cond('can_add_light(B, (6, 6))')]),
       exp('ttSet(lights3, B)', [cond('can_add_light(B, (2, 3))')]),
       exp('ttSet(lights3, B0), add_light(B0, (2, 3), B)', [
               cond('can_add_light(B, (0, 2))'), cond('\\+ can_add_light(B, (1, 2))')])
   ]).
tt(valid, [
       % a
  0.5, exp('ttSet(lit_test3, B)', [cond('is_valid_solution(B)')]),
  0.5, exp('ttSet(valid1, B0), ttaddlights(B0, [(3,1), (6,0), (2,6)], B)', [cond('is_valid_solution(B)')]),
  0.5, exp('ttSet(valid2, B)', [cond('is_valid_solution(B)')]),
  0.5, exp('ttSet(valid3, B)', [cond('is_valid_solution(B)')]),
       % e
  0.5, exp('ttSet(can_light, B)', [cond('\\+ is_valid_solution(B)')]),
  0.5, exp('ttSet(lights1, B)', [cond('\\+ is_valid_solution(B)')]),
  0.5, exp('ttSet(lights2, B)', [cond('\\+ is_valid_solution(B)')]),
  0.5, exp('ttSet(lights3, B)', [cond('\\+ is_valid_solution(B)')]),
       % i
       exp('ttSet(invalid1, B)', [cond('\\+ is_valid_solution(B)')]),
       exp('ttSet(invalid2, B)', [cond('\\+ is_valid_solution(B)')]),
       exp('ttSet(invalid3, B)', [cond('\\+ is_valid_solution(B)')]),
       exp('ttSet(invalid4, B)', [cond('\\+ is_valid_solution(B)')])
   ]).
tt(solve, [
       exp('ttSet(solve1, B), solve(B, S)', [
               val('S'),
               cond('is_valid_solution(S)'),
               cond('is_light(S, (0, 0))'),
               cond('is_light(S, (3, 0))'),
               cond('is_light(S, (1, 2))'),
               cond('is_light(S, (2, 3))'),
               cond('\\+ is_light(S, (2, 0))'),
               cond('\\+ is_light(S, (0, 2))'),
               cond('\\+ is_light(S, (2, 1))')
                         ]),
       nSO('ttSet(solve1, B), solve(B, S)', 'S', 1),
       exp('ttSet(solve2, B), solve(B, S)', [
               val('S'),
               cond('is_valid_solution(S)'),
               cond('is_light(S, (2, 3))')
           ]),
       done
   ]).
tt(solvePlus, [
       exp('ttSet(solve1, B), solve_plus(B, S, H)',  ['H', [(1, 2), (2, 3), (0, 0), (3, 0)]]),
       exp('ttSet(solve3, B), solve_plus(B, S, H)',
           ['H', [(6, 0),(5, 1),(0, 0),(5, 4),(2, 5),(3, 6),(4, 2),(1, 3),(0, 6)]]),
       exp('ttSet(solve4, B), solve_plus(B, S, H)',
           ['H', [(6, 0),(5, 1),(4, 0),(1, 0),(0, 2),(6, 4),(6, 6),(3, 6),(2, 3),(0, 5)]])
   ]).

% încarcă o hartă conform cu reprezentarea corespunzătoare din levels.pl
ttSet(Level, Board) :-
    intern_load_map(Level, Map),
    length(Map, Sz),
    set_empty_board(Sz, B0),
    findall(L,
            (   between(0, Sz, Y),
                nth0(Y, Map, Line),
                findall(((X, Y), W),
                        (   between(0, Sz, X),
                            nth0(X, Line, Cell),
                            ttencode(Cell, W)
                        ), L)
            ), WLL),
    append(WLL, Walls),
    set_walls(B0, Walls, B1), !,
    findall(L,
            (   between(0, Sz, Y),
                nth0(Y, Map, Line),
                findall((X, Y),
                        (   between(0, Sz, X),
                            nth0(X, Line, '#')
                        ), L)
            ), LLS),
    append(LLS, Lights),
    ttaddlights(B1, Lights, Board)
    .

ttencode(C, N) :- atom_number(C, N), !.
ttencode('X', '').
ttaddlights(B, [], B) :- !.
ttaddlights(B, [Pos | Ls], B1) :-
    add_light(B, Pos, BL),
    ttaddlights(BL, Ls, B1).

