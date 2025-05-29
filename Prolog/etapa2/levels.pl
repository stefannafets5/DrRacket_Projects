

% . este o poziție liberă
% X este un perete fără număr
% <N> este un perete cu numărul N
% # este o lumină

intern_level(w1, "
.....
..X..
.....
.....
.....
").

intern_level(wn1, "
.....
.....
.....
...3.
.....
").

intern_level(w2, "
.....
..X..
.XXX.
..X..
.....
").


intern_level(w3, "
.....
.X...
..XX.
.....
.....
").

intern_level(light_test, "
#....
.....
.....
.....
.....
").

intern_level(lit_test, "
#....
.....
..#..
..XX.
.#...
").

% no_solution
intern_level(lit_test2, "
.......
....X..
.#2.2.#
.......
.X..#..
.......
.......
").

intern_level(lit_test3, "
...#.
..1..
..#..
.#.1#
#....
").

% no solution
intern_level(can_light, "
..#..
..2.2
.#...
.3...
.#...
").

intern_level(lights1, "
X#..#.
#..#3#
....#.
#..##.
X##XX#
...##.
").

intern_level(lights2, "
.......
.#2.2..
....#..
#......
3....#.
#......
.......
").

intern_level(lights3, "
......
......
..X...
.2.2#.
.#X...
......
").

intern_level(valid1, "
.......
.#2.2..
....#..
#......
2....#.
#......
.......
").

intern_level(valid2, "
...#..
#.....
.XX...
.2#2#.
.#X..#
..#...
").

intern_level(valid3, "
#...X2#
1.0..#.
0...#X.
.#.....
.1...#1
..#.X.X
#X2#...
").

% wall at 0, 2 has 1 light around
intern_level(invalid1, "
.#..X2#
0.0..#.
0...#X.
#......
.0...#1
..#.X.X
.X2#...
").

% wall at 1,4 has no lights around
intern_level(invalid2, "
.#..X2#
0.0..#.
1...#X.
#......
.1...#1
..#.X.X
.X2#...
").

% lights light each other on column 1
intern_level(invalid3, "
.#..X2#
0.0..#.
0...#X.
.#.....
.1...#1
..#.X.X
#X2#...
").

% cell 1,1 is unlit
intern_level(invalid4, "
#..X2#
1.0..#.
1...#X.
#......
.0...#1
..#.X.X
.X2#...
").

intern_level(solve1, "
.1..
.X.X
....
X2..
").

intern_level(solve2, "
......
......
.XX...
.2.2..
..X...
......
").

intern_level(solve3, "
....X2.
1.0....
0....X.
.......
.1....1
....X.X
.X2....
").

intern_level(solve4, "
..1..3.
1......
......X
.......
X......
......2
.0..1..
").


intern_load_map(Level, Map) :-
    intern_level(Level, String), !,
    string_lines(String, Lines), !,
    maplist(atom_chars, Lines, [_|Map]).

