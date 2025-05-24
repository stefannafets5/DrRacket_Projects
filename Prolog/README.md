# Light Up Puzzle Solver – Prolog Implementation

**Copyright © Springer Robert Stefan, 2024**

## General Overview

This project implements a solver for the **Light Up** logic game (also known as "Akari") using **Prolog**.

### Game Rules:

- The board is square, with empty cells and walls (some walls can have numbers from 0 to 4).
- Bulbs can only be placed on empty cells.
- A bulb illuminates all cells in its row and column until a wall is encountered.
- The goal is:
  - All empty cells must be illuminated.
  - No bulb should be lit by another bulb.
  - Every wall with a number must be adjacent (orthogonally) to exactly that many bulbs.

---

## Features

- Custom internal board representation.
- Predicates for manipulating and accessing the board.
- Computation of lit cells.
- Validity checking of the board configuration.
- Complete solving using **backtracking**.
- Optimized solving using basic heuristics.

---

## File Structure

- `lightup.pl` – main implementation.
- `util.pl` – helper functions provided.
- `levels.pl` – predefined levels.
- `testing.pl` – testing infrastructure.
- `checker.pl` – checker logic used by the test framework.
- `points1.pl` – evaluation points (Stage1).
- `points2.pl` – evaluation points (Stage2).
- `files.pl` – loads all required files (entry point for loading the whole project).

---

## Board Representation

The internal board representation is opaque and should only be manipulated via user-defined predicates.

The representation must keep track of:
- Board size
- Wall positions and numbers
- Bulb positions
- (Optional) Lit cells

Positions are of the form `(X, Y)`, where `(0,0)` is the top-left corner of the board.

---

## Stage 1 – Representation and Lighting

### Implemented Predicates

```prolog
empty_state(+Size, -Board)
set_walls(+Board0, +Walls, -Board)
get_board_size(+Board, -Size)
is_wall(+Board, +Position)
get_number(+Board, +Position, -Number)
add_light(+Board0, +Position, -Board)
is_lit(+Board, +Position)
```

## Stage 2 – Solving

### Implemented Predicates

```prolog
can_place_light(+Board, +Position)
valid_solution(+Board)
solve(+Board0, -BoardSolved)
solve_optimized(+Board0, -BoardSolved)
```
## A valid solution must satisfy:

- All empty cells are illuminated.

- No bulb is illuminated by another bulb.

- Every numbered wall has exactly the required number of adjacent bulbs.

## Usage Instructions

Load the program in Prolog
```prolog
?- [lightup].
```

Run all tests
```prolog
?- check.
```

Run an individual test
```prolog
?- vmtest(b0).
```