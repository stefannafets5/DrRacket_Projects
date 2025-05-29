
vmpoints(Test, Points):-
        member(Test:Points,
               [
                        b0:0,
                        wall:0,
                        light:0,
                        lit:0,
                        ispos:0,

                        lightsAround:20,
                        canAdd:20,
                        valid:40,
                        solve:20,
                        solvePlus:20
              ]).
