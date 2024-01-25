testCase(Actions, Time) :-
    go(
        [
            available(r1), at(r1, 1, 1), at(box1, 1, 2), at(box2, 1, 3),
            at(box3, 2, 1), at(box4, 2, 2), at(box5, 2, 3), at(box6, 3, 1), 
            at(architrave, 3, 2)
        ],
        [
            available(r1), at(r1, 1, 1), pillar(point(a)), pillar(point(b)), 
            placed_architrave
        ],
        Actions,
        Time
    ).