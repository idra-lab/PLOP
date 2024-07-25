:-ensure_loaded('../planner.pl').

test_choose_action :-
  choose_action(
    [ontable(b1), ontable(b2), at(b1,3,3), at(b2,2,2), clear(b1), clear(b2), available(a1)],
    [ontable(b1), on(b2,b1), at(b1,3,3), at(b2,3,3), clear(b2), available(a1)],
    Action,
    PT,
    PF,
    NF, 
    V, 
    E
  ),
  format('Action ~w~nPT ~w~nPF ~w~nNF ~w~nV ~w~nE ~w~n', [Action, PT, PF, NF, V, E]).


test_choose_action1 :-
  choose_action(
    [ontable(b1), ontable(b2), at(b1,1,1), at(b2,2,2), clear(b1), clear(b2), available(a1)],
    [ontable(b1), on(b2,b1), at(b1,3,3), at(b2,3,3), clear(b2), available(a1)],
    Action,
    PT,
    PF,
    NF, 
    V, 
    E
  ),
  format('Action ~w~nPT ~w~nPF ~w~nNF ~w~nV ~w~nE ~w~n', [Action, PT, PF, NF, V, E]).

