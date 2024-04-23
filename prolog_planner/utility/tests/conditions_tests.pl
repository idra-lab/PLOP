:-ensure_loaded('../conditions.pl').

agent(a1).
agent(a2).

resources(agent(_)).

test_check_args :-
  check_args([a1], [a1,a2], R),
  R = a1,
  \+check_args([], [], _),
  \+check_args([a1], [a2], _),
  check_args([a1, a2], [a1, a3, a2], R1),
  R1 = a1,
  true.

test_in_resources :-
  in_resources([av(a1)], R),
  R = a1,
  \+in_resources([], _),
  \+in_resources([av(a3)], _),
  in_resources([av(a1), av(a2)], R1),
  R1 = a1,
  in_resources([av(a2), av(a1)], R2),
  R2 = a2,
  in_resources([tree(a2), av(a2)], R2),
  in_resources([tree(a2)], _),
  true.