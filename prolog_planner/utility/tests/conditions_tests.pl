:-ensure_loaded('../conditions.pl').
:-ensure_loaded('../../includes.pl').
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
  in_resources(av(a1), R),
  R = a1,
  \+in_resources([], _),
  \+in_resources([av(a3)], _),
  in_resources([av(a1), av(a2)], R1),
  R1 = a1,
  in_resources([av(a2), av(a1)], R2),
  R2 = a2,
  in_resources([tree(a2), av(a2)], R2),
  in_resources([tree(a2)], _),
  in_resources([av(a1),free(block2)], R3),
  R3 = a1,
  true.

test_achievers_4 :-
  achiever(
    [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)], 
    [placing_arch(_19930,a,b,_19936),placed_arch(a,b,_19950)], 
    [del(pillaring(a1,b,block2)),add(av(a1)),add(pillar(b,block2))], 
    []
  ),
  true.

test_achievers_3 :-
  % trace,
  achiever(
    [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)], 
    [placing_arch(_19930,a,b,_19936),placed_arch(a,b,_19950)], 
    build_pillar_end(a1, b, block2)
  ),
  true.