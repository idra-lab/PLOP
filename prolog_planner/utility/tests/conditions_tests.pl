:-ensure_loaded('../conditions.pl').
:-ensure_loaded('../../includes.pl').

% agent(a1).
% agent(a2).

% arm(a1, 10, 10, 10).
% arm(a2, 20, 20, 20).

% gripper(a1).
% gripper(a2).

% new_agent(a1, a2, a3).
% new_agent(a2, a3, a7).

% resources :- resources(X), X, functor(X, Y, _), write(X), write(' '), write(Y), nl.
% resources(new_agent(_, _, _)).
% resources(agent(_)).
% resources(arm(_, _, _, _)).
% resources(gripper(_)).

% test_check_args :-
%   check_args([a1], [a1,a2], R),
%   R = a1,
%   \+check_args([], [], _),
%   \+check_args([a1], [a2], _),
%   check_args([a1, a2], [a1, a3, a2], R1),
%   R1 = a1,
%   true.

test_in_resources :-
  in_resources([av(a1)], [agent(a1)], agent(a1)),
  \+in_resources([], [], _),
  \+in_resources([av(a3)], [], _),
  in_resources([av(a1), av(a2)], [agent(a1), agent(a2)], agent(a1)),
  in_resources([av(a2), av(a1)], [agent(a1), agent(a2)], agent(a2)),
  in_resources([tree(t2), av(a2)], [point(p2), agent(a2)], agent(a2)),
  \+in_resources([tree(t2)], [tree(t2)], _),
  in_resources([av(a1), free(block2)], [agent(a1)], agent(a1)),
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

test_achievers_5 :-
  last_achievers_ids(
    [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)],
    [],
    [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
    A
  ),
  format('A ~w~n', [A]).


test_check_args_verify :-
  check_args_verify([a1], [agent(a1),agent(a2)], agent(a1)),
  \+check_args_verify([b], [agent(a2)], _),
  check_args_verify([b, a2], [block(b1), agent(a3), agent(a2)], agent(a2)),
  true.