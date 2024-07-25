:-ensure_loaded('../conditions.pl').

action(build_pillar_start(A, Pos, Block1),
  [av(A), free(Block1)],
  [pillar(Pos, _), pillaring(_, Pos, _)],
  [pillar(Pos, Block1)],
  [block(Block1), agent(A), pos(Pos,_,_,_)],
  [
  del(av(A)), del(free(Block1)),
  add(pillaring(A, Pos, Block1))
  ]
).
action(build_pillar_end(A, Pos, Block1),
  [pillaring(A, Pos, Block1)],
  [pillar(Pos, _)],
  [],
  [agent(A), block(Block1), pos(Pos,_,_,_)],
  [
    del(pillaring(A, Pos, Block1)),
    add(av(A)), add(pillar(Pos, Block1))
  ]
).

ll_action(move_arm_start(A, To, X1, Y1, Z1, X2, Y2, Z2),
  [arm_at(A, X1, Y1, Z1)],
  [moving_arm(A, _, _, _, _, _, _), gripping(A, _), releasing(A)],
  [],
  [arm(A), pos(To, X2, Y2, Z2)],
  [
    add(moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)),
    del(arm_at(A, X1, Y1, Z1))
  ]
).
ll_action(move_arm_end(A, To, X1, Y1, Z1, X2, Y2, Z2),
  [moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)],
  [],
  [],
  [arm(A)],
  [
    del(moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)),
    add(arm_at(A, X2, Y2, Z2))
  ]
).

planner_debug(true).

agent(a1).
agent(a2).

arm(a1, 10, 10, 10).
arm(a2, 20, 20, 20).

arm(a1).

gripper(a1).
gripper(a2).

new_agent(a1, a2, a3).
new_agent(a2, a3, a7).

pos(pos1, 1, 2, 3).

block(b2).

resources :- resources(X), X, functor(X, Y, _), write(X), write(' '), write(Y), nl.
resources(new_agent(_, _, _)).
resources(agent(_)).
resources(arm(_, _, _, _)).
resources(gripper(_)).
resources(pos(_, _, _, _)).
resources(block(_)).
resources(arm(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


test_check_in_verify :- 
  format('Running\n'),
  check_in_verify(a1, [agent(a1),agent(a2)], agent(a1)),
  format('1~n'),
  check_in_verify(a2, [agent(a1),agent(a2)], agent(a2)),
  format('2~n'),
  \+check_in_verify(b, [agent(a2)], _),
  format('3~n'),
  \+check_in_verify(b, [block(b1), agent(a3), agent(a2)], _),
  format('5~n'),
  \+check_in_verify(a1, [], _),
  format('6~n'),
  check_in_verify(a1, [agent(a4), block(b1, a1)], block(b1, a1)),
  format('7~n'),
  check_in_verify(b2, [block(b2), pos(pos1, 1, 1, 1)], block(b2)),
  format('8~n'),
  true.

test_not_in_resources :-
  format('Running\n'),
  \+not_in_resources([av(a1)], [agent(a1)], _),
  format('1~n'), !,
  \+not_in_resources([], [], _),
  format('2~n'), !,
  not_in_resources([block(b1)], [agent(a1)], _),
  format('3~n'), !,
  \+not_in_resources([av(a1), av(a2)], [agent(a1), agent(a2)], _),
  format('4~n'), !,
  \+not_in_resources([av(a2), av(a1)], [agent(a1), agent(a2)], _),
  format('5~n'), !,
  not_in_resources([tree(t2), av(a2)], [point(p2), agent(a2)], _),
  format('6~n'), !,
  not_in_resources([tree(t2)], [tree(t2)], _),
  format('7~n'), !,
  not_in_resources([av(a1), free(block2)], [agent(a1)], _),
  format('8~n'), !,
  % There is b1 which is not in the resources
  not_in_resources([block(pos1, b1), free(block2)], [block(b2), pos(pos1, 1, 1, 1)], _),
  format('9~n'),
  % trace(not_in_resources),
  \+not_in_resources([block(pos1, b2)], [block(b2), pos(pos1, 1, 1, 1)], _),
  format('10~n'),
  \+not_in_resources([av(a1)], [block(block2),agent(a1),pos(b,_8786,_8788,_8790)], agent(a1)),
  format('11~n'),
  true.

test_last_achievers_ids :-
  last_achievers_ids(
    [av(a1),pillar(a,block1),free(arch1)],
    [],
    [agent(a1), block(b), block(a), arch(arch1)],
    [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
    [1]
  ),
  format('1~n'),

  last_achievers_ids(
    [av(a1),pillar(a1,block1),free(arch2)],
    [],
    [agent(a1), block(b), block(a), arch(arch1)],
    [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
    A
  ),
  format('2 ~w~n', [A]),
  
  
  true.

test_achievers :-
  % \+achiever(
  %  [av(a1),free(block2)],
  %  [pillar(b,_10558),pillaring(_10568,b,_10572)],
  %  [block(block2),agent(a1),pos(b,_10616,_10618,_10620)],
  %  [del(pillaring(a1,a,block1)),add(av(a1)),add(pillar(a,block1))],
  %  []
  % ),
  % format('1~n'),
  
  % achiever(
  %   [moving_arm(a1,a,0,0,0,1,1,0)],
  %   [],
  %   [arm(a1)],
  %   [add(moving_arm(a1,a,0,0,0,1,1,0)),del(arm_at(a1,0,0,0))],
  %   []
  % ),
  % format('2~n'),

  % \+achiever(
  %   [av(a1),free(block2)],
  %   [pillar(b,_6458),pillaring(_6468,b,_6472)],
  %   [block(block2),agent(a1),pos(b,_6516,_6518,_6520)],
  %   [add(av(a1)),add(pillar(a,block1))],
  %   []
  % ),
  % format('3~n'),

% Adding constraint that 12_move_arm_start(a1, a, 0, 0, 0, 1, 1, 0) starts after 5_grip_end(a1, block1)

  \+achiever(
    [arm_at(a1,0,0,0)],
    [moving_arm(a1, _, _, _, _, _, _), gripping(a1, _), releasing(a1)],
    [arm(a1),pos(a,1,1,0)],
    [add(gripped(a1)),add(gripper(a1,close)),del(gripping(a1,block1))],
    []
  ),
  format('4~n'),
  true.

test_apply_action :-
  \+is_applicable(
    [available(a1),available(a2),ontable(b1,2,2),ontable(b2,4,4),ontable(b4,8,8),on(b5,b1,2,2),on(b3,b2,4,4),clear(b5),clear(b3),clear(b4)],
    [ontable(B, X, Y), available(A), clear(B)],
    [gripped(_, B), gripping(_, B)],
    [ontable(B, X, Y)],
    [agent(A)]
  ),
  debug_format('B ~w X ~w Y ~w A ~w~n', [B, X, Y, A]),
  
  is_applicable(
    [available(a1),available(a2),ontable(b1,2,2),ontable(b2,4,4),ontable(b4,8,8),on(b5,b1,2,2),on(b3,b2,4,4),clear(b5),clear(b3),clear(b4)],
    [on(B1, B2, X1, Y1), available(A1), clear(B1)],
    [gripped(_, B1), gripping(_, B1)],
    [on(B1, B2, X, Y)],
    [agent(A1)]
  ),
  debug_format('B1 ~w B2 ~w X1 ~w Y1 ~w A1 ~w~n', [B1, B2, X1, Y1, A1]),
  true.

test_conditions_met :-
  conditions_met(
    [av(Agent),free(Arch)],
    [av(a2),free(b1)]
  ),
  debug_format('Agent ~w Arch ~w~n', [Agent, Arch]),
  \+conditions_met(
    [av(Agent1),free(Arch1)],
    [av(a2),block(b1)]
  ),
  which_conditions_not_met(
    [av(Agent1),free(Arch1)],
    [av(a2),block(b1)],
    R
  ),
  debug_format('R ~w~n', [R]),
  true.
