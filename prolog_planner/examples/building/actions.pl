%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                     HL                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  [],
  [
    del(pillaring(A, Pos, Block1)),
    add(av(A)), add(pillar(Pos, Block1))
  ]
).
action(place_arch_start(A, Pos1, Pos2, Arch),
  [av(A), pillar(Pos1, _), pillar(Pos2, _), free(Arch)],
  [placing_arch(_, Pos1, Pos2, _), placed_arch(Pos1, Pos2, _)],
  [placed_arch(Pos1, Pos2, Arch)],
  [arch(Arch), agent(A), pos(Pos1,_,_,_), pos(Pos2,_,_,_), Pos1\=Pos2],
  [
    del(av(A)), del(free(Arch)),
    add(placing_arch(A, Pos1, Pos2, Arch))
  ]
).
action(place_arch_end(A, Pos1, Pos2, Arch),
  [placing_arch(A, Pos1, Pos2, Arch)],
  [arch(_, Pos1, Pos2)],
  [],
  [],
  [
    del(placing_arch(A, Pos1, Pos2, Arch)),
    add(arch(Pos1, Pos2, Arch)), add(av(A))
  ]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    INT                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ll_action(move_block_start(A, Block, From, To),
  [pillaring(A, To, Block), at(From, Block)],
  [at(To, _)],
  [at(To, Block)],
  [],
  [
    del(at(From, Block)),
    add(moving_block(A, Block, From, To))
  ]
).
ll_action(move_block_end(A, Block, From, To),
  [moving_block(A, Block, From, To)],
  [at(To, _)],
  [],
  [],
  [
    del(moving_block(A, Block, From, To)),
    add(at(To, Block))
  ]
).
ll_action(move_arch_start(A, Arch, From, To, Pos1, Pos2),
  [placing_arch(A, Pos1, Pos2, Arch), at(From, Arch)],
  [at(To, _)],
  [at(To, Arch)],
  [
    % pos(Pos1, X1, Y1, Z1),pos(Pos2, X2, Y2, Z2),
    % Xf is (X1+X2)/2.0, Yf is (Y1+Y2)/2.0, Zf is (Z1+Z2)/2.0,
    % format('X ~w Y ~w Z ~w~n', [Xf, Yf, Zf]),
    % \+pos(_, Xf, Yf, Zf) -> assertz(pos(To, Xf, Yf, Zf)) ; true
  ],
  [
    del(at(From, Arch)),
    add(moving_arch(A, Arch, From, To))
  ]
).
ll_action(move_arch_end(A, Arch, From, To, _Pos1, _Pos2),
  [moving_arch(A, Arch, From, To)],
  [at(To, _)],
  [],
  [],
  [
    del(moving_arch(A, Arch, From, To)),
    add(at(To, Arch))
  ]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                     LL                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ll_action(move_arm_start(A, To),
  [],
  [moving_arm(A, _), gripping(A, _), releasing(A)],
  [],
  [arm(A, X1, Y1, Z1), pos(To, X2, Y2, Z2), retract(arm(A, X1, Y1, Z1))],
  [
    add(moving_arm(A, X2, Y2, Z2))
  ]
).
ll_action(move_arm_end(A, To),
  [moving_arm(A, X, Y, Z)],
  [],
  [],
  [assertz(arm(A, X, Y, Z))],
  [
    del(moving_arm(A, X, Y, Z))
  ]
).
ll_action(grip_start(A, B),
  [],
  [moving_arm(A, _), gripping(A, _), releasing(A)],
  [],
  [gripper(A)],
  [
    add(gripping(A, B))
  ]
).
ll_action(grip_end(A, B),
  [gripping(A, B)],
  [],
  [],
  [gripper(A)],
  [
    del(gripping(A, B))
  ]
).
ll_action(release_start(A),
  [],
  [moving_arm(A, _), gripping(A, _), releasing(A)],
  [],
  [gripper(A)],
  [
    add(releasing(A))
  ]
).
ll_action(release_end(A),
  [releasing(A)],
  [],
  [],
  [gripper(A)],
  [
    del(releasing(A))
  ]
).