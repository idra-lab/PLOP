mapping(build_pillar_start(A, Pos, Block),
  [
    move_arm_start(A, _, _, _, _, _, _, _),
    move_arm_end(A, _, _, _, _, _, _, _),
    grip_start(A, Block),
    grip_end(A, Block),
    move_arm_start(A, Pos, _, _, _, _, _, _),
    move_arm_end(A, Pos, _, _, _, _, _, _),
    release_start(A),
    release_end(A)
  ]
).
mapping(place_arch_start(A, Pos1, Pos2, Arch, To),
  [
    move_arm_start(A, _, _, _, _, _, _, _),
    move_arm_end(A, _, _, _, _, _, _, _),
    grip_start(A, Arch),
    grip_end(A, Arch),
    move_arm_start(A, To, _, _, _, _, _, _),
    move_arm_end(A, To, _, _, _, _, _, _),
    release_start(A),
    release_end(A)
  ]
).

