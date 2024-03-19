mapping(build_pillar_start(A, Pos, Block),
  [
    move_block_start(A, Block, From, Pos),
    move_block_end(A, Block, From, Pos)
  ]
).
mapping(place_arch_start(A, Pos1, Pos2, Arch),
  [
    move_arch_start(A, Arch, From, To, Pos1, Pos2),
    move_arch_end(A, Arch, From, To, Pos1, Pos2)
  ]
).
mapping(move_block_start(A, Block, From, To),
  [
    move_arm_start(A, From),
    move_arm_end(A, From),
    grip_start(A, Block),
    grip_end(A, Block),
    move_arm_start(A, To),
    move_arm_end(A, To),
    release_start(A),
    release_end(A)
  ]
).
mapping(move_arch_start(A, Arch, From, To),
  [
    move_arm_start(A, From),
    move_arm_end(A, From),
    grip_start(A, Arch),
    grip_end(A, Arch),
    move_arm_start(A, To),
    move_arm_end(A, To),
    release_start(A),
    release_end(A)
  ]
).