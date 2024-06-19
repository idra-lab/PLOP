mapping(
  move_block_start(Agent, Block, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    close_gripper_start(Agent),
    close_gripper_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_gripper_start(Agent),
    open_gripper_end(Agent)
  ]
).

mapping(
  move_block_on_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    close_gripper_start(Agent),
    close_gripper_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_gripper_start(Agent),
    open_gripper_end(Agent)
  ]
).