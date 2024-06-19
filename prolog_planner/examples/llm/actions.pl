action(
  move_block_start(Agent, Block, X1, Y1, X2, Y2),
  [available(Agent), onTable(Block, X1, Y1), clear(Block)],
  [onTable(_, X2, Y2)],
  [onTable(Block, X2, Y2)],
  [block(Block), agent(Agent), pos(X1, Y1), pos(X2, Y2)],
  [del(available(Agent)), del(onTable(Block, X1, Y1)), add(moving(Agent, Block, X1, Y1, X2, Y2))]
).

action(
  move_block_end(Agent, Block, X1, Y1, X2, Y2),
  [moving(Agent, Block, X1, Y1, X2, Y2)],
  [],
  [],
  [],
  [del(moving(Agent, Block, X1, Y1, X2, Y2)), add(available(Agent)), add(onTable(Block, X2, Y2))]
).

action(
  move_block_on_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), onTable(Block1, X1, Y1), clear(Block1), onTable(Block2, X2, Y2), clear(Block2)],
  [on(_, Block2, X2, Y2)],
  [on(Block1, Block2, X2, Y2)],
  [block(Block1), block(Block2), agent(Agent), pos(X1, Y1), pos(X2, Y2)],
  [del(available(Agent)), del(onTable(Block1, X1, Y1)), del(clear(Block2)), add(moving_on_block(Agent, Block1, Block2, X1, Y1, X2, Y2))]
).

action(
  move_block_on_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving_on_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
  [],
  [],
  [],
  [del(moving_on_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(available(Agent)), add(on(Block1, Block2, X2, Y2))]
).


ll_action(
  move_arm_start(Agent, X1, Y1, X2, Y2),
  [available(Agent), arm_at(Agent, X1, Y1)],
  [moving_arm(Agent, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2)],
  [del(available(Agent)), del(arm_at(Agent, X1, Y1)), add(moving_arm(Agent, X1, Y1, X2, Y2))]
).

ll_action(
  move_arm_end(Agent, X1, Y1, X2, Y2),
  [moving_arm(Agent, X1, Y1, X2, Y2)],
  [],
  [],
  [],
  [del(moving_arm(Agent, X1, Y1, X2, Y2)), add(available(Agent)), add(arm_at(Agent, X2, Y2))]
).

ll_action(
  close_gripper_start(Agent),
  [available(Agent), gripper_open(Agent)],
  [gripping(Agent)],
  [],
  [agent(Agent)],
  [del(available(Agent)), del(gripper_open(Agent)), add(gripping(Agent))]
).

ll_action(
  close_gripper_end(Agent),
  [gripping(Agent)],
  [],
  [],
  [],
  [del(gripping(Agent)), add(available(Agent)), add(gripper_close(Agent))]
).

ll_action(
  open_gripper_start(Agent),
  [available(Agent), gripper_close(Agent)],
  [releasing(Agent)],
  [],
  [agent(Agent)],
  [del(available(Agent)), del(gripper_close(Agent)), add(releasing(Agent))]
).

ll_action(
  open_gripper_end(Agent),
  [releasing(Agent)],
  [],
  [],
  [],
  [del(releasing(Agent)), add(available(Agent)), add(gripper_open(Agent))]
).