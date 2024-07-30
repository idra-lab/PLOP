% Move Block1 from (X1,Y1) on the table to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
action(
  move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
  [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
  [
    del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
    add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
  ]
).
action(
  move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent)],
  [
    del(clear(Block2)), del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
    add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).
% Move a block from a position (X1,Y1) to another position (X2,Y2) on the table. 
action(
  move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2), 
  [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
  [at(_, X2, Y2), on(Block, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
  [
    del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
    add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
  ]
).
action(
  move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
  [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
  [at(X2, Y2)],
  [],
  [agent(Agent)],
  [
    del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
    add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
  ]
).
% Move Block1 from (X1,Y1) on top of another block to the table in (X2,Y2).
action(
  move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
  [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
    add(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
  ]
).
action(
  move_onblock_to_table_end(Agent, Block1, X1, Y1, X2, Y2),
  [moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent)],
  [
    del(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)),
    add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).
% Move Block1 from (X1,Y1) on top of another block to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
action(
  move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
  [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 \= Block2, Block1 \= Block3, Block2 \= Block3],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
    add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
  ]
).
action(
  move_onblock_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent)],
  [
    del(clear(Block2)), del(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
    add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOW-LEVEL ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ll_action(grip_start(A),
  [],
  [ll_arm_gripping(A), ll_gripped(A)],
  [],
  [arm(A)],
  [
    add(ll_arm_gripping(A))
  ]
).

ll_action(grip_end(A),
  [ll_arm_gripping(A)],
  [],
  [],
  [arm(A)],
  [
    del(ll_arm_gripping(A)),
    add(ll_gripped(A))
  ]
).

ll_action(release_start(A),
  [ll_gripped(A)],
  [],
  [],
  [arm(A)],
  [
    add(ll_arm_releasing(A))
  ]
).

ll_action(release_end(A),
  [ll_arm_releasing(A)],
  [],
  [],
  [arm(A)],
  [
    del(ll_arm_releasing(A)),
    del(ll_gripped(A))
  ]
).

ll_action(move_arm_start(A, X1, Y1, X2, Y2),
  [ll_at_arm(A, X1, Y1)],
  [ll_at_arm(A1, X2, Y2), ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
  [],
  [arm(A), pos(X1, Y1), pos(X2, Y2)],
  [
    del(ll_at_arm(A, X1, Y1)),
    add(ll_moving(A, X1, Y1, X2, Y2))
  ]
).

ll_action(move_arm_end(A, X1, Y1, X2, Y2),
  [ll_moving(A, X1, Y1, X2, Y2)],
  [ll_at_arm(_, X2, Y2)],
  [],
  [arm(A)],
  [
    del(ll_moving(A, X1, Y1, X2, Y2)),
    add(ll_at_arm(A, X2, Y2))
  ]
).

% This solves the situation in which (X1,Y1)=(X2,Y2) meaning that the arm is already in the desired position.
ll_action(move_arm_start(A, X1, Y1, X2, Y2),
  [ll_at_arm(A, X1, Y1), ll_at_arm(A1, X2, Y2)],
  [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
  [],
  [arm(A), pos(X1, Y1), pos(X2, Y2)],
  [
    del(ll_at_arm(A, X1, Y1)),
    add(ll_moving(A, X1, Y1, X2, Y2))
  ]
).

