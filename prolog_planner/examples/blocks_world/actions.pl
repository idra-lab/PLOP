% Move Block1 from (X1,Y1) on the table to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
action(
  move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
  [on(_, Block1), moving_table_to_table(_, Block, X1, Y1, _, _), moving_table_to_block(_, Block, _, X1, Y1, _, _)],
  [ontable(Block1), at(Block1, X1, Y1)],
  [pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
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
  [],
  [
    del(clear(Block2)), del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
    add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).
% Move a block from a position (X1,Y1) to another position (X2,Y2) on the table. 
action(
  move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2), 
  [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
  [at(_, X2, Y2), moving_table_to_table(_, Block, X1, Y1, _, _), moving_table_to_block(_, Block, _, X1, Y1, _, _)],
  [ontable(Block), at(Block, X1, Y1)],
  [pos(X1, Y1), pos(X2, Y2), block(Block)],
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
  [],
  [
    del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
    add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
  ]
).
% Move Block1 from (X1,Y1) on top of another block to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
% action(
%   move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
%   [moving(_, Block1), on(_, Block1)],
%   [on(Block1, Block3), at(Block1, X1, Y1)],
%   [pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3)],
%   [
%     del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
%     add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
%   ]
% ).
% action(
%   move_onblock_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
%   [],
%   [],
%   [],
%   [
%     del(clear(Block2)), del(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
%     add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1))
%   ]
% ).
% % Move Block1 from (X1,Y1) on top of another block to the table in (X2,Y2). 
% action(
%   move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
%   [moving(_, Block1), on(_, Block1), at(_, X2, Y2)],
%   [on(Block1, Block2), at(Block1, X1, Y1)],
%   [pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2)],
%   [
%     del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
%     add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block2))
%   ]
% ).
% action(
%   move_onblock_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
%   [at(_, X2, Y2)],
%   [],
%   [],
%   [
%     del(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
%     add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1))
%   ]
% ).
