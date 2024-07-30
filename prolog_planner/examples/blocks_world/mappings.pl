:- discontiguous mapping/2.

mapping(move_table_to_table_start(Agent, _, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    grip_start(Agent),
    grip_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    release_start(Agent),
    release_end(Agent)
    % home_start(Agent),
    % home_end(Agent)
    
  ]
).

mapping(move_table_to_block_start(Agent, _, _, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    grip_start(Agent),
    grip_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    release_start(Agent),
    release_end(Agent)
    % home_start(Agent),
    % home_end(Agent)
  ]
).

mapping(move_onblock_to_table_start(Agent, _, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    grip_start(Agent),
    grip_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    release_start(Agent),
    release_end(Agent)
    % home_start(Agent),
    % home_end(Agent)
  ]
).

mapping(move_onblock_to_block_start(Agent, _, _, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, _, _, X1, Y1),
    move_arm_end(Agent, _, _, X1, Y1),
    grip_start(Agent),
    grip_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    release_start(Agent),
    release_end(Agent)
    % home_start(Agent),
    % home_end(Agent)
  ]
).