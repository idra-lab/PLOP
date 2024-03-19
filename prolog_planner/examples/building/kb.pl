:-dynamic block/1.
:-dynamic agent/1.
:-dynamic arch/1.
:-dynamic pos/4.
:-dynamic arm/4.
:-dynamic gripper/1.

block(block1).
block(block2).

agent(a1).

arch(arch1).

pos(a, 1, 1, 0).
pos(b, 2, 2, 0).
pos(c, 3, 4, 0).
pos(d, 5, 5, 0).
pos(e, 9, 9, 0).
pos(f, 1.5, 1.5, 0).

arm(a1, 10, 10, 10).

gripper(a1).