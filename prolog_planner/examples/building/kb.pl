:-dynamic block/1.
:-dynamic agent/1.
:-dynamic arch/1.
:-dynamic pos/4.
:-dynamic arm/4.
:-dynamic gripper/1.

:- discontiguous resources/1.

block(block1).
block(block2).
block(block3).
block(block4).

agent(a1).
agent(a2).

arch(arch1).
arch(arch2).

pos(a, 1, 1, 0).
pos(b, 2, 2, 0).
pos(c, 3, 4, 0).
pos(d, 5, 5, 0).
pos(e, 9, 9, 0).
pos(f, 1.5, 1.5, 0).
pos(g, 0, 0, 0).

posi(_, X, Y, Z) :- 
  pos(_, X1, Y1, Z1), 
  pos(_, X2, Y2, _),
  X is (X1 + X2)/2, 
  Y is (Y1 + Y2)/2,
  Z is Z1.

arm(a1).
arm(a2).

gripper(a1).
gripper(a2).

resources(agent(_)).
resources(arm(_)).
resources(gripper(_)).

