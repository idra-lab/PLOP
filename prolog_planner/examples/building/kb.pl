%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             HL Knowledge Base                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-discontiguous point/1.

point(a).
point(b).
point(c).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             LL Knowledge Base                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-discontiguous block/4.
:-discontiguous arm/1.
:-discontiguous point/2.
:-discontiguous wheeled/1.

% Define the points on the grid
point(1,1).
point(1,2).
point(1,3).
point(2,1).
point(2,2).
point(2,3).
point(3,1).
point(3,2).
point(3,3).

% Define the blocks and their initial positions
block(block1, 1, 1, 0).
block(block2, 2, 2, 0).
block(block3, 3, 3, 0).
block(block4, 4, 4, 0).
block(block5, 5, 5, 0).
block(block6, 6, 6, 0).
block(architrave, 7, 7, 0).

% Define the robotic arm
arm(r1).

% Define the wheeled robots
wheeled(r1).
