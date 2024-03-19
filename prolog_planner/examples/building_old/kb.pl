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
:-discontiguous point/4.
:-discontiguous wheeled/1.

% Define the points on the grid
point(point1, 1, 1, 0).
point(point2, 1, 2, 0).
point(point3, 1, 3, 0).
point(point4, 2, 1, 0).
point(point5, 2, 2, 0).
point(point6, 2, 3, 0).
point(point7, 3, 1, 0).
point(point8, 3, 2, 0).
point(point9, 3, 3, 0).
point(point10, 4, 4, 0).
point(point11, 5, 5, 0).
point(architrave, 9, 9, 0).

% Define the robotic arm
arm(r1).

% Define the wheeled robots
wheeled(r1).
