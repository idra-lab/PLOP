% init_state([
%   ontable(b1), ontable(b2),
%   at(b1,1,1), at(b2,2,2), 
%   clear(b1), clear(b2),
%   available(a1)
% ]).

% goal_state([
%   ontable(b1),
%   on(b2,b1),
%   at(b1,3,3), at(b2,3,3), 
%   clear(b2),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), on(b3, b1),
%   at(b1,1,1), at(b2,2,2), at(b3,1,1),
%   clear(b3), clear(b2),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), on(b3,b2),
%   at(b1,1,1), at(b2,2,2), at(b3,2,2),
%   clear(b1), clear(b3),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), on(b3, b1),
%   at(b1,1,1), at(b2,2,2), at(b3,1,1),
%   clear(b3), clear(b2),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), on(b3,b2),
%   at(b1,1,1), at(b2,3,3), at(b3,3,3),
%   clear(b1), clear(b3),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b4), on(b3, b4),
%   at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b3),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,4,4), at(b2,4,4), at(b3,4,4), at(b4,3,3),
%   clear(b3), clear(b4),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), on(b2, b1),
%   at(b1,1,1), at(b2,1,1),
%   clear(b2),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), 
%   at(b1,1,1), at(b2,2,2),
%   clear(b1), clear(b2),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b3),
%   on(b2, b1), 
%   at(b1,1,1), at(b2,1,1), at(b3,3,3),
%   clear(b2), clear(b3),
%   available(a1)
% ]).


% goal_state([
%   ontable(b2), ontable(b3), 
%   on(b1,b3),
%   at(b1,3,3), at(b2,2,2), at(b3,3,3),
%   clear(b1), clear(b2),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b3), ontable(b4),
%   on(b2, b1), 
%   at(b1,1,1), at(b2,1,1), at(b3,3,3), at(b4,4,4),
%   clear(b2), clear(b3), clear(b4),
%   available(a1)
% ]).


% goal_state([
%   ontable(b2), ontable(b3), 
%   on(b1,b3), on(b4,b1),
%   at(b1,3,3), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b2), clear(b4), 
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b3), on(b4, b3),
%   at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b4),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,4,4), at(b2,4,4), at(b3,4,4), at(b4,2,2),
%   clear(b3), clear(b4),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b3), on(b4, b3),
%   at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b4),
%   available(a1)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,2,2), at(b2,2,2), at(b3,2,2), at(b4,1,1),
%   clear(b3), clear(b4),
%   available(a1)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOT WORKING
init_state([
  ontable(b1), ontable(b2), ontable(b3), on(b4, b3),
  at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3),
  clear(b1), clear(b2), clear(b4),
  available(a1)
]).


goal_state([
  ontable(b1), ontable(b4), 
  on(b2,b1), on(b3,b2),
  at(b1,2,2), at(b2,2,2), at(b3,2,2), at(b4,3,3),
  clear(b3), clear(b4),
  available(a1)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% goal_state([
%   ontable(b1), at(b1,5,5), on(b6,b1), at(b6,5,5), clear(b6), 
%   ontable(b2), at(b2,2,2), clear(b2), ontable(b3), at(b3,3,3), 
%   on(b4,b3), at(b4,3,3), clear(b4), 
%   available(a1)
% ]).s