%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given an initial state in which there are two blocks b1, b2 in positions (1,1) and (2,2) 
% respectively, move the block b1 to position (3,3) and place b2 on top of b1 using an agent, which
% is initially available and it will also be available at the end.

% init_state([
%   ontable(b1), ontable(b2),
%   at(b1,1,1), at(b2,2,2), 
%   clear(b1), clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).

% goal_state([
%   ontable(b1),
%   on(b2,b1),
%   at(b1,3,3), at(b2,3,3), 
%   clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given an initial state in which there are two blocks b1, b2 on the table in positions (1,1), 
% (2,2), respectively, and a third block b3 on top of b1 in position (1,1), move the block b3 on top
% of b2 using an agent, which is initially available and it will also be available at the end.

% init_state([
%   ontable(b1), ontable(b2), on(b3, b1),
%   at(b1,1,1), at(b2,2,2), at(b3,1,1),
%   clear(b3), clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), on(b3,b2),
%   at(b1,1,1), at(b2,2,2), at(b3,2,2),
%   clear(b1), clear(b3),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), on(b3, b1),
%   at(b1,1,1), at(b2,2,2), at(b3,1,1),
%   clear(b3), clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), on(b3,b2),
%   at(b1,1,1), at(b2,3,3), at(b3,3,3),
%   clear(b1), clear(b3),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b4), on(b3, b4),
%   at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b3),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,4,4), at(b2,4,4), at(b3,4,4), at(b4,3,3),
%   clear(b3), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), on(b2, b1),
%   at(b1,1,1), at(b2,1,1),
%   clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b2), 
%   at(b1,1,1), at(b2,2,2),
%   clear(b1), clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b3),
%   on(b2, b1), 
%   at(b1,1,1), at(b2,1,1), at(b3,3,3),
%   clear(b2), clear(b3),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b2), ontable(b3), 
%   on(b1,b3),
%   at(b1,3,3), at(b2,2,2), at(b3,3,3),
%   clear(b1), clear(b2),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b3), ontable(b4),
%   on(b2, b1), 
%   at(b1,1,1), at(b2,1,1), at(b3,3,3), at(b4,4,4),
%   clear(b2), clear(b3), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b2), ontable(b3), 
%   on(b1,b3), on(b4,b1),
%   at(b1,3,3), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b2), clear(b4), 
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b3), on(b4, b3),
%   at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,4,4), at(b2,4,4), at(b3,4,4), at(b4,2,2),
%   clear(b3), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_state([
%   ontable(b1), ontable(b2), ontable(b3), on(b4, b3),
%   at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3),
%   clear(b1), clear(b2), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), 
%   on(b2,b1), on(b3,b2),
%   at(b1,2,2), at(b2,2,2), at(b3,2,2), at(b4,1,1),
%   clear(b3), clear(b4),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given an initial state in which there are four blocks b1, b2, b3, b5 on the table in positions
% (2,2), (1,1), (3,3), (4,4), respectively, and a fifth block b4 on top of b3 in position (3,3),
% move the block b2 on top of b1, b3 on top of b2 and place b5 on the table in position (0,0) and 
% b4 on the table in position (3,3). The agent is initially available and it will also be available 
% at the end.

% % This HL plan has length 10 and must be run without debug, oterwise it will take too long
% init_state([
%   ontable(b1), ontable(b2), ontable(b3), on(b4, b3), ontable(b5),
%   at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3), at(b5,4,4),
%   clear(b1), clear(b2), clear(b4), clear(b5),
%   available(a1),
%   % ----
%   ll_at_arm(a1,0,0)
% ]).


% goal_state([
%   ontable(b1), ontable(b4), ontable(b5),
%   on(b2,b1), on(b3,b2),
%   at(b1,2,2), at(b2,2,2), at(b3,2,2), at(b4,3,3), at(b5,0,0),
%   clear(b3), clear(b4), clear(b5),
%   available(a1),
%   % ----
%   ll_at_arm(a1,_,_)
% ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state([
  ontable(b1), ontable(b2), on(b3,b2),
  at(b1,1,1), at(b2,2,2), at(b3,2,2),
  clear(b1), clear(b3),
  available(a1),
  % ----
  ll_at_arm(a1,0,0)
]).


goal_state([
  ontable(b1), ontable(b2), ontable(b3),
  at(b1,3,3), at(b2,4,4), at(b3,0,0),
  clear(b1), clear(b2), clear(b3),
  available(a1),
  % ----
  ll_at_arm(a1,_,_)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


