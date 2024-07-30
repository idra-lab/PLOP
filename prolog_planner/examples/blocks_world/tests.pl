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


