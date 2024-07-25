init_state([
  ontable(b1), ontable(b2),
  at(b1,1,1), at(b2,2,2), 
  clear(b1), clear(b2),
  available(a1)
]).

% goal_state([
%   ontable(b1), at(b1,5,5), on(b6,b1), at(b6,5,5), clear(b6), 
%   ontable(b2), at(b2,2,2), clear(b2), ontable(b3), at(b3,3,3), 
%   on(b4,b3), at(b4,3,3), clear(b4), 
%   available(a1)
% ]).

goal_state([
  ontable(b1),
  on(b2,b1),
  at(b1,3,3), at(b2,3,3), 
  clear(b2),
  available(a1)
]).

