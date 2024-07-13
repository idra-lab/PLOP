init_state([
  % HL
  av(a1), free(block1), free(block2), free(arch1),
  % LL
  arm_at(a1, 0, 0, 0), gripper(a1, open)
]).

goal_state([
  % HL
  av(a1), pillar(a, block1), pillar(b, block2), arch(a, b, arch1, e),
  % LL 
  arm_at(a1, _, _, _), gripper(a1, _)
]).

% init_state([
%   % HL
%   av(a1), free(block1), free(block2), free(arch1), free(block3), free(block4), free(arch2),
%   % LL
%   arm_at(a1, 0, 0, 0), gripper(a1, open)
% ]).

% goal_state([
%   % HL
%   av(a1), pillar(a, block1), pillar(b, block2), arch(a, b, arch1, e), pillar(c, block3), pillar(d, block4), arch(c, d, arch2, f),
%   % LL 
%   arm_at(a1, _, _, _), gripper(a1, open)
% ]).

