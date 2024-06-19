hl_init([
  % HL
  av(a1), free(block1), free(block2), free(arch1),
  % INT
  at(c, block1), at(d, block2), at(e, arch1),
  % LL
  arm_at(a1, 0, 0, 0), gripper(a1, open)
]).

hl_goal([
  % HL
  av(a1), pillar(a, block1), pillar(b, block2), arch(a, b, arch1),
  % INT
  at(a, block1), at(b, block2), at(f, arch1),
  % LL 
  arm_at(a1, _, _, _), gripper(a1, _)
]).
