hl_init([
  % HL
  av(a1), free(block1), free(block2), free(arch1),
  % INT
  at(c, block1), at(d, block2), at(e, arch1),
  % LL
  arm_at(a1, 0,0,0)
]).

hl_goal([
  % HL
  av(a1), pillar(a, block1), pillar(b, block2), arch(a, b, arch1),
  % INT
  at(a, block1), at(b, block2), at(f, arch1),
  % LL 
  arm_at(a1, 0,0,0)
]).


% move_block_start()
% move_block_end()


% s0 av(a1), free(block1), free(block2), free(arch1), at(c, block1), at(d, block2), at(e, arch1), arm_at(a1, 0,0,0)
% build_pillar_start()
% s1 pillaring(a1, block1, a), free(block2), free(arch1), at(c, block1), at(d, block2), at(e, arch1), arm_at(a1, 0,0,0)
% move_block_start()
% s2 moving_block, pillaring(a1, block1, a), free(block2), free(arch1), at(c, block1), at(d, block2), at(e, arch1), arm_at(a1, 0,0,0)
