:- ensure_loaded('ll_planner.pl').
:- ensure_loaded('hl_planner.pl').

plan :- 
  hl_init(Init),
  hl_goal(Goal),
  total_hl_plan(Init, Goal, HL_Actions),
  write('HL Plan: '), nl,
  print_list(HL_Actions),
  write('mapping to LL actions: '), nl,
  % trace,
  total_ll_order(HL_Actions, LL_Actions),
  write('Finihed mapping'), nl,
  true.