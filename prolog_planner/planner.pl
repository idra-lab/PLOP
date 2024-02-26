:- ensure_loaded('ll_planner.pl').
:- ensure_loaded('hl_planner.pl').

plan :- 
  hl_init(Init),
  hl_goal(Goal),
  total_hl_plan(Init, Goal, HLActions),
  write('HL total-order Plan: '), nl,
  reverse(HLActions, HLActionsReversed),
  print_list(HLActionsReversed),
  partial_hl_plan(Init, HLActionsReversed, HLGraph),
  format('HL total-order Plan: ~w~n', [HLGraph]),

  write('mapping to LL actions: '), nl,
  trace,
  total_ll_order(HLActionsReversed, LLActions),
  write('Finihed mapping'), nl,
  % write('Partial LL Plan: '), nl,
  % partial_ll_order(LLActions)
  true.