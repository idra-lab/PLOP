% :- ensure_loaded('ll_planner.pl').
% :- ensure_loaded('hl_planner.pl').
:- ensure_loaded('full_planner.pl').

plan :- 
  hl_init(Init),
  hl_goal(Goal),
  format('Planning from: ~w to: ~w~n', [Init, Goal]),
  plan(Init, Goal, HLActions),
  write('HL total-order plan: '), nl,
  reverse(HLActions, HLActionsReversed),
  print_list(HLActionsReversed),
  % partial_hl_plan(HLActionsReversed, HLGraph),
  % format('HL partial-order plan: ~w~n', [HLGraph]),

  % write('mapping to LL actions: '), nl,
  % total_ll_plan(HLActionsReversed, LLActions),
  % write('Total LL order: '), nl, print_list(LLActions),
  % write('Partial LL plan: '), nl,
  % partial_ll_plan(LLActions, HLGraph, LLGraph),
  % format('LL partial-order plan: ~w~n', [LLGraph]),
  true.