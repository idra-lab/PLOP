:- ensure_loaded('full_planner.pl').

planner_debug(true).
% planner_debug(false).

plan(Actions, AdjMatrix, TTActionList, Resources, ActionXResources) :-
  init_state(Init),
  goal_state(Goal),
  debug_format('Planning from: ~w to: ~w~n', [Init, Goal]),
  % leash(-all), etrace,
  % extract_hl_goal(Goal, HLGoal),
  generate_plan(Init, Goal, TOActions, LastAchievers),
  debug_format('Total-order plan: ~n'),
  reverse(TOActions, TOActionsReversed),
  print_list(TOActionsReversed),
  debug_format('Last achievers: ~n'),
  reverse(LastAchievers, LastAchieversReversed),
  print_list(LastAchieversReversed),
  nl,nl,nl,
  
  extract_adj_matrix_actions(LastAchievers, AdjMatrix, Actions),
  debug_format('Adjacency matrix:~n'),
  print_list(AdjMatrix),
  debug_format('Actions:~n'),
  print_list(Actions),
  nl,nl,nl,

  extract_tt_action_list(Actions, TTActionList),
  debug_format('Time-triggered actions:~n'),
  print_list(TTActionList),
  nl,nl,nl,

  extract_resources_number(Resources),
  debug_format('Resources:~n'),
  print_list(Resources),
  nl,nl,nl,

  extract_resources_per_action(TTActionList, Resources, ActionXResources),
  debug_format('Resources per action:~n'),
  print_list(ActionXResources),
  nl,nl,nl,

  true.

plan :-
  plan(_, _, _, _, _).  

