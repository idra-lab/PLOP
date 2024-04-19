:- ensure_loaded('full_planner.pl').

plan(Actions, AdjMatrix, TTActionList, Resources, ActionXResources) :-
  hl_init(Init),
  hl_goal(Goal),
  % format('Planning from: ~w to: ~w~n', [Init, Goal]),
  generate_plan(Init, Goal, HLActions, LastAchievers),
  % write('HL total-order plan: '), nl,
  reverse(HLActions, HLActionsReversed),
  % print_list(HLActionsReversed),
  % write('Last achievers: '), nl,
  reverse(LastAchievers, LastAchieversReversed),
  % print_list(LastAchieversReversed),
  nl,nl,nl,
  
  extract_adj_matrix_actions(LastAchievers, AdjMatrix, Actions),
  % format('Adjacency matrix:~n'),
  % print_list(AdjMatrix),
  % format('Actions:~n'),
  % print_list(Actions),
  nl,nl,nl,

  extract_tt_action_list(Actions, TTActionList),
  % format('Time-triggered actions:~n'),
  % print_list(TTActionList),
  nl,nl,nl,

  extract_resources_number(Resources),
  % format('Resources:~n'),
  % print_list(Resources),
  nl,nl,nl,

  extract_resources_per_action(TTActionList, Resources, ActionXResources),
  % format('Resources per action:~n'),
  % print_list(ActionXResources),
  nl,nl,nl,

  true.

plan :-
  plan(_, _, _, _, _).  

