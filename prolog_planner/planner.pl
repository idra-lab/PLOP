% :- ensure_loaded('ll_planner.pl').
% :- ensure_loaded('hl_planner.pl').
:- ensure_loaded('full_planner.pl').

plan :- 
  hl_init(Init),
  hl_goal(Goal),
  format('Planning from: ~w to: ~w~n', [Init, Goal]),
  plan(Init, Goal, HLActions, LastAchievers),
  write('HL total-order plan: '), nl,
  reverse(HLActions, HLActionsReversed),
  print_list(HLActionsReversed),
  write('Last achievers: '), nl,
  reverse(LastAchievers, LastAchieversReversed),
  print_list(LastAchieversReversed),
  nl,nl,nl,
  
  extract_adj_matrix_actions(LastAchievers, AdjMatrix, Actions),
  print_list(AdjMatrix),
  print_list(Actions),
  nl,nl,nl,

  % extract_tt_action_list(Actions, TTActionList),
  % print_list(TTActionList),
  % nl,nl,nl,

  % extract_action_list(LastAchieversReversed, ActionList),
  % print_list(ActionList),
  % nl,nl,nl,

  % extract_resources_number(Resources),
  % print_list(Resources),
  % nl,nl,nl,

  % extract_resources_per_action(ActionList, ActionXResources),

  true.

gen_first_row(TmpRow, 1, Row) :-
  append([0], TmpRow, Row).
gen_first_row(TmpRow, Length, Row) :-
  NewLength is Length - 1,
  append([1], TmpRow, NewTmpRow),
  gen_first_row(NewTmpRow, NewLength, Row).

append_first_row(Matrix, Length, RetMatrix) :-
  gen_first_row([], Length, Row),
  append([Row], Matrix, RetMatrix).

gen_last_row(Row, 0, Row).
gen_last_row(TmpRow, Length, Row) :-
  NewLength is Length - 1,
  append([0], TmpRow, NewTmpRow),
  gen_last_row(NewTmpRow, NewLength, Row).

append_last_row(Matrix, Length, RetMatrix) :-
  gen_last_row([], Length, Row),
  append([Row], Matrix, RetMatrix).

extract_adj_matrix_actions(LastAchievers, RetMatrix, Actions):-
  length(LastAchievers, PlanLength),
  ActualLength is PlanLength + 2,
  append_first_row([], ActualLength, TmpMatrix),
  extract_adj_matrix_actions(LastAchievers, TmpMatrix, TmpMatrix2, PlanLength, 0, [], Actions),
  append_last_row(TmpMatrix2, ActualLength, TmpMatrix3),
  reverse(TmpMatrix3, RetMatrix).  

extract_adj_matrix_actions(_LastAchievers, RetMatrix, RetMatrix, PlanLength, PlanLength, RetActions, RetActions).
extract_adj_matrix_actions(LastAchievers, TmpMatrix, RetMatrix, PlanLength, PlanID, TmpActions, RetActions):-
  extract_row_achievers(LastAchievers, PlanID, PlanLength, [1], Row),
  append([Row], TmpMatrix, NewTmpMatrix),
  NewPlanID is PlanID + 1,
  nth0(PlanID, LastAchievers, _ID-Action-_Achievers),
  MatrixPos is PlanLength - PlanID,
  append([MatrixPos-Action], TmpActions, NewTmpActions),
  extract_adj_matrix_actions(LastAchievers, NewTmpMatrix, RetMatrix, PlanLength, NewPlanID, NewTmpActions, RetActions).

extract_row_achievers(_, _RefActionID, 0, TmpRow, RetRow) :-
  append([0], TmpRow, RetRow),
  length(TmpRow, Length).
extract_row_achievers([_ID-_A-AAchievers|T], RefActionID, I, TmpRow, RetRow):-
  (
    member(RefActionID, AAchievers) 
    -> 
      append([1], TmpRow, NewTmpRow)
    ; 
      append([0], TmpRow, NewTmpRow)
  ),
  NewI is I - 1,
  extract_row_achievers(T, RefActionID, NewI, NewTmpRow, RetRow).

% Extract the dictionary of time-triggered actions
extract_tt_action_list(Actions, TTActionList) :-
  extract_tt_action_list(Actions, [0-meta_action()], TmpTTActionList),
  reverse(TmpTTActionList, TTActionList).

extract_tt_action_list([], TTActionList, TTActionList).
extract_tt_action_list([ID-Action|T], TmpTTActionList, TTActionList) :-
  ll_action(Action, _, _, _, _, _),
  format('Action: ~w~n', [Action]),
  \+mapping(Action, _),
  format(' has no mapping~n'),
  append([Action], TmpTTActionList, NewTmpTTActionList),
  extract_tt_action_list(T, NewTmpTTActionList, TTActionList).

% Extract the dictionary of actions
extract_action_list(LastAchievers, ActionList) :-
  extract_action_list(LastAchievers, ['0_s'], TmpActionList),
  append(['0_e'], TmpActionList, TmpTmpActionList),
  reverse(TmpTmpActionList, ActionList).

extract_action_list([], ActionList, ActionList).
extract_action_list([_ID-Action-_Achievers|T], TmpActionList, ActionList) :-
  ll_action(Action, _, _, _, _, _),
  format('Action: ~w~n', [Action]),
  \+mapping(Action, _),
  format(' has no mapping~n'),
  append([Action], TmpActionList, NewTmpActionList),
  extract_action_list(T, NewTmpActionList, ActionList).
extract_action_list([_ID-Action-_Achievers|T], TmpActionList, ActionList) :-
  action(Action, _, _, _, _, _),
  format('Action: ~w~n', [Action]),
  \+mapping(Action, _),
  format(' has mapping~n'),
  append([Action], TmpActionList, NewTmpActionList),
  extract_action_list(T, TmpActionList, ActionList).
extract_action_list([_ID-Action-_Achievers|T], TmpActionList, ActionList) :-
  ll_action(Action, _, _, _, _, _),
  format('Action: ~w~n', [Action]),
  mapping(Action, _),
  format(' has no mapping~n'),
  extract_action_list(T, NewTmpActionList, ActionList).
extract_action_list([_ID-Action-_Achievers|T], TmpActionList, ActionList) :-
  action(Action, _, _, _, _, _),
  format('Action: ~w~n', [Action]),
  mapping(Action, _),
  format(' has mapping~n'),
  extract_action_list(T, TmpActionList, ActionList).


extract_resources_number(Resources) :-
  findall(X, resources(X), AllResources),
  extract_resources_number(AllResources, [], Resources).

extract_resources_number([], Resources, Resources).
extract_resources_number([Resource|T], TmpResources, Resources) :- 
  findall(Resource, Resource, List),
  length(List, Len),
  functor(Resource, ResourceName, _),
  append([ResourceName-Len], TmpResources, NewTmpResources),
  extract_resources_number(T, NewTmpResources, Resources).

extract_resources_per_action(Actions, ActionXResources) :-
  extract_resources_per_action(Actions, [], ActionXResources).

extract_resources_per_action([], ActionXResources, ActionXResources, Resources).
extract_resources_per_action([Action|T], TmpActionXResources, ActionXResources, Resources) :-

  extract_resources_per_action(T, TmpActionXResources, ActionXResources).