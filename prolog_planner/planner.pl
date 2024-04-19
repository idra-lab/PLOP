% :- ensure_loaded('ll_planner.pl').
% :- ensure_loaded('hl_planner.pl').
:- ensure_loaded('full_planner.pl').

generate_plan(Actions, AdjMatrix, TTActionList, ActionXResources) :-
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
  format('Adjacency matrix:~n'),
  print_list(AdjMatrix),
  format('Actions:~n'),
  print_list(Actions),
  nl,nl,nl,

  extract_tt_action_list(Actions, TTActionList),
  format('Time-triggered actions:~n'),
  print_list(TTActionList),
  nl,nl,nl,

  extract_resources_number(Resources),
  format('Resources:~n'),
  print_list(Resources),
  nl,nl,nl,

  extract_resources_per_action(TTActionList, Resources, ActionXResources),
  format('Resources per action:~n'),
  print_list(ActionXResources),
  nl,nl,nl,

  true.

% Function plan/0 to call plan
plan :-
  generate_plan(_, _, _, _).  

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
  length(Actions, PlanLength),
  FinalAction is PlanLength + 1,
  extract_tt_action_list(Actions, 1, [0-[0-init()]-[FinalAction-end()]], TmpTTActionList),
  reverse(TmpTTActionList, TTActionList).

start_action(Action, ActionName, Args) :-
  Action =.. [StartActionName | Args],
  % format('Start action: ~w ~w~n', [StartActionName, Args]),
  sub_string(StartActionName, Before, Length, After, '_start'),
  sub_string(StartActionName, 0, Before, Length, ActionName),
  % format('Action name ~w~n', [ActionName]),
  true.

end_action(Action, ActionName, Args) :-
  Action =.. [EndActionName | Args],
  % format('End action: ~w ~w~n', [EndActionName, Args]),
  sub_string(EndActionName, Before, Length, After, '_end'),
  sub_string(EndActionName, 0, Before, Length, ActionName),
  % format('Action name ~w~n', [ActionName]),
  true.

extract_tt_action_list([], _, TTActionList, TTActionList).
extract_tt_action_list([ID-Action|T], TT_ID, TmpTTActionList, TTActionList) :-
  % format('Action: ~w~n', [Action]),
  start_action(Action, ActionName, Args),
  find_tt_end_action(T, ActionName, Args, [EndID-EndAction]),
  % format('End action: ~w~n', [EndAction]),
  NewTT_ID is TT_ID + 1,
  append([TT_ID-[ID-Action]-[EndID-EndAction]], TmpTTActionList, NewTmpTTActionList),
  extract_tt_action_list(T, NewTT_ID, NewTmpTTActionList, TTActionList).
extract_tt_action_list([ID-Action|T], TT_ID, TmpTTActionList, TTActionList) :-
  % format('Action: ~w~n', [Action]),
  \+start_action(Action, ActionName, Args),
  extract_tt_action_list(T, TT_ID, TmpTTActionList, TTActionList).

find_tt_end_action([], _ActionName, _Args, _EndAction) :- 
  % format('THIS SHOULD NOT HAPPEN'), 
  fail.
find_tt_end_action([ID-EndAction|T], ActionName, Args, [ID-EndAction]) :-
  % format('Testing end action: ~w against ~w ~w ~n', [EndAction, ActionName, Args]),
  end_action(EndAction, ActionName, Args).
find_tt_end_action([_ID-EndAction|T], ActionName, Args, [RetID-RetEndAction]) :-
  % format('Testing end action: ~w against ~w ~w ~n', [EndAction, ActionName, Args]),
  \+end_action(EndAction, ActionName, Args),
  find_tt_end_action(T, ActionName, Args, [RetID-RetEndAction]).
  


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



extract_resources_per_action(TTActions, Resources, ActionXResources) :-
  [H|T] = TTActions,
  extract_resources_per_action(T, Resources, [], ActionXResources).

extract_resources_per_action([], _Resources, ActionXResources, ActionXResources).
extract_resources_per_action([TT_ID-StartAction-EndAction|T], Resources, TmpActionXResources, ActionXResources) :-
  check_resources_per_action(StartAction, EndAction, Resources, [], RetResources),
  append([TT_ID-RetResources], TmpActionXResources, NewActionXResources),
  extract_resources_per_action(T, Resources, NewActionXResources, ActionXResources).

check_resources_per_action(StartAction, EndAction, [], UsedResources, UsedResources).
check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resource_in_verify(Resource, []) :- fail.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  H =.. [Resource | _],
  true.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  \+ (H =.. [Resource | _]),
  check_resource_in_verify(Resource, T).

