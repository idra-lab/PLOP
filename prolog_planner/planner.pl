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
  
  extract_adj_matrix(LastAchieversReversed, AdjMatrix),
  print_list(AdjMatrix),
  nl,nl,nl,

  extract_action_list(LastAchieversReversed, ActionList),
  print_list(ActionList),
  true.

% Mind that the matrix so extracted is rotated of 180 degrees, it's jsut better 
% rotate it later in Python
extract_adj_matrix(LastAchievers, RetMatrix):-
  length(LastAchievers, PlanLength),
  extract_adj_matrix(LastAchievers, [], RetMatrix, PlanLength, 0).

extract_adj_matrix(_LastAchievers, RetMatrix, RetMatrix, PlanLength, PlanLength).
extract_adj_matrix(LastAchievers, TmpMatrix, RetMatrix, PlanLength, I):-
  extract_column_achievers(LastAchievers, I, [], Row),
  append([Row], TmpMatrix, NewTmpMatrix),
  NewI is I + 1,
  extract_adj_matrix(LastAchievers, NewTmpMatrix, RetMatrix, PlanLength, NewI).

extract_column_achievers([], I, RetRow, RetRow).
extract_column_achievers([_ID-_A-AAchievers|T], I, TmpRow, RetRow):-
  (
    member(I, AAchievers) 
    -> 
      append([1], TmpRow, NewTmpRow)
    ; 
      append([0], TmpRow, NewTmpRow)
  ),
  extract_column_achievers(T, I, NewTmpRow, RetRow).

% Extract the dictionary of actions
extract_action_list(LastAchievers, ActionList) :-
  extract_action_list(LastAchievers, ['0_s'], TmpActionList),
  append(['0_e'], TmpActionList, TmpTmpActionList),
  reverse(TmpTmpActionList, ActionList).

extract_action_list([], ActionList, ActionList).
extract_action_list([ID-Action-Achievers|T], TmpActionList, ActionList) :-
  % length(TmpActionList, Len),
  append([Action], TmpActionList, NewTmpActionList),
  extract_action_list(T, NewTmpActionList, ActionList).

