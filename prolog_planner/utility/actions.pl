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
  extract_tt_action_list(Actions, 1, [0-[0-init_s()]-[FinalAction-end_e()]], TmpTTActionList),
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
  
