:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

add_achievers_end(_PrevActionName, [], LastAchievers, LastAchievers).
add_achievers_end(PrevActionName, [[_ID-HAction]|_TActions], LastAchievers, LastAchievers) :-
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName).
add_achievers_end(PrevActionName, [[ID-HAction]|TActions], LastAchievers, RetLastAchievers) :-
  functor(HAction, ActionName, _),
  \+sub_string(ActionName, _, _, _, PrevActionName),
  append([ID], LastAchievers, TempLastAchievers),
  add_achievers_end(PrevActionName, TActions, TempLastAchievers, RetLastAchievers).

add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers) :-
  \+mapping(HAction, _),
  format('No mapping for action ~w adding prev resource constraints ~w ~w~n~w\n', [HAction, Lenght, IDHLAction, Plan]),
  add_no_mapping_achievers_wrapped(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers).

add_no_mapping_achievers(_Lenght-HAction, _Plan, _IDHLAction, TempLastAchievers, TempLastAchievers) :-
  mapping(HAction, _),
  format('Mapping for action ~w, not adding prev resource constraints\n', [HAction]),
  true.

add_no_mapping_achievers_wrapped1(_PrevID-_PrevAction, [], _IDHLAction, RetAchievers, RetAchievers).
add_no_mapping_achievers_wrapped1(This, [H|T], IDHLAction, TmpAchievers, RetAchievers) :-
  format('TESTTEST achievers for ~w ~w ~w ~w ~w ~w~n', [This, H, T, IDHLAction, TmpAchievers, RetAchievers]),
  add_no_mapping_achievers_wrapped1(This, T, IDHLAction, TmpAchievers, RetAchievers).

add_no_mapping_achievers_wrapped(_PrevID-_PrevAction, [[IDHLAction-_]|_], IDHLAction, RetAchievers, RetAchievers) :-
  format('New mappings ~w~n', [RetAchievers]),
  true.
add_no_mapping_achievers_wrapped(ID-Action, [[PrevID-PrevAction]|T], IDHLAction, TmpAchievers, RetAchievers) :-
  format('Adding achievers for ~w ~w ~w ~w~n', [ID, Action, PrevID, PrevAction]),
  append([PrevID], TmpAchievers, NewTmpAchievers),
  format('Added achievers for ~w ~w ~w ~w~n', [ID, Action, PrevID, PrevAction]),
  add_no_mapping_achievers_wrapped(ID-Action, T, IDHLAction, NewTmpAchievers, RetAchievers).

% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
apply_map([], _IDHLAction, State, Been_list, Plan, LastAchievers, State, Been_list, Plan, LastAchievers, _).
apply_map([HAction|TActions], IDHLAction, State, Been_list, Plan, LastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre) :-
  format('~wAdding map ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  format('~wfound action ~w ~w ~w ~w ~w ~w ~n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  format('~wstacking\n', [Pre]),
  length(Plan, Length),

  % Find last achievers
  last_achievers_ids(PreconditionsT, PreconditionsF, Plan, Achievers),
  (
    functor(HAction, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      format('Adding achievers for ~w ~w ~w ~w~n', [HAction, ActionNameFull, ActionName, Value]),
        add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers)
    );(  
      append([IDHLAction], Achievers, TempLastAchievers)
    )
  ),
  add_no_mapping_achievers(Length-HAction, Plan, IDHLAction, TempLastAchievers, TempTempLastAchievers),

  append([Length-HAction-TempTempLastAchievers], LastAchievers, NewLastAchievers),

  stack([Length-HAction], Plan, NewPlan),
  % Change state.
  format('~wchanging state\n', [Pre]),
  change_state(State, Effects, NewState),
  format('~wchanged to ~w~n', [Pre, NewState]),
  % trace(verify),trace(conditions_met),trace(conditions_not_met),trace(change_state),trace(is_applicable),trace(stack),trace(mapping),trace(apply_map),% trace,
  string_concat(Pre, '\t', NewPre),
  (
    \+member_state(NewState, Been_list) 
    ->( 
      stack(NewState, Been_list, NewBeen_list),
      (
        mapping(HAction, Mappings)
        ->(
          format('~w1 Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]), 
          append(Mappings, TActions, NewActionList),
          apply_map(NewActionList, Length, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, NewPre)
        )
        ; (
          format('~w1 No mappings for action ~w\n', [Pre, HAction]),
          apply_map(TActions, IDHLAction, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre)
        )
      )
    );(
      % Even though the new state has already been visited, we still need to change the current state
      % to that state to continue to add other actions correctly
      mapping(HAction, Mappings)
      ->(
        format('~w2 Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
        append(Mappings, TActions, NewActionList),
        apply_map(NewActionList, Length, NewState, Been_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, NewPre)
      )
      ; (
        format('~w2 No mappings for action ~w\n', [Pre, HAction]),
        apply_map(TActions, IDHLAction, NewState, Been_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre)
      )
    )
  ),
  true
  .

is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
  verify(Verify),
  conditions_met(PreconditionsT, State),
  conditions_not_met(PreconditionsF, State),
  conditions_not_met(FinalConditionsF, State).

generate_plan(State, Goal, _Been_list, Plan, LastAchievers, _MaxDepth, Plan, LastAchievers) :-
  equal_set(State, Goal).
generate_plan(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
  % trace(verify),trace(conditions_met),trace(conditions_not_met),trace(change_state),trace(is_applicable),trace(stack),trace(mapping),trace(apply_map),% trace,
  \+equal_set(State, Goal),
  length(Plan, Length), Length < MaxDepth,
  % Check new action
  action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  format('Checking action ~w for state: ~w\n', [Name, State]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  change_state(State, Effects, NewState),
  (
    (
      \+equal_set(NewState, Goal),
      \+member_state(NewState, Been_list)
    );
    equal_set(NewState, Goal)
  ),

  % Find last achievers
  last_achievers_ids(PreconditionsT, PreconditionsF, Plan, Achievers),
  format('Last achievers: ~w\n', [Achievers]),
  (
    functor(Name, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      format('Adding achievers for ~w ~w ~w ~w~n', [Name, ActionNameFull, ActionName, Value]),
      add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers)
    );(
      append([], Achievers, TempLastAchievers)
    )
  ),
  append([Length-Name-TempLastAchievers], LastAchievers, NewLastAchievers),

  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  format('New state: ~w~n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  % Look for mappings and apply them
  format('Using mappings~n'),
  (
    mapping(Name, Mappings) 
    ->  (
        format('Applying mappings for action ~w\n', [Name]),
        apply_map(Mappings, Length, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, '\t'),
        generate_plan(RetState, Goal, RetBeen_list, RetPlan, RetLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
    % Continue planning
    ;   (
        format('No mappings for action ~w\n', [Name]),
        generate_plan(NewState, Goal, NewBeen_list, NewPlan, NewLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
  )
  % generate_plan(NewState, Goal, NewBeen_list, NewPlan, MaxDepth, FinalPlan)
  .

generate_plan(Init, Goal, Plan, LastAchievers) :-
  \+equal_set(Init, Goal),
  generate_plan(Init, Goal, [], [], [], 50, Plan, TmpAchievers),
  clean_achievers(TmpAchievers, LastAchievers).

move_to_set([], R, R).
move_to_set([H|T], Temp, Ret) :-
  member(H, Temp),
  move_to_set(T, Temp, Ret).
move_to_set([H|T], Temp, Ret) :-
  \+member(H, Temp),
  append([H], Temp, NewTemp),
  move_to_set(T, NewTemp, Ret).

move_to_set(Ach, Ret) :-
  move_to_set(Ach, [], TmpRet),
  reverse(TmpRet, Ret).

clean_achievers([], LastAchievers, LastAchievers).
clean_achievers([ID-Action-Achievers|TActions], TempLastAchievers, RetLastAchievers) :-
  move_to_set(Achievers, NewAchievers),
  format('Cleaned achievers for ~w ~w ~w from ~w\n', [ID, Action, NewAchievers, Achievers]),
  append([ID-Action-NewAchievers], TempLastAchievers, NewLastAchievers),
  clean_achievers(TActions, NewLastAchievers, RetLastAchievers).

clean_achievers(LastAchievers, RetLastAchievers) :-
  clean_achievers(LastAchievers, [], TmpLastAchievers),
  reverse(TmpLastAchievers, [], RetLastAchievers).

reverse([], Ret, Ret).
reverse([H|T], Temp, Ret) :-
  append([H], Temp, NewTemp),
  reverse(T, NewTemp, Ret).