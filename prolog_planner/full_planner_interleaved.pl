:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

extract_hl_goal(Goal, HLGoal) :-
  findall(Action, action(Action, _, _, _, _, _), ActionsList).
  extract_hl_goal(Actions, Goal, [], HLGoal).

extract_hl_goal([], _Goal, HLGoal, HLGoal).
extract_hl_goal([HAction|TActions], Goal, TempHLGoal, HLGoal) :-
  action(HAction, PreconditionsT, PreconditionsF, _FinalConditionsF, _Verify, Effects),
  check_goal_in_preconditions(Goal, PreconditionsT, RetPredicates),
  % check_goal_in_preconditions(PreconditionsF, Goal),
  % check_goal_in_effects(Effects, Goal)
  true.

check_goal_in_preconditions(Goal, Preconditions, RetPredicates) :-
  check_goal_in_preconditions(Goal, Preconditions, [], RetPredicates).

check_goal_in_preconditions([], _Preconditions, RetPredicates, RetPredicates).
check_goal_in_preconditions([HGoal|TGoals], Preconditions, Predicates, RetPredicates) :-
  (
    match_goal(HGoal, Preconditions)
    ->(
      append([HGoal], Predicates, NewPredicates),
      debug_format('Matched goal ~w in ~w\n', [HGoal, Preconditions])
    ); (
      append([], Predicates, NewPredicates),
      debug_format('Goal not matched ~w in ~w\n', [HGoal, Preconditions])
    )
  ),
  check_goal_in_preconditions(TGoals, Preconditions, NewPredicates, RetPredicates).

match_goal(Goal, []) :- false.
match_goal(Goal, [HPrecondition|TPreconditions]) :-
  functor(Goal, Name, Arity),
  (
    functor(HPrecondition, Name, Arity)
    -> true
    ;  match_goal(Goal, TPreconditions)
  ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function checks if an action is applicable, i.e., checks if the 
% preconditions are met and the final conditions are not met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
  verify(Verify),
  conditions_met(PreconditionsT, State),
  conditions_not_met(PreconditionsF, State),
  conditions_not_met(FinalConditionsF, State).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function is used to add the last achievers for end actions.
% For each end action, the start action is an achiever and if the action is not
% low-level, then all the lower-level action between the start action and the 
% end action are achievers of the end action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_achievers_end(_PrevActionName, [], LastAchievers, LastAchievers).
add_achievers_end(PrevActionName, [[_ID-HAction]|_TActions], LastAchievers, LastAchievers) :-
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName).
add_achievers_end(PrevActionName, [[ID-HAction]|TActions], LastAchievers, RetLastAchievers) :-
  functor(HAction, ActionName, _),
  \+sub_string(ActionName, _, _, _, PrevActionName),
  append([ID], LastAchievers, TempLastAchievers),
  add_achievers_end(PrevActionName, TActions, TempLastAchievers, RetLastAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As a first iteration, all the low-level actions that are not mapped to any 
% lower level action share the same resources and hence are achievers of the 
% following low-level action. This funciton adds all the previous low-level 
% actions as achievers of the current low-level action, up until the previous
% high-level action.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers) :-
  \+mapping(HAction, _),
  debug_format('No mapping for action ~w adding prev resource constraints ~w ~w~n~w\n', [HAction, Lenght, IDHLAction, Plan]),
  add_no_mapping_achievers_wrapped(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers).
add_no_mapping_achievers(_Lenght-HAction, _Plan, _IDHLAction, TempLastAchievers, TempLastAchievers) :-
  mapping(HAction, _),
  debug_format('Mapping for action ~w, not adding prev resource constraints\n', [HAction]),
  true.
add_no_mapping_achievers_wrapped(_PrevID-_PrevAction, [[IDHLAction-_]|_], IDHLAction, RetAchievers, RetAchievers) :-
  debug_format('New mappings ~w~n', [RetAchievers]),
  true.
add_no_mapping_achievers_wrapped(ID-Action, [[PrevID-PrevAction]|T], IDHLAction, TmpAchievers, RetAchievers) :-
  debug_format('Adding achievers for ~w ~w ~w ~w~n', [ID, Action, PrevID, PrevAction]),
  append([PrevID], TmpAchievers, NewTmpAchievers),
  debug_format('Added achievers for ~w ~w ~w ~w~n', [ID, Action, PrevID, PrevAction]),
  add_no_mapping_achievers_wrapped(ID-Action, T, IDHLAction, NewTmpAchievers, RetAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% add_achievers_test(Length-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers) :-
%   \+mapping(HAction, _),
%   add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers).

% add_achievers_test(Length-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers) :-
%   \+mapping(HAction, _),
%   add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_map([], _IDHLAction, _State, Plan, LastAchievers, Plan, LastAchievers, Pre) :-
  debug_format('~wExiting apply_map\n', [Pre]).

apply_map([HAction|TActions], IDHLAction, State, Plan, LastAchievers, RetPlan, RetLastAchievers, Pre) :-
  debug_format('~wAdding map ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('~wfound action ~w ~w ~w ~w ~w ~w ~n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  debug_format('~wstacking\n', [Pre]),
  length(Plan, Length),

  % Find last achievers
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, Achievers),
  (
    functor(HAction, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      debug_format('~wAdding achievers for ~w ~w ~w ~w~n', [Pre, HAction, ActionNameFull, ActionName, Value]),
        add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers)
    );(  
      append([IDHLAction], Achievers, TempLastAchievers)
    )
  ),
  % add_no_mapping_achievers(Length-HAction, Plan, IDHLAction, TempLastAchievers, TempTempLastAchievers),
  debug_format('~wLast achievers: ~w\n', [Pre, TempTempLastAchievers]),
  append([Length-HAction-TempTempLastAchievers], LastAchievers, NewLastAchievers),

  stack([Length-HAction], Plan, NewPlan),
  % Change state.
  change_state(State, Effects, NewState),
  debug_format('~wchanged to ~w~n', [Pre, NewState]),
  % trace(verify),trace(conditions_met),trace(conditions_not_met),trace(change_state),trace(is_applicable),trace(stack),trace(mapping),trace(apply_map),% trace,
  string_concat(Pre, '\t', NewPre),
  (
    mapping(HAction, Mappings)
    ->(
      debug_format('~wFound mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
      append(Mappings, TActions, NewActionList),
      apply_map(NewActionList, Length, NewState, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers, NewPre)
    )
    ; (
      debug_format('~wNo mappings for action ~w\n', [Pre, HAction]),
      apply_map(TActions, IDHLAction, NewState, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers, Pre)
    )
  ),
  true
  .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function generates a plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plan(Init, Goal, Plan, LastAchievers) :-
  debug_format('Checking if the initial state is the goal state ~w~w\n', [Init, Goal]),
  \+equal_set(Init, Goal),
  debug_format('Generating plan from ~w to ~w\n', [Init, Goal]),
  generate_plan(Init, Goal, [], [], [], 61, Plan, TmpAchievers),
  debug_format('Plan generated\n'),
  clean_achievers(TmpAchievers, LastAchievers).

generate_plan(State, Goal, _Been_list, Plan, LastAchievers, _MaxDepth, Plan, LastAchievers) :-
  debug_format('Check if state is goal state ~w ~w\n', [State, Goal]),
  equal_set(State, Goal),
  debug_format('They are\n\n\n\n\n\n').

generate_plan(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
  \+equal_set(State, Goal),
  length(Plan, Length), 
  (
    Length < MaxDepth ->
    (
      format('Length is fine ~w\n', [Length])
    );(
      format('Length is too long ~w\n', [Length]),
      fail
    )
  ),
  % Check new action
  action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('\n\nChecking action ~w for state: ~w\n', [Name, State]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  debug_format('Action ~w is applicable for state ~w\n', [Name, State]),
  change_state(State, Effects, NewState),
  (
    (
      \+equal_set(NewState, Goal),
      \+member_state(NewState, Been_list)
    );
    equal_set(NewState, Goal)
  ),
  debug_format('Obtained new state ~w\n', [NewState]),

  % Find last achievers
  debug_format('Finding last achievers for ~w ~w ~w ~w\n', [Name, PreconditionsT, PreconditionsF, Plan]),
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, Achievers),
  (
    functor(Name, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      debug_format('Adding achievers for ~w ~w ~w ~w~n', [Name, ActionNameFull, ActionName, Value]),
      add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers)
    );(
      append([], Achievers, TempLastAchievers)
    )
  ),
  append([Length-Name-TempLastAchievers], LastAchievers, NewLastAchievers),
  debug_format('Last achievers: ~w\n', [TempLastAchievers]),

  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  debug_format('New state: ~w~n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  % Look for mappings and apply them
  debug_format('Looking for mappings~n'),
  (
    mapping(Name, Mappings) 
    ->  (
        debug_format('Applying mappings for action ~w\n', [Name]),
        apply_map(Mappings, Length, State, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers, '\t'),
        debug_format('Back to generating plan\n'),
        generate_plan(NewState, Goal, NewBeen_list, RetPlan, RetLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
    % Continue planning
    ;   (
        debug_format('No mappings for action ~w\n\n', [Name]),
        generate_plan(NewState, Goal, NewBeen_list, NewPlan, NewLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
  ),
  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function takes a list of achievers, removes the duplicates and reverses the list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_achievers(LastAchievers, RetLastAchievers) :-
  clean_achievers(LastAchievers, [], TmpLastAchievers),
  reverse(TmpLastAchievers, [], RetLastAchievers).

clean_achievers([], LastAchievers, LastAchievers).
clean_achievers([ID-Action-Achievers|TActions], TempLastAchievers, RetLastAchievers) :-
  move_to_set(Achievers, NewAchievers),
  debug_format('Cleaned achievers for ~w ~w ~w from ~w\n', [ID, Action, NewAchievers, Achievers]),
  append([ID-Action-NewAchievers], TempLastAchievers, NewLastAchievers),
  clean_achievers(TActions, NewLastAchievers, RetLastAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function removes duplicates from a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_to_set(Ach, Ret) :-
  move_to_set(Ach, [], TmpRet),
  reverse(TmpRet, Ret).
  
move_to_set([], R, R).
move_to_set([H|T], Temp, Ret) :-
  member(H, Temp),
  move_to_set(T, Temp, Ret).
move_to_set([H|T], Temp, Ret) :-
  \+member(H, Temp),
  append([H], Temp, NewTemp),
  move_to_set(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function reverses a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse([], Ret, Ret).
reverse([H|T], Temp, Ret) :-
  append([H], Temp, NewTemp),
  reverse(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%