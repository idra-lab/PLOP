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
% TODO I should also check that the actions have the same arguments here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_achievers_end(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end(PrevActionName, [[_ID-HAction]|_TActions], LastAchievers, LastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName).
add_achievers_end(PrevActionName, [[ID-HAction]|TActions], LastAchievers, RetLastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  \+sub_string(ActionName, _, _, _, PrevActionName),
  append([ID], LastAchievers, TempLastAchievers),
  add_achievers_end(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).

add_achievers_end_ll(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end_ll(PrevActionName, [_ID-HAction|_TActions], LastAchievers, LastAchievers, Pre) :-
  debug_format('~w[add_achievers_end] Is ~w the start action ~w_start\n', [Pre, HAction, PrevActionName]),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName),
  sub_string(ActionName, _, _, _, '_start'),
  debug_format('~w[add_achievers_end] Found start action ~w for action ~w\n', [Pre, ActionName, PrevActionName]),
  true.
add_achievers_end_ll(PrevActionName, [ID-HAction|TActions], LastAchievers, RetLastAchievers, Pre) :-
  debug_format('~w[add_achievers_end] ~w is not the start action of ~w_end\n', [Pre, HAction, PrevActionName]),
  % functor(HAction, ActionName, _),
  % \+((
  %   sub_string(ActionName, _, _, _, PrevActionName),
  %   sub_string(ActionName, _, _, _, '_start')
  % )),
  append([ID], LastAchievers, TempLastAchievers),
  debug_format('~w[add_achievers_end] Adding achiever ~w for action ~w\n', [Pre, HAction, ID]),
  debug_format('~w[add_achievers_end] to ~w\n', [Pre, LastAchievers]),
  debug_format('~w[add_achievers_end] obtaining ~w\n', [Pre, TempLastAchievers]),
  add_achievers_end_ll(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As a first iteration, all the low-level actions that are not mapped to any 
% lower level action share the same resources and hence are achievers of the 
% following low-level actions in the same mapping. This function adds all the
% previous low-level actions as achievers of the current low-level action, up 
% until the previous high-level action.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre) :-
  \+mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] No mapping for action ~w adding prev resource constraints\n', [Pre, HAction]),
  debug_format('~w[add_no_mapping_achievers] Length: ~w\n', [Pre, Lenght]),
  debug_format('~w[add_no_mapping_achievers] IDHLAction: ~w\n', [Pre, IDHLAction]),
  debug_format('~w[add_no_mapping_achievers] Plan: ~w\n', [Pre, Plan]),
  debug_format('~w[add_no_mapping_achievers] HAction: ~w\n', [Pre, HAction]),
  add_no_mapping_achievers_wrapped(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre),
  debug_format('~w[add_no_mapping_achievers] New achievers ~w\n', [Pre, RetLastAchievers]).

add_no_mapping_achievers(_Lenght-HAction, _Plan, _IDHLAction, TempLastAchievers, TempLastAchievers, Pre) :-
  mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] Mapping for action ~w, not adding prev resource constraints\n', [Pre, HAction]),
  true.
add_no_mapping_achievers_wrapped(_ID-_Action, [IDHLAction-_PrevAction|_], IDHLAction, RetAchievers, RetAchievers, Pre) :-
  debug_format('~w[add_no_mapping_achievers_wrapped single] New mappings ~w\n', [Pre, RetAchievers]),
  true.
add_no_mapping_achievers_wrapped(ID-Action, [PrevID-PrevAction|T], IDHLAction, TmpAchievers, RetAchievers, Pre) :-
  IDHLAction \= PrevID,
  append([PrevID], TmpAchievers, NewTmpAchievers),
  debug_format('~w[add_no_mapping_achievers_wrapped single] Adding achiever ~w ~w for action ~w ~w, achievers: ~w\n', [Pre, PrevID, PrevAction, ID, Action, NewTmpAchievers]),
  add_no_mapping_achievers_wrapped(ID-Action, T, IDHLAction, NewTmpAchievers, RetAchievers, Pre).
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
apply_mappings(Init, Goal, HL_Plan, HL_Achievers, LL_Plan, LL_Achievers) :-
  debug_format('[apply_mappings] The HL plan is ~w\n', [HL_Plan]),
  apply_mappings(Init, Goal, HL_Plan, HL_Achievers, [], [], LL_Plan, LL_Achievers).

apply_mappings(Init, Goal, _HL_Plan, _HL_Achievers, LL_Plan, LL_Achievers, LL_Plan, LL_Achievers) :-
  equal_set(Init, Goal),
  % debug_format('[apply_mappings] Reached this point ~w\n', [LL_Plan]),
  true.

apply_mappings(Init, Goal, [[IDHLAction-HL_Action]|T_HL_Actions], [IDHLAction-HL_Action-HL_Achievers|T_HL_Achievers], Plan, LastAchievers, RetPlan, RetLastAchievers) :-
  debug_format('\n\n[apply_mappings] HL_Achievers:\n'), 
  print_list([HL_Achievers]),
  length(Plan, Length),
  action(HL_Action, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
  append([Length-HL_Action], Plan, TempPlan),
  change_state(Init, Effects, CurrentState),
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, TempActionLastAchievers),
  (
    mapping(HL_Action, Mappings) 
    ->(
      append([Length-HL_Action-TempActionLastAchievers], LastAchievers, TempLastAchievers),
      debug_format('[apply_mappings] Found mapping for action ~w ~w ~w\n', [HL_Action, Mappings, Length]),
      debug_format('[apply_mappings] Calling apply_action_map with'),
      debug_format('[apply_mappings] Mappings: ~w\n', [Mappings]), 
      debug_format('[apply_mappings] Length: ~w\n', [Length]), 
      debug_format('[apply_mappings] CurrentState: ~w\n', [CurrentState]), 
      debug_format('[apply_mappings] TempPlan: ~w\n', [TempPlan]),
      apply_action_map(Mappings, Length, CurrentState, TempPlan, TempLastAchievers, NewPlan, NewLastAchievers, '\t'),
      debug_format('[apply_mappings] NewLastAchievers: ~w\n', [NewLastAchievers])
    );(
      NewPlan = TempPlan,
      functor(HL_Action, ActionNameFull, _), 
      sub_string(ActionNameFull, Value, _, _, '_end'), 
      sub_string(ActionNameFull, _, Value, _, ActionName)
      ->(
        debug_format('[apply_action_map] Calling add_achievers_end_ll for ~w ~w\n', [ActionName, TempLastAchievers]),
        debug_format('[apply_mappings] TempPlan: \n'),
        print_list(TempPlan),
        % leash(-all),trace,
        add_achievers_end_ll(ActionName, TempPlan, TempActionLastAchievers, NewActionLastAchievers, '\t'),
        % append(TempTempLastAchievers, TempLastAchievers, NewLastAchievers),
        debug_format('[apply_mappings] NewActionLastAchievers: ~w\n', [NewActionLastAchievers]),
        append([Length-HL_Action-NewActionLastAchievers], LastAchievers, NewLastAchievers),
        true
      );(
        append([Length-HL_Action-TempActionLastAchievers], LastAchievers, NewLastAchievers),
        true
      )
    )
  ),
  debug_format('[apply_mappings] Action name: ~w\n', [HL_Action]),
  debug_format('[apply_mappings] Current state: ~w\n', [CurrentState]),
  debug_format('[apply_mappings] NewPlan: ~w\n', [NewPlan]),
  apply_mappings(CurrentState, Goal, T_HL_Actions, T_HL_Achievers, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_action_map([], _IDHLAction, _State, Plan, LastAchievers, Plan, LastAchievers, _).
apply_action_map([HAction|TActions], IDHLAction, State, Plan, LastAchievers, RetPlan, RetLastAchievers, Pre) :-
  debug_format('\n~w[apply_action_map] Adding map ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('~w[apply_action_map] found action ~w ~w ~w ~w ~w ~w \n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  debug_format('~w[apply_action_map] applicable ~w\n', [Pre, HAction]),
  length(Plan, Length),

  % Find last achievers
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, Achievers),
  append([IDHLAction], Achievers, TempLastAchievers),
  debug_format('~w[apply_action_map] Achievers ~w\n', [Pre, TempLastAchievers]),
  (
    functor(HAction, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      debug_format('~w[apply_action_map] Calling add_achievers_end_ll for ~w ~w ~w\n', [Pre, ActionName, Plan, TempLastAchievers]),
      % Create NewPre by concatenating Pre and a tab
      string_concat(Pre, '\t', NewPre),
      add_achievers_end_ll(ActionName, Plan, TempLastAchievers, TempTempLastAchievers, NewPre),
      true
    );( 
      append([], TempLastAchievers, TempTempLastAchievers)  
    )
  ),
  debug_format('~w[apply_action_map] TempLastAchievers: ~w\n', [Pre, TempLastAchievers]),
  % NewPre is the concatenation of Pre and a tab
  string_concat(Pre, '\t', NewPre),
  add_no_mapping_achievers(Length-HAction, Plan, IDHLAction, TempTempLastAchievers, TempTempTempLastAchievers, NewPre),
  % TempTempTempLastAchievers = TempLastAchievers,
  debug_format('~w[apply_action_map] Last achievers: ~w\n', [Pre, TempTempTempLastAchievers]),
  append([Length-HAction-TempTempTempLastAchievers], LastAchievers, NewLastAchievers),

  stack(Length-HAction, Plan, NewPlan),
  debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
  % Change state.
  change_state(State, Effects, NewState),
  debug_format('~w[apply_action_map] changed to ~w\n', [Pre, NewState]),
  string_concat(Pre, '\t', NewPre),
  (
    mapping(HAction, Mappings)
    ->(
      debug_format('~w[apply_action_map] Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
      append(Mappings, TActions, NewActionList),
      apply_action_map(NewActionList, Length, NewState, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers, NewPre)
    )
    ; (
      debug_format('~w[apply_action_map] No mappings for action ~w\n', [Pre, HAction]),
      debug_format('~w[apply_action_map] Applying next action\n', [Pre]), 
      debug_format('~w[apply_action_map] TActions: ~w\n', [Pre, TActions]),
      debug_format('~w[apply_action_map] IDHLAction: ~w\n', [Pre, IDHLAction]),
      debug_format('~w[apply_action_map] NewState: ~w\n', [Pre, NewState]),
      debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
      debug_format('~w[apply_action_map] NewLastAchievers: ~w\n', [Pre, NewLastAchievers]),
      apply_action_map(TActions, IDHLAction, NewState, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers, Pre)
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
  debug_format('Generating the high-level temporal plan from ~w to ~w\n', [Init, Goal]),
  generate_plan_hl(Init, Goal, [], [], [], 50, HL_Plan, HL_Achievers),
  % Print information on the high-level part
  debug_format('High-level plan generated\n~w\n', [HL_Plan]),
  debug_format('High-level achievers found\n'),
  print_list(HL_Achievers),
  extract_adj_matrix_actions(HL_Achievers, AdjMatrix, Actions),
  debug_format('Adjacency matrix:\n'),
  print_list(AdjMatrix),
  print_list(Actions),
  % Find the low-level plan
  debug_format('Applying the mappings to obtain the low-level temporal plan from ~w to ~w\n', [Init, Goal]),
  findall(Mapping, mapping(Mapping, _), Mappings),
  debug_format('Mappings available: ~w\n', [Mappings]),
  reverse(HL_Plan, HL_PlanReversed),
  reverse(HL_Achievers, HL_AchieversReversed),
  apply_mappings(Init, Goal, HL_PlanReversed, HL_AchieversReversed, Plan, LL_Achievers),
  debug_format('Plan generated~w\n', [Plan]),
  debug_format('LL achievers:\n'),
  print_list(LL_Achievers),
  clean_achievers(LL_Achievers, LastAchievers),
  true.

generate_plan_hl(State, Goal, _Been_list, Plan, LastAchievers, _MaxDepth, Plan, LastAchievers) :-
  equal_set(State, Goal).
generate_plan_hl(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
  \+equal_set(State, Goal),
  length(Plan, Length), Length < MaxDepth,
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
      debug_format('Adding achievers for ~w ~w ~w ~w\n', [Name, ActionNameFull, ActionName, Value]),
      add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers, '\t')
    );(
      append([], Achievers, TempLastAchievers)
    )
  ),
  append([Length-Name-TempLastAchievers], LastAchievers, NewLastAchievers),
  debug_format('Last achievers: ~w\n', [TempLastAchievers]),

  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  debug_format('New state: ~w\n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  generate_plan_hl(NewState, Goal, NewBeen_list, NewPlan, NewLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers),
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