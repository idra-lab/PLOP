:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
apply_map([], State, Been_list, Plan, LastAchievers, State, Been_list, Plan, LastAchievers, _).
apply_map([HAction|TActions], State, Been_list, Plan, LastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre) :-
  format('~wAdding map ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  format('~wfound action ~w ~w ~w ~w ~w ~w ~n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  format('~wstacking\n', [Pre]),
  length(Plan, Length),

  % Find last achievers
  last_achievers_ids(PreconditionsT, Plan, Achievers),
  append([Length-HAction-Achievers], LastAchievers, NewLastAchievers),

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
          apply_map(NewActionList, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, NewPre)
        )
        ; (
          format('~w1 No mappings for action ~w\n', [Pre, HAction]),
          apply_map(TActions, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre)
        )
      )
    )
    ; (
      % Even though the new state has already been visited, we still need to change the current state
      % to that state to continue to add other actions correctly
      mapping(HAction, Mappings)
      ->(
        format('~w2 Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
        append(Mappings, TActions, NewActionList),
        apply_map(NewActionList, NewState, Been_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, NewPre)
      )
      ; (
        format('~w2 No mappings for action ~w\n', [Pre, HAction]),
        apply_map(TActions, NewState, Been_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, Pre)
      )
    )
  ).

is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
  verify(Verify),
  conditions_met(PreconditionsT, State),
  conditions_not_met(PreconditionsF, State),
  conditions_not_met(FinalConditionsF, State).

plan(State, Goal, _Been_list, Plan, LastAchievers, _MaxDepth, Plan, LastAchievers) :-
  equal_set(State, Goal).

plan(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
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
  last_achievers_ids(PreconditionsT, Plan, Achievers),
  append([Length-Name-Achievers], LastAchievers, NewLastAchievers),

  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  format('New state: ~w~n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  % Look for mappings and apply them
  write('Using mappings\n'),
  (
    mapping(Name, Mappings) 
    ->  (
        format('Applying mappings for action ~w\n', [Name]),
        apply_map(Mappings, NewState, NewBeen_list, NewPlan, NewLastAchievers, RetState, RetBeen_list, RetPlan, RetLastAchievers, '\t'),
        plan(RetState, Goal, RetBeen_list, RetPlan, RetLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
    % Continue planning
    ;   (
        format('No mappings for action ~w\n', [Name]),
        plan(NewState, Goal, NewBeen_list, NewPlan, NewLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers)
    )
  )
  % plan(NewState, Goal, NewBeen_list, NewPlan, MaxDepth, FinalPlan)
  .

plan(Init, Goal, Plan, LastAchievers) :-
  \+equal_set(Init, Goal),
  plan(Init, Goal, [], [], [], 50, Plan, LastAchievers).
