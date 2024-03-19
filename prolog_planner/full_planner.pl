:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
apply_map([], State, Been_list, Plan, State, Been_list, Plan).
apply_map([HAction|TActions], State, Been_list, Plan, RetState, RetBeen_list, RetPlan) :-
  format('\tAdding map ~w ~w\n', [HAction, State]),
  functor(HAction, ActionName, _),
  % format('\tChecking action ~w\n', [ActionName]),
  % sub_string(ActionName, _, _, _, 'move_arch_start')
  %   ->  true 
  %       , trace(verify),trace(conditions_met),trace(conditions_not_met),trace(change_state),trace(is_applicable),trace(stack),trace(mapping),trace(apply_map)
  %       % , leash(-all), trace
  %   ;   true,
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  write('\tfound action'), nl,
  
  % write('testing applicable\n'),

  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
  write('\tstacking\n'),
  stack(HAction, Plan, NewPlan),
  % Change state.
  write('\tchanging state\n'),
  change_state(State, Effects, NewState),
  format('\tchanged to ~w~n', [NewState]),
  % trace(verify),trace(conditions_met),trace(conditions_not_met),trace(change_state),trace(is_applicable),trace(stack),trace(mapping),trace(apply_map),% trace,
  (
    \+member_state(NewState, Been_list) 
    ->  stack(NewState, Been_list, NewBeen_list),
        (
          mapping(HAction, Mappings)
          ->  format('\t1 Found mapping for action ~w ~w\n', [HAction, Mappings]), 
              append(Mappings, TActions, NewActionList),
              apply_map(NewActionList, NewState, NewBeen_list, NewPlan, RetState, RetBeen_list, RetPlan)
          ;   format('\t1 No mappings for action ~w\n', [HAction]),
              apply_map(TActions, NewState, NewBeen_list, NewPlan, RetState, RetBeen_list, RetPlan)
        )
    ;   (
          % Even though the new state has already been visited, we still need to change the current state
          % to that state to continue to add other actions correctly
          mapping(HAction, Mappings)
          ->  format('\t2 Found mapping for action ~w ~w\n', [HAction, Mappings]),
              append(Mappings, TActions, NewActionList),
              apply_map(NewActionList, NewState, Been_list, Plan, RetState, RetBeen_list, RetPlan)
          ;   format('\t2 No mappings for action ~w\n', [HAction]),
              apply_map(TActions, NewState, Been_list, Plan, RetState, RetBeen_list, RetPlan)
        )
  ).

is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
  verify(Verify),
  conditions_met(PreconditionsT, State),
  conditions_not_met(PreconditionsF, State),
  conditions_not_met(FinalConditionsF, State).

plan(State, Goal, _Been_list, Plan, _MaxDepth, Plan) :-
  equal_set(State, Goal).

plan(State, Goal, Been_list, Plan, MaxDepth, FinalPlan) :-
  \+equal_set(State, Goal),
  length(Plan, Depth), Depth < MaxDepth,
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
  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  format('New state: ~w~n', [NewState]),
  stack(Name, Plan, NewPlan),
  % Look for mappings and apply them
  (
    mapping(Name, Mappings) 
    -> format('Applying mappings for action ~w\n', [Name]),
       apply_map(Mappings, NewState, NewBeen_list, NewPlan, RetState, RetBeen_list, RetPlan),
       plan(RetState, Goal, RetBeen_list, RetPlan, MaxDepth, FinalPlan)
       % Continue planning
    ;  format('No mappings for action ~w\n', [Name]),
       plan(NewState, Goal, NewBeen_list, NewPlan, MaxDepth, FinalPlan)
  ).

plan(Init, Goal, Plan) :-
  \+equal_set(Init, Goal),
  plan(Init, Goal, [], [], 30, Plan).
