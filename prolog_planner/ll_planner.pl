:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('actions.pl').
:- ensure_loaded('kb.pl').
:- ensure_loaded('mappings.pl').
:- ensure_loaded('tests.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              TOTAL LL ORDER                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This functions allows to take a high-level plan (total order) and extract 
% the corresponding low-level actions from the mappings providing a simple
% 1 to 1 mapping.

extract_mapping(Action, Total_order, Total_order) :-
    functor(Action, ActionName, _),
    \+sub_string(ActionName, _, _, _, '_start').

extract_mapping(Action, Total_order, NewTotal_order) :-
    mappings(Action, LLActions),
    print_list(LLActions, "\t"),
    append(Total_order, LLActions, NewTotal_order).


total_ll_order([], Total_order, _I, Total_order).
total_ll_order([HA|TA], Total_order, I, NewT) :-
    format('[~w] ~w~n', [I, HA]),
    NewI is I + 1,
    append(Total_order, [HA], NewTotal_order),
    extract_mapping(HA, NewTotal_order, NewNewTotal_order),
    total_ll_order(TA, NewNewTotal_order, NewI, NewT),
    true.

% TODO check if possible not to reverse the list
total_ll_order(Plan, NewT):- 
    reverse_list(Plan, ReversedPlan),
    total_ll_order(ReversedPlan, [], 0, NewT), 
    write('Total LL order:'), nl,
    print_list(NewT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             PARTIAL LL ORDER                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partial_ll_order(ActionNames, InitState, Actions, ActionsAchievers) :-
    partial_ll_order(ActionNames, InitState, [], Actions, [], ActionsAchievers).

partial_ll_order([], State, CheckedActions, CheckedActions, ActionsAchievers, ActionsAchievers) :-
    format('Final state is: ~w~n', [State]), 
    format('Achievers are ~w~n', [ActionsAchievers]).

partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    action(ActionName, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
    functor(ActionName, HAName, _Names),
    verify(Verify),
    conditions_met(PreconditionsT, State),
    conditions_not_met(PreconditionsF, State),
    change_state(State, Effects, NewState),
    format('Last achievers for ~w:~n', [ActionName]),
    last_achievers(PreconditionsT, CheckedActions, ActionAchievers),
    print_list(ActionAchievers, '||'),
    append(ActionsAchievers, [ActionAchievers], NewActionsAchievers),
    partial_ll_order(TANames, NewState, [ActionName|CheckedActions], FinalActions, NewActionsAchievers, FinalActionsAchievers).

% partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
%     format('Testing ~w~n', [HAName]),
%     ll_action(ActionName, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
%     functor(ActionName, HAName, _Names),
%     format('Found ~w~n', [ActionName]),
%     verify(Verify),
%     conditions_met(PreconditionsT, State),
%     conditions_not_met(PreconditionsF, State),
%     change_state(State, Effects, NewState),
%     format('Last achievers for ~w:~n', [ActionName]),
%     last_achievers(PreconditionsT, CheckedActions, ActionAchievers),
%     print_list(ActionAchievers, '||'),
%     append(ActionsAchievers, [ActionAchievers], NewActionsAchievers),
%     partial_ll_order(TANames, NewState, [ActionName|CheckedActions], FinalActions, NewActionsAchievers, FinalActionsAchievers).

partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    action(ActionName, _PreconditionsT, _PreconditionsF, __FinalConditionsF, _Verify, _Effects),
    \+functor(ActionName, HAName, _),
    partial_ll_order(TANames, State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers).

partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    ll_action(ActionName, _PreconditionsT, _PreconditionsF, __FinalConditionsF, _Verify, _Effects),
    \+functor(ActionName, HAName, _),
    partial_ll_order(TANames, State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers).

test(Actions, ActionsAchievers) :- total_ll_order(ActionNames), partial_ll_order(ActionNames, [available(r1), at(r1, 1, 1, 0)], Actions, ActionsAchievers).
testNoTrace :- test(_A, _T). 
testTrace :- leash(-all), trace, testNoTrace. 

testSmallTrace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(partial_order, all),
	trace(achiever, all),
	trace(plan, all),
	trace(stack, all),
	trace(testPlan, all),
	trace(stack, all),
	test(_A, _T).