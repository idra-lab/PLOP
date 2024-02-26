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

extract_mapping(Action, TotalOrder, TotalOrder) :-
    functor(Action, ActionName, _),
    \+sub_string(ActionName, _, _, _, '_start').

extract_mapping(Action, TotalOrder, NewTotalOrder) :-
    mappings(Action, LLActions),
    print_list(LLActions, "\t"),
    append(TotalOrder, LLActions, NewTotalOrder).


total_ll_order([], TotalOrder, _I, TotalOrder).
total_ll_order([HA|TA], TotalOrder, I, NewT) :-
    format('[~w] ~w~n', [I, HA]),
    NewI is I + 1,
    append(TotalOrder, [HA], NewTotalOrder),
    extract_mapping(HA, NewTotalOrder, NewNewTotalOrder),
    total_ll_order(TA, NewNewTotalOrder, NewI, NewT),
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
    ll_goal(Goal),
    \+equal_set(State, Goal),
    fail.
partial_ll_order([], State, CheckedActions, CheckedActions, ActionsAchievers, ActionsAchievers) :-
    format('Final state is: ~w~n', [State]), 
    format('Achievers are ~w~n', [ActionsAchievers]).

partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    action(ActionName, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
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

partial_ll_order([HAName|TANames], HLState, LLState, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    action(ActionName, _PreconditionsT, _PreconditionsF, __FinalConditionsF, _Verify, _Effects),
    \+functor(ActionName, HAName, _),
    partial_ll_order(TANames, State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers).

partial_ll_order([HAName|TANames], HLState, LLState, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    ll_action(ActionName, _PreconditionsT, _PreconditionsF, __FinalConditionsF, _Verify, _Effects),
    \+functor(ActionName, HAName, _),
    partial_ll_order(TANames, State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers).


% So basically to check the partial order we need to check the type of action we
% are dealing with, and then divide the function in two. If it is a high-level
% action we need to set that as a check-point and then check all the low-level 
% that follow keeping in mind that they cannot be executed before the high-level
% action. 
% To set a partial order between low-level actions belonging to different 
% high-level actions, we can produce a high-level partial order and then use it
% as the actual checkpoint. BUT the partial-order only gives us a logical 
% precedence between actions, so it may not be sufficient. I'm still thinking 
% about a graph... Each node would have as connected nodes, those actions onto 
% which it depends. 