:- include('actions.pl').
:- include('kb.pl').
% :- include('tests.pl').
:- include('adts.pl').


printL([], _I, _S).
printL([H|T], I, S) :- 
    format('~w[~w] ~w~n', [S, I, H]),
    NewI is I + 1,
    printL(T, NewI, S).

printL(L, S) :- printL(L, 0, S).

printL(L) :- printL(L, 0, "").


mappings(map{
    build_pillar:[
        move_arm_start, 
        move_arm_end, 
        grip_start, 
        grip_end, 
        move_arm_start, 
        move_arm_end, 
        release_start, 
        release_end, 
        move_arm_start, 
        move_arm_end, 
        grip_start, 
        grip_end, 
        move_arm_start, 
        move_arm_end, 
        release_start, 
        release_end],
    place_architrave:[move_arm_start, move_arm_end, grip_start, grip_end, move_arm_start, move_arm_end, release_start, release_end]
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              TOTAL LL ORDER                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_mapping(Action, Total_order, Total_order) :-
    \+sub_atom(Action, _Before, _, _, '_start').

extract_mapping(Action, Total_order, NewTotal_order) :-
    sub_atom(Action, Before, _, _, '_start'),
    sub_atom(Action, 0, Before, _, ActionName),
    mappings(M), LLActions=M.ActionName,
    printL(LLActions, "\t"),
    append(Total_order, LLActions, NewTotal_order).


total_ll_order([], Total_order, _I, Total_order).
total_ll_order([HA|TA], Total_order, I, NewT) :-
    format('[~w] ~w~n', [I, HA]),
    NewI is I + 1,
    append(Total_order, [HA], NewTotal_order),
    extract_mapping(HA, NewTotal_order, NewNewTotal_order),
    total_ll_order(TA, NewNewTotal_order, NewI, NewT).

% Notice that the final order is reversed
total_ll_order(NewT):- 
    total_ll_order([build_pillar_start, build_pillar_end, build_pillar_start, build_pillar_end, place_architrave_start, place_architrave_end], [], 0, NewT), 
    write('Total LL order:'), nl,
    printL(NewT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              STATE FUNCTIONS                               %%
change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	
	change_state(S, T, S2),
	add_to_set(P, S2, S_new),
	!.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
	remove_from_set(P, S2, S_new),
	!.

conditions_met([], _S).
conditions_met([H|T], S) :- 
	member_set(H,S),
	conditions_met(T, S).

conditions_not_met([], _).
conditions_not_met([H|T], S) :- 
	\+member_set(H, S),
	conditions_not_met(T, S).

verify([]).
verify([H|T]) :-
	H,
	verify(T),
	H.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             PARTIAL LL ORDER                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % achiever states if an action is an achiever for another actions' preconditions.
% % The arguments are:
% % - List of PreconditionT
% % - List of PreconditionF
% % - List of Effects
% % - List of Achievers
% % It returns true if
% % - The add effects of the action add a preconditionT of the other action
% % - The del effects of the action del a preconditionF of the other action
% achiever([HPT|TPT], _, [add(HPT)|_TE], _).
% achiever(_, [HPF|TPF], [del(HPF)|_TE], _).
% achiever([HPT|TPT], [HPF|TPF], [_HE|TE], _):-
%     achiever(TPT, TPF, TE, _).

% achiever states if an action is an achiever for another actions' preconditions.
% The arguments are:
% - List of PreconditionT
% - List of Effects
% - List of Achievers
% It returns true if
% - The add effects of the action add a preconditionT of the other action
% Details:
% The function recursively checks if the head of the preconditions is enabled 
% by one of the effects of the action. By doing so, it recursively eliminate the 
% head of the effects and stores it in the UsedEff list. If the head of the
% preconditions is enabled by any of the effects, then it returns true (second
% function), otherwise it will loop until the effects are empty and at that 
% point it will remove the head of the preconditions and restart the recursion 
% by resetting the effects from the used effects list (third function). If after
% all the precondition are checked, the list is empty, then it returns false 
% (first function).
achiever([], _, _) :- false.
achiever([HPreT|_TPreT], [add(HPreT)|_TEff], _).
achiever([_HPreT|TPreT], [], Eff):-
    achiever(TPreT, Eff, []).
achiever(PreT, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever(PreT, TEff, NewUsedEff).

% This wrapper function is used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, Action):-
    action(Action, _PreconditionsT, _PreconditionsF, _FinalConditionsF, _Verify, Effects),
    achiever(PreT, Effects, []).


last_achievers(_PreconditionsT, [], []).
last_achievers(PreconditionsT, [HA|TA], [HA|LastAchievers]):-
    achiever(PreconditionsT, HA),
    last_achievers(PreconditionsT, TA, LastAchievers).

last_achievers(PreconditionsT, [HA|TA], LastAchievers):-
    \+achiever(PreconditionsT, HA),
    last_achievers(PreconditionsT, TA, LastAchievers).


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
    printL(ActionAchievers, '||'),
    append(ActionsAchievers, [ActionAchievers], NewActionsAchievers),
    partial_ll_order(TANames, NewState, [ActionName|CheckedActions], FinalActions, NewActionsAchievers, FinalActionsAchievers).

partial_ll_order([HAName|TANames], State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers) :-
    action(ActionName, _PreconditionsT, _PreconditionsF, __FinalConditionsF, _Verify, _Effects),
    \+functor(ActionName, HAName, _),
    partial_ll_order(TANames, State, CheckedActions, FinalActions, ActionsAchievers, FinalActionsAchievers).


% partial_ll_order([HA|TA], Partial_order, State, NewP) :-
%     ll_action(ActionName, PreconditionsT, PreconditionsF, _FinalConditionsF, _Verify, Effects),
%     functor(HA, ActionName, _),
%     conditions_met(PreconditionsT, State),
%     conditions_not_met(PreconditionsF, State),
%     change_state(State, Effects, NewState),
%     last_achievers(PreconditionsT, Partial_order, LastAchievers),
%     format('Last achievers for ~w:~n', [HA]),
%     printL(LastAchievers, '>'),
%     partial_ll_order(TA, [HA|Partial_order], NewState, NewP).


test(Actions, ActionsAchievers) :- total_ll_order(ActionNames), partial_ll_order(ActionNames, [available(r1)], Actions, ActionsAchievers).
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