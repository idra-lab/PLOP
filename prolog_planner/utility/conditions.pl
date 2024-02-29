%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           CONDITIONS FUNCTIONS                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('adts.pl').

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

% The function achiever states if an action is an achiever for another actions' 
% preconditions. The arguments are:
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

% TODO: an action a_i is an achiever of an action a_j also if it removes a 
% predicate that would otherwise prevent a_j from being executed. 

achiever([], _, _) :- false.
achiever([HPreT|_TPreT], [add(HPreT)|_TEff], _).
achiever([_HPreT|TPreT], [], Eff):-
    achiever(TPreT, Eff, []).
achiever(PreT, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever(PreT, TEff, NewUsedEff).

% This wrapper functoins are used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, Action):-
    action(Action, _PreconditionsT, _PreconditionsF, _FinalConditionsF, _Verify, Effects),
    achiever(PreT, Effects, []).
achiever(PreT, Action):-
    ll_action(Action, _PreconditionsT, _PreconditionsF, _FinalConditionsF, _Verify, Effects),
    achiever(PreT, Effects, []).

% These functions are used to return a list of achievers of a certain function. 
last_achievers(_PreconditionsT, [], []).
last_achievers(PreconditionsT, [HA|TA], [HA|LastAchievers]):-
    achiever(PreconditionsT, HA),
    last_achievers(PreconditionsT, TA, LastAchievers).

last_achievers(PreconditionsT, [HA|TA], LastAchievers):-
    \+achiever(PreconditionsT, HA),
    last_achievers(PreconditionsT, TA, LastAchievers).