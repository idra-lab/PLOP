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
	verify(T).

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

% When we don't find an achiever
achiever([], [], _, _) :- false.

% When we find an achiever
achiever([HPreT|_TPreT], _PreF, [add(HPreT)|_TEff], _UsedEff) :- !.
achiever(_PreT, [HPreF|_TPreF], [del(HPreF)|_TEff], _UsedEff) :- !.

% When we have finished the effects and must restart the recursion on another precondition
achiever([_HPreT|TPreT], PreF, [], Eff):-
    achiever(TPreT, PreF, Eff, []).
achiever([], [_HPref|TPreF], [], Eff):-
    achiever([], TPreF, Eff, []).

% Normal execution going through the effects
achiever(PreT, PreF, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever(PreT, PreF, TEff, NewUsedEff).
achiever([], PreF, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever([], PreF, TEff, NewUsedEff).

% This wrapper functions are used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, PreF, Action):-
    action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    achiever(PreT, PreF, Effects, []),
    format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF]).
achiever(PreT, PreF, Action):-
    ll_action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    achiever(PreT, PreF, Effects, []),
    format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF]).
achiever(PreT, PreF, Action):-
    action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    \+achiever(PreT, PreF, Effects, []),
    format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]), false.
achiever(PreT, PreF, Action):-
    ll_action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    \+achiever(PreT, PreF, Effects, []),
    format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]), false.

% These functions are used to return a list of achievers of a certain function. 
last_achievers(_PreT, _PreF, [], []).
last_achievers(PreT, PreF, [HA|TA], [HA|LastAchievers]):-
    achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).
last_achievers(PreT, PreF, [HA|TA], LastAchievers):-
    \+achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).

% These functions are used to return a the ids of the achievers of a certain function. 
last_achievers_ids(_PreT, _PreF, [], []).
last_achievers_ids(PreT, PreF, [[ID-HA]|TA], [ID|LastAchievers]):-
    achiever(PreT, PreF, HA),
    last_achievers_ids(PreT, PreF, TA, LastAchievers).
last_achievers_ids(PreT, PreF, [[_ID-HA]|TA], LastAchievers):-
    \+achiever(PreT, PreF, HA),
    last_achievers_ids(PreT, PreF, TA, LastAchievers).





