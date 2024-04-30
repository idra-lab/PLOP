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

% True if at least of the condition arguments is in the resource arguments, false otherwise
check_args([], _, _) :- fail.
check_args([H|_T], Args, H) :-
    member(H, Args).

check_args([H|T], Args, Res) :-
    \+member(H, Args), 
    check_args(T, Args, Res).

% True if at least one of the predicates used in the preconditions is in the resources, false otherwise
% TODO this is agnostic w.r.t. the type of the arguments: pillar(a1, block1) where a1 is a position instead of an agent, would still be considered true
% in_resources_cond(_, [], _) :- fail.
% in_resources_cond(CondArgs, [HResource|TResources], Res) :-
%     HResource,
%     HResource =.. [_|ResourceArgs], 
%     format('Checking ~w in ~w with arguments ~w~n', [CondArgs, HResource, ResourceArgs]),
%     check_args(CondArgs, ResourceArgs, Res),
%     HResource.

% in_resources_cond(CondArgs, [HResource|TResources], Res) :-
%     HResource, !,
%     HResource =.. [_|ResourceArgs], 
%     format('Checking ~w in ~w with arguments ~w~n', [CondArgs, HResource, ResourceArgs]),
%     \+check_args(CondArgs, ResourceArgs, _),
%     in_resources_cond(CondArgs, TResources, Res).

% in_resources(Pre, Res) :-
%     findall(X, resources(X), Resources),
%     format('Resources: ~w~n', [Resources]),
%     in_resources(Pre, Resources, Res).

% in_resources([HPre|TPre], Resources, Res) :-
%     HPre =.. [_|CondArgs],
%     in_resources_cond(CondArgs, Resources, Res),
%     format('in_resources(~w, ~w) found resource ~w~n', [CondArgs, Resources, Res]).
% in_resources([HPre|TPre], Resources, Res) :-
%     HPre =.. [_|CondArgs],
%     \+in_resources_cond(CondArgs, Resources, _),
%     in_resources(TPre, Resources, Res),
%     format('in_resources(~w, ~w) did not found~n', [CondArgs, Resources]).

findall_resources([], RetResources, RetResources).

findall_resources([HResource|TResources], TmpResources, RetResources) :-
    HResource,
    (
        \+member(HResource, TmpResources)
        ->(
            append(TmpResources, [HResource], NewTmpResources),
            findall_resources([HResource|TResources], NewTmpResources, RetResources)
        );(
            findall_resources([HResource|TResources], NewTmpResources, RetResources)
        )
    ),
    findall_resources(TResources, NewTmpResources, RetResources).


findall_resources(Resources) :-
    findall(X, resources(X), TmpResources),
    findall_resources(TmpResources, [], Resources).

in_resources(Pre, Res) :-
    findall_resources(Resources),
    format('Resources: ~w~n', [Resources]),
    true.
    in_resources(Pre, Resources, Res).

% Save the listing of 'agent' predicate into a variable
% redirect_output(listing(agent), ListingOutput).


% When we don't find an achiever
achiever([], [], _, _) :- false.

% When we have finished the effects and must restart the recursion on another precondition
achiever([_HPreT|TPreT], PreF, [], Eff):-
    TPreT = [NewH|_T],
    format('\tChecking next true precondition ~w~n', [NewH]),
    achiever(TPreT, PreF, Eff, []).
achiever([], [_HPref|TPreF], [], Eff):-
    TPreF = [NewH|_T],
    format('\tChecking next false precondition ~w~n', [NewH]),
    achiever([], TPreF, Eff, []).

% When we find an achiever
achiever([HPreT|_TPreT], _PreF, [add(HPreT)|_TEff], _UsedEff) :- 
    \+in_resources([HPreT], _), 
    % format('HPreT: ~w is not in resources~n', [HPreT]),
    true.
achiever(_PreT, [HPreF|_TPreF], [del(HPreF)|_TEff], _UsedEff) :- 
    \+in_resources([HPreF], _),
    % format('HPreF: ~w is not in resources~n', [HPreF]),
    true.

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
    (
        achiever(PreT, PreF, Effects, [])
        ->(
            format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF])
        );(
            format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            fail
        )
    ),
    true.
achiever(PreT, PreF, Action):-
    mapping(Action, _),
    ll_action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    (
        achiever(PreT, PreF, Effects, [])
        ->(
            format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF])
        );(
            format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            fail
        )
    ),
    true.

% These functions are used to return a list of achievers of a certain function. 
last_achievers(_PreT, _PreF, [], []).
last_achievers(PreT, PreF, [HA|TA], [HA|LastAchievers]):-
    achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).
last_achievers(PreT, PreF, [HA|TA], LastAchievers):-
    \+achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).

% These functions are used to return a the ids of the achievers of a certain function. 
% Apparently, this code is better than having two functions, one if the action is achiever and one if it is not. TODO fix above function with same if-then-else
last_achievers_ids(_PreT, _PreF, [], RetAchievers, RetAchievers).
last_achievers_ids(PreT, PreF, [[ID-HA]|TA], Achievers, RetAchievers):-
    (
        achiever(PreT, PreF, HA) 
        ->(
            append(Achievers, [ID], NewAchievers)
        );(
            NewAchievers = Achievers
        )
    ),
    last_achievers_ids(PreT, PreF, TA, NewAchievers, RetAchievers).

last_achievers_ids(PreT, PreF, Plan, RetAchievers):-
    last_achievers_ids(PreT, PreF, Plan, [], RetAchievers).
