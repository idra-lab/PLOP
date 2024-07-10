%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           CONDITIONS FUNCTIONS                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('adts.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the conditions are met
% :args:
% - List of conditions
% - Current state
% :returns: true if all the conditions are met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conditions_met([], _S).
conditions_met([H|T], S) :- 
	member_set(H,S),
	conditions_met(T, S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the conditions are not met
% :args:
% - List of conditions
% - Current state
% :returns: true if all the conditions are not met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conditions_not_met([], _).
conditions_not_met([H|T], S) :- 
	\+member_set(H, S),
	conditions_not_met(T, S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the conditions can be grounded to the static KB
% :args:
% - List of conditions
% :returns: true if all the conditions can be grounded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify([]).
verify([H|T]) :-
	H,
	verify(T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the arguments of a preconditions are inside the verify
% predicates
% :returns: The first verify predicate that contains the arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_args_verify(PreArgs, Ver, RetRes) :-
    check_args_verify(PreArgs, Ver, [], RetRes).

check_args_verify([], _, _, _) :- fail.
check_args_verify([_HArgs|TArgs], [], UsedVer, RetRes) :- 
    check_args_verify(TArgs, UsedVer, [], RetRes).
check_args_verify([HArgs|_TArgs], [HVer|_TVer], _, HVer) :-
    HVer =.. [_|VerifyArgs],
    member(HArgs, VerifyArgs).
check_args_verify([HArgs|TArgs], [HVer|TVer], TmpUsedVer, RetRes) :-
    append(TmpUsedVer, [HVer], NewTmpUsedVer),
    check_args_verify([HArgs|TArgs], TVer, NewTmpUsedVer, RetRes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the arguments of the preconditions are inside the resources.
% :details: It does so by first checking if the arguments are inside the verify 
% predicates and then by checking that the predicate returned by 
% `check_args_verify` is inside the resources.
% :returns: The first predicate it finds inside the resources
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
in_resources([], _, _) :- fail.
in_resources([HPre|_TPre], Verify, RetRes) :-
    HPre =.. [_|PreArgs],
    check_args_verify(PreArgs, Verify, RetRes),
    debug_format('\t\t\t[in_resources] PreArgs: ~w~n', [PreArgs]),
    debug_format('\t\t\t[in_resources] Verify: ~w~n', [Verify]),
    findall(X, resources(X), Resources),
    debug_format('\t\t\t[in_resources] Resources: ~w~n', [Resources]),
    member(RetRes, Resources),
    true.
in_resources([_HPre|TPre], Verify, RetRes) :-
    in_resources(TPre, Verify, RetRes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks that at least one of the arguments is not a resource
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_in_resources([], _, _) :- true.
not_in_resources([HPre|_TPre], Verify, RetRes) :-
    HPre =.. [_|PreArgs],
    check_args_verify(PreArgs, Verify, RetRes),
    debug_format('\t\t[in_resources] PreArgs: ~w~n', [PreArgs]),
    debug_format('\t\t[in_resources] Verify: ~w~n', [Verify]),
    findall(X, resources(X), Resources),
    debug_format('\t\t[in_resources] Resources: ~w~n', [Resources]),
    \+member(RetRes, Resources),
    true.
not_in_resources([_HPre|TPre], Verify, RetRes) :-
    in_resources(TPre, Verify, RetRes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: It checks if an action is an achiever for another action's 
% preconditions. 
% :args:
% - List of PreconditionT
% - List of PreconditionF
% - List of Verify conditions
% - List of Effects
% - List of Achievers
% :details:
% The function recursively checks if the head of the preconditions is enabled 
% by one of the effects of the action. By doing so, it recursively eliminate the 
% head of the effects and stores it in the UsedEff list. If the head of the
% preconditions is enabled by any of the effects, then it returns true (second
% function), otherwise it will loop until the effects are empty and at that 
% point it will remove the head of the preconditions and restart the recursion 
% by resetting the effects from the used effects list (third function). If after
% all the precondition are checked, the list is empty, then it returns false 
% (first function).
% :returns: true if
% - The add effects of the action add a preconditionT of the other action, or 
% - The del effects of the action delete a preconditionF of the other action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% When we don't find an achiever
achiever([], [], _, _, _) :- false.

% When we have finished the effects and must restart the recursion on another precondition
achiever([_HPreT|TPreT], Verify, PreF, [], Eff):-
    TPreT = [NewH|_T],
    achiever(TPreT, PreF, Verify, Eff, []).
achiever([], [_HPref|TPreF], Verify, [], Eff):-
    TPreF = [NewH|_T],
    achiever([], TPreF, Verify, Eff, []).

% % When we find an achiever
% achiever([HPreT|_TPreT], _PreF, Verify, [add(HPreT)|_TEff], _UsedEff) :- 
%     \+in_resources([HPreT], Verify, _), 
%     % format('HPreT: ~w is not in resources~n', [HPreT]),
%     true.
% achiever(_PreT, [HPreF|_TPreF], Verify, [del(HPreF)|_TEff], _UsedEff) :- 
%     \+in_resources([HPreF], Verify, _),
%     % format('HPreF: ~w is not in resources~n', [HPreF]),
%     true.

    % When we find an achiever
achiever([HPreT|_TPreT], _PreF, Verify, [add(HPreT)|_TEff], _UsedEff) :- 
    \+in_resources([HPreT], Verify, _), 
    % debug_format('HPreT: ~w is not in resources~n', [HPreT]),
    true.
achiever(_PreT, [HPreF|_TPreF], Verify, [del(HPreF)|_TEff], _UsedEff) :- 
    \+in_resources([HPreF], Verify, _),
    % debug_format('HPreF: ~w is not in resources~n', [HPreF]),
    true.

% Normal execution going through the effects
achiever(PreT, PreF, Verify, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever(PreT, PreF, Verify, TEff, NewUsedEff).
achiever([], PreF, Verify, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    achiever([], PreF, Verify, TEff, NewUsedEff).

% This wrapper functions are used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, PreF, Verify, Action):-
    mapping(Action, _),
    action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    (
        achiever(PreT, PreF, Verify, Effects, [])
        ->(
            true
            % , format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF])
        );(
            % format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            fail
        )
    ),
    true.
achiever(PreT, PreF, Verify, Action):-
    ll_action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    (
        achiever(PreT, PreF, Verify, Effects, [])
        ->(
            true
            % , format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF])
        );(
            % format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            fail
        )
    ),
    true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These functions are used to return a list of achievers of a certain function. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
last_achievers(_PreT, _PreF, [], []).
last_achievers(PreT, PreF, [HA|TA], [HA|LastAchievers]):-
    achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).
last_achievers(PreT, PreF, [HA|TA], LastAchievers):-
    \+achiever(PreT, PreF, HA),
    last_achievers(PreT, PreF, TA, LastAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: These functions are used to return a the ids of the achievers of a certain 
% action. 
% :note: Apparently, this code is better than having two functions, one if the
% action is achiever and one if it is not. 
% TODO fix last_achievers function with same if-then-else
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
last_achievers_ids(_PreT, _PreF, _Verify, [], RetAchievers, RetAchievers).
last_achievers_ids(PreT, PreF, Verify, [[ID-HA]|TA], Achievers, RetAchievers):-
    (
        achiever(PreT, PreF, Verify, HA) 
        ->(
            append(Achievers, [ID], NewAchievers)
        );(
            NewAchievers = Achievers
        )
    ),
    last_achievers_ids(PreT, PreF, Verify, TA, NewAchievers, RetAchievers).

last_achievers_ids(PreT, PreF, Verify, [ID-HA|TA], Achievers, RetAchievers):-
    (
        achiever(PreT, PreF, Verify, HA) 
        ->(
            append(Achievers, [ID], NewAchievers)
        );(
            NewAchievers = Achievers
        )
    ),
    last_achievers_ids(PreT, PreF, Verify, TA, NewAchievers, RetAchievers).

last_achievers_ids(PreT, PreF, Verify, Plan, RetAchievers):-
    last_achievers_ids(PreT, PreF, Verify, Plan, [], RetAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
