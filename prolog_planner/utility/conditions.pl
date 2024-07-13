%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           CONDITIONS FUNCTIONS                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('adts.pl').
:- ensure_loaded('utility.pl').

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
% :details: It runs recursively through the list of arguments of a precondition
% and through the predicates inside the verify list. For each argument, it 
% checks if it is inside of the arguments of a verify predicate. If it is, then
% it returns the verify predicate that contains it, otherwise it continues. If
% the list of verify predicates is empty, then the call fails. 
% In the wrapper version, the function arguments are 3 instead of 4, with :#3: 
% not present and moving :#4: to :#3:.
% :#1: List of arguments
% :#2: List of verify predicates
% :#3: The list of used verify predicates
% :#4: The first verify predicate that contains the arguments
% :returns: The first verify predicate that contains the arguments (#3)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_args_verify(PreArgs, Ver, RetRes) :-
    debug_format('[check_args_verify] PreArgs: ~w~n', [PreArgs]),
    debug_format('[check_args_verify] Ver: ~w~n', [Ver]),
    check_args_verify(PreArgs, Ver, [], RetRes).

check_args_verify([], _, _, _) :- fail.
check_args_verify([_HArgs|TArgs], [], UsedVer, RetRes) :- 
    check_args_verify(TArgs, UsedVer, [], RetRes).
check_args_verify([HArgs|_TArgs], [HVer|_TVer], _, HVer) :-
    HVer =.. [_|VerifyArgs],
    member(HArgs, VerifyArgs).
check_args_verify([HArgs|TArgs], [HVer|TVer], TmpUsedVer, RetRes) :-
    HVer =.. [_|VerifyArgs],
    \+member(HArgs, VerifyArgs),
    debug_format('[check_args_verify] ~w not in ~w~n', [HArgs, VerifyArgs]),
    append(TmpUsedVer, [HVer], NewTmpUsedVer),
    check_args_verify([HArgs|TArgs], TVer, NewTmpUsedVer, RetRes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if at least one of the arguments of a preconditions is not 
% inside the verify predicates
% :details: It runs recursively through the list of arguments of a precondition
% and through the predicates inside the verify list. For each argument, it 
% checks if it is inside of the arguments of a verify predicate. If it is not, 
% then it returns the verify predicate that contains it, otherwise it continues.
% If the list of verify predicates is empty, then the call returns true. 
% In the wrapper version, the function arguments are 3 instead of 4, with :#3: 
% not present and moving :#4: to :#3:.
% :#1: List of arguments
% :#2: List of verify predicates
% :#3: The list of used verify predicates
% :#4: The first arguments that is not contained in the verify arguments
% :returns: The first argument that's not contained in the verify arguments (#3)
% TODO this does not consider the case in which Args are made of recurring
% predicates, such as `arm(agent(a1))`, nor the case in which the arguments it's 
% actually a single predicate, such as `system_sleeping`
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_not_args_verify(Pre, Ver, Res) :-
    debug_format('[check_not_args_verify_init] Pre: ~w~n', [Pre]),
    debug_format('[check_not_args_verify_init] Ver: ~w~n', [Ver]),
    Pre =.. [_|PreArgs],
    lenght(PreArgs, Len),
    check_not_args_verify(PreArgs, Ver, [], Res).

check_not_args_verify([], [], _, _).

check_not_args_verify([], Ver, _, _) :- fail.

check_not_args_verify([HArg|TArgs], [], _, HArg).

% If arg is a member, then for sure it's not what we are looking for, so move to 
% the next predicate
check_not_args_verify([HArg|TArgs], [HVer|TVer], UsedVerify, Res) :-
    HVer =.. [_|VerifyArgs],
    member(HArg, VerifyArgs),
    append(UsedVerify, [HVer], TempUsedVerify),
    append(TempUsedVerify, TVer, Verify),
    check_not_args_verify(TArgs, Verify, [], Res).

% If arg is not a member, then we keep going until we finish the list of verify
% predicates
check_not_args_verify([HArg|TArgs], [HVer|TVer], UsedVerify, Res) :-
    HVer =.. [_|VerifyArgs],
    \+member(HArg, VerifyArgs),
    append(UsedVerify, [HVer], TempUsedVerify),
    check_not_args_verify([HArg|TArgs], TVer, TempUsedVerify, Res).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks that at least one of the arguments is not a resource
% :details: It runs recursively through the list of preconditions. For each 
% precondition, it checks if its arguments are inside the verify predicates.
% If they are, then it checks if the predicate is inside the resources, and if 
% it is, then the call fails and it enters the last case recurring to the next
% predicate. If the list of predicates is empty, then the call fails.
% :#1: List of preconditions
% :#2: List of verify predicates
% :#3: The first predicate that is not inside the resources
% :returns: true if at least one predicate is not inside the resources, otherwise false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_in_resources([], _, _) :- false.

% Check if the predicate, or if any of its arguments, is not an argument in the
% verify predicates
not_in_resources([HPre|TPre], Verify, RetRes) :-
    check_not_args_verify(HPre, Verify, RetRes),
    debug_format('[in_resources2] ~w not in verify arguments, so not in resources~n', [PreArgs]),
    true.

% Check if the predicate 
not_in_resources([HPre|TPre], Verify, RetRes) :-
    HPre =.. [_|PreArgs],
    debug_format('[in_resources1] PreArgs: ~w~n', [PreArgs]),
    debug_format('[in_resources1] Verify: ~w~n', [Verify]),
    \+check_not_args_verify(PreArgs, Verify, _),
    check_args_verify(PreArgs, Verify, RetRes1),
    findall(X, resources(X), Resources),
    debug_format('[in_resources1] Resources: ~w~n', [Resources]),
    (
        \+member(RetRes, Resources)
        ->(
            true
        );(
            debug_format('[in_resources1] ~w is in resources~n', [RetRes]),
            % This RetRes must be different from the previous one
            not_in_resources(TPre, Verify, RetRes1)
        )
    ).

% not_in_resources([_HPre|TPre], Verify, _) :-
%     not_in_resources(TPre, Verify, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the arguments of the preconditions are inside the resources.
% :details: It does so by first checking if the arguments are inside the verify 
% predicates and then by checking that the predicate returned by 
% `check_args_verify` is inside the resources. This assumes that the resources
% are predicates that are inside the verify predicates, as they should be.
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
% :returns: A list of the ids of the actions that are achievers of the action
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
