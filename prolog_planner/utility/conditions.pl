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
check_in_verify(Pred, [HVer|TVers], Res) :-
    % Check that Pred is not a list
    \+is_list(Pred), 
    check_in_verify(Pred, [HVer|TVers], Res, HVer).

check_in_verify(Pred, [HVer|TVers], Res) :-
    is_list(Pred),
    debug_format('[check_in_verify] Pred ~w~n', [Pred]),
    halt.

check_in_verify(Pred, [HVer|TVers], Res, LargeVer) :-
    \+var(HVer),
    functor(HVer, VerName, Arity),
    (
        Arity > 0
        ->(
            HVer =.. [VerName|VerArgs],
            check_in_verify(Pred, VerArgs, Res, LargeVer)
            ->(
                true
            );(
                TVers = [NewHVer | _],
                check_in_verify(Pred, TVers, Res, NewHVer)
            )
        );(
            Pred = VerName
            ->(
                Res = LargeVer
            );(
                TVers = [NewHVer | _],
                check_in_verify(Pred, TVers, Res, LargeVer)
            )
        )
    ),
    true.

check_in_verify(Pred, [HVer|TVers], Res, LargeVer) :-
    check_in_verify(Pred, TVers, Res, LargeVer).

check_in_verify(_, [], _, _) :- fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks that at least one of the arguments is NOT a resource
% :details: It runs recursively through the list of preconditions. For each 
% precondition, it checks if its arguments are inside the verify predicates.
% If they are, then it checks if the predicate is inside the resources, and if 
% it is, then the call fails and it enters the last case recurring to the next
% predicate. If the list of predicates is empty, then the call fails.
% :#1: List of preconditions
% :#2: List of verify predicates
% :#3: The first predicate that is NOT inside the resources
% :returns: true if at least one predicate is NOT inside the resources, otherwise false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_in_resources(Predicates, Verify, Res) :-
    findall(X, resources(X), Resources),
    not_in_resources(Predicates, Verify, Res, Resources).

not_in_resources([HPred|TPreds], Verify, Res, Resources) :-
    functor(HPred, _, Arity),
    (
        Arity > 0
        ->(
            HPred =.. [_|PredArgs],
            not_in_resources(PredArgs, Verify, Res, Resources)
            ->(
                % format('[not_in_resources1]~n'),
                true
            );(
                % format('[not_in_resources2]~n'),
                not_in_resources(TPreds, Verify, Res, Resources)
            )
        );(
            % format('[not_in_resources2.2] check_in_verify(~w, ~w)~n', [HPred, Verify]),
            check_in_verify(HPred, Verify, ResPre)
            ->(
                % format('[not_in_resources2.5]~w ~w~n', [ResPre, Resources]),
                member(ResPre, Resources)
                ->(
                    % format('[not_in_resources3]~n'),
                    not_in_resources(TPreds, Verify, Res, Resources)
                );(
                    % format('[not_in_resources4]~n'),
                    Res = HPred
                )
            );(
                % format('[not_in_resources5]~n'),
                Res = HPred
            )
        )
    ),
    true.

not_in_resources([], _, _, _) :- 
    fail.
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
achiever([], [], _, _, _) :- 
    format('[achiever0] Called~n'),
    false.

achiever([], [_HPreF|TPreF], Verify, [], Eff) :-
    format('[achiever1] Calling with [] ~w ~w ~w~n', [TPreF, Verify, Eff]),
    achiever([], TPreF, Verify, Eff, []).

% When we have finished the effects and must restart the recursion on another precondition
achiever([_HPreT|TPreT], PreF, Verify, [], Eff):-
    format('[achiever2] Calling with ~w ~w ~w ~w~n', [TPreT, PreF, Verify, Eff]),
    achiever(TPreT, PreF, Verify, Eff, []).
achiever([], [_HPreF|TPreF], Verify, [], Eff):-
    format('[achiever3] Calling with [] ~w ~w ~w~n', [TPreF, Verify, Eff]),
    achiever([], TPreF, Verify, Eff, []).

% When we find an achiever
achiever([HPreT|_TPreT], PreF, Verify, [add(HPreT)|_TEff], UsedEff) :- 
    format('[achiever4] Called~n'),
    not_in_resources([HPreT], Verify, _), 
    debug_format('HPreTAdd: ~w is not in resources~n', [HPreT]),
    true.
achiever([], [HPreF|_TPreF], Verify, [del(HPreF)|_TEff], UsedEff) :- 
    format('[achiever5] Called~n'),
    not_in_resources([HPreF], Verify, _),
    debug_format('HPreFDel: ~w is not in resources~n', [HPreF]),
    true.

% Normal execution going through the effects
achiever([], PreF, Verify, [HEff|TEff], UsedEff):-
    format('[achiever7] Calling with [] ~w ~w ~w~n', [PreF, Verify, TEff]),
    append(UsedEff, [HEff], NewUsedEff),
    achiever([], PreF, Verify, TEff, NewUsedEff).
achiever(PreT, PreF, Verify, [HEff|TEff], UsedEff):-
    append(UsedEff, [HEff], NewUsedEff),
    format('[achiever6] Calling with ~w ~w ~w ~w~n', [PreT, PreF, Verify, TEff]),
    achiever(PreT, PreF, Verify, TEff, NewUsedEff).


% This wrapper functions are used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, PreF, Verify, Action):-
    % mapping(Action, _),
    action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    debug_format('Calling func achiever with ~w ~w ~w ~w~n', [PreT, PreF, Verify, Effects]),
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
