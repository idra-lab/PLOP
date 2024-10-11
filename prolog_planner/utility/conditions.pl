%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           CONDITIONS FUNCTIONS                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('adts.pl').
:- ensure_loaded('utility.pl').

:-discontiguous ll_action/6.

ll_action(action_name, pred_true, pred_f, final_f, verify, eff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the conditions are met
% :args:
% - List of conditions
% - Current state
% :returns: true if all the conditions are met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conditions_met_wrapper(Conditions, State) :-
    conditions_met(Conditions, State).
which_conditions_not_met_wrapper(Conditions, State, Res) :-
    which_conditions_not_met(Conditions, State, Res).

conditions_met([], _S).
conditions_met([H|T], S) :- 
	member(H,S),
	conditions_met(T, S).

which_conditions_not_met([], _S, _R) :- fail.
which_conditions_not_met([H|T], S, H) :- 
    \+member(H, S).
which_conditions_not_met([_H|T], S, R) :-
    which_conditions_not_met(T, S, R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Checks if the conditions are not met
% :args:
% - List of conditions
% - Current state
% :returns: true if all the conditions are not met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conditions_not_met_wrapper(Conditions, State) :-
    conditions_not_met(Conditions, State).
which_conditions_met_wrapper(Conditions, State, Res) :-
    which_conditions_met(Conditions, State, Res).

conditions_not_met([], _).
conditions_not_met([H|T], S) :- 
	\+member(H, S),
	conditions_not_met(T, S).

which_conditions_met([], _S, _R) :- fail.
which_conditions_met([H|T], S, H) :- 
    member(H, S).
which_conditions_met([_H|T], S, R) :-
    which_conditions_met(T, S, R).
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
% This function checks if an action is applicable, i.e., checks if the 
% preconditions are met and the final conditions are not met
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
    is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, []).

is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Goal) :-
    verify(Verify),
    % debug_format('Grounded ~w\n', [Verify]),
    (
        conditions_met_wrapper(PreconditionsT, State)
        ->(
            % debug_format('Checked that the preconditions are met ~w in state ~w\n', [PreconditionsT, State]),
            (
                conditions_not_met_wrapper(PreconditionsF, State)
                ->(
                    true
                );(
                    % which_conditions_met_wrapper(FinalConditionsF, State, R),
                    % debug_format('Precondition ~w is met in state ~w, but should not\n', [R, State]),
                    fail
                )
            )
        );(
            % debug_format('Some precondition is not met in state ~w\n', [State]),
            % which_conditions_not_met_wrapper(PreconditionsT, State, R), 
            % debug_format('Precondition ~w is not met in state ~w\n', [R, State]), 
            fail
        )
    ),
    true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
why_not_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify) :-
    why_not_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, []).

why_not_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Goal) :-
    debug_format('Checking why action is not applicable\n'),
    verify(Verify),
    (
        which_conditions_not_met_wrapper(PreconditionsT, State, R)
        ->(
            debug_format('Precondition ~w is not met in state ~w, but should\n', [R, State])
        );(
            which_conditions_met_wrapper(PreconditionsF, State, R)
            ->(
                debug_format('Precondition ~w is met in state ~w, but should not\n', [R, State])
            );(
                true
            )
        )
    ),
    true.


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
    % debug_format('[check_in_verify0] ~w ~w ~n', [Pred, [HVer|TVers]]),
    check_in_verify(Pred, [HVer|TVers], Res, HVer).

check_in_verify(Pred, [HVer|TVers], Res) :-
    is_list(Pred),
    % debug_format('[check_in_verify0.5] Pred ~w~n', [Pred]),
    halt.

check_in_verify(Pred, [HVer|TVers], Res, LargeVer) :-
    \+var(HVer),
    \+number(HVer),
    functor(HVer, VerName, Arity),
    (
        Arity > 0
        ->(
            HVer =.. [VerName|VerArgs],
            check_in_verify(Pred, VerArgs, Res, LargeVer)
            ->(
                % debug_format('[check_in_verify1] ~w ~w ~n', [Pred, [HVer|TVers]]),
                true
            );(
                TVers = [NewHVer | _],
                % debug_format('[check_in_verify2] ~w ~w ~n', [Pred, [HVer|TVers]]),
                check_in_verify(Pred, TVers, Res, NewHVer)
            )
        );(
            Pred = VerName
            ->(
                % debug_format('[check_in_verify3] ~w ~w ~n', [Pred, [HVer|TVers]]),
                Res = LargeVer
            );(
                TVers = [NewHVer | _],
                % debug_format('[check_in_verify4] ~w ~w ~n', [Pred, [HVer|TVers]]),
                check_in_verify(Pred, TVers, Res, LargeVer)
            )
        )
    ),
    % debug_format('[check_in_verify5] ~w ~w ~n', [Pred, [HVer|TVers]]),
    true.

check_in_verify(Pred, [HVer|TVers], Res, LargeVer) :-
    % debug_format('[check_in_verify6] ~w ~w ~n', [Pred, [HVer|TVers]]),
    check_in_verify(Pred, TVers, Res, LargeVer).

check_in_verify(_, [], _, _) :- 
    % debug_format('[check_in_verify7]~n'),
    fail.
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
                % debug_format('[not_in_resources1]~n'),
                true
            );(
                % debug_format('[not_in_resources2]~n'),
                not_in_resources(TPreds, Verify, Res, Resources)
            )
        );(
            % debug_format('[not_in_resources2.2] calling check_in_verify(~w, ~w)~n', [HPred, Verify]),
            % leash(-all),trace,
            % (check_in_verify(HPred, Verify, ResPre), notrace)
            check_in_verify(HPred, Verify, ResPre)
            ->(
                % debug_format('[not_in_resources2.5] ~w ~w~n', [ResPre, Resources]),
                member(ResPre, Resources)
                ->(
                    % debug_format('[not_in_resources3] ~w~n', [TPreds]),
                    not_in_resources(TPreds, Verify, Res, Resources)
                );(
                    % debug_format('[not_in_resources4]~n'),
                    Res = HPred
                )
            );(
                % debug_format('[not_in_resources5]~n'),
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
% - List of used effects
% :details:
% The function recursively checks if the head of the preconditions is enabled 
% by one of the effects of the action. By doing so, it recursively eliminate the 
% head of the effects and stores it in the UsedEff list. If the head of the
% preconditions is enabled by any of the effects, then it checks if the 
% predicate is part of the resources. If it is not, then it returns true, 
% otherwise it will recurse until the effects are empty and at that 
% point it will remove the head of the preconditions and restart the recursion 
% by resetting the effects from the used effects list (third function). If after
% all the precondition are checked, the list is empty, then it returns false.
% :returns: true if
% - The add effects of the action adds a preconditionT of the other action, or 
% - The del effects of the action deletes a preconditionF of the other action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% When we have finished the effects and must restart the recursion on another precondition
achiever([], [_HPreF|TPreF], Verify, [], Eff) :-
    debug_format('[achiever1] Calling with~n'),
    debug_format('\t[achiever1] PreT:   []~n'),
    debug_format('\t[achiever1] PreF:   ~w~n', [TPreF]),
    debug_format('\t[achiever1] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever1] Eff:    ~w~n', [Eff]),
    achiever([], TPreF, Verify, Eff, []).

achiever([_HPreT|TPreT], PreF, Verify, [], Eff):-
    debug_format('[achiever2] Calling with~n'),
    debug_format('\t[achiever2] PreT:   ~w~n', [TPreT]),
    debug_format('\t[achiever2] PreF:   ~w~n', [PreF]),
    debug_format('\t[achiever2] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever2] Eff:    ~w~n', [Eff]),
    achiever(TPreT, PreF, Verify, Eff, []).

% When the head of the preconditions is enabled by the head of the effects
achiever([HPreT|TPreT], PreF, Verify, [add(HPreT)|TEff], UsedEff) :- 
    debug_format('[achiever4] Called with~n'),
    debug_format('\t[achiever4] PreT:   ~w~n', [[HPreT|TPreT]]),
    debug_format('\t[achiever4] PreF:   ~w~n', [PreF]),
    debug_format('\t[achiever4] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever4] Eff:    ~w~n', [[add(HPreT)|TEff]]),
    not_in_resources([HPreT], Verify, _)
    ->(
        debug_format('[achiever4.1] ~w not in resources with verify ~w~n', [HPreT, Verify]),
        true
    );(
        debug_format('[achiever4.2] ~w in resources with verify ~w~n', [HPreT, Verify]),
        append(UsedEff, [add(HPreT)], NewUsedEff),
        achiever([HPreT|TPreT], PreF, Verify, TEff, NewUsedEff)
    ).

achiever([], [HPreF|TPreF], Verify, [del(HPreF)|TEff], UsedEff) :- 
    debug_format('[achiever5] Called with~n'),
    debug_format('\t[achiever5] PreT:   []~n'),
    debug_format('\t[achiever5] PreF:   ~w~n', [[HPreF|TPreF]]),
    debug_format('\t[achiever5] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever5] Eff:    ~w~n', [[del(HPreF)|TEff]]),
    not_in_resources([HPreF], Verify, _)
    ->(
        debug_format('[achiever5.1] ~w not in resources with verify ~w~n', [HPreF, Verify]),
        true
    );(
        debug_format('[achiever5.2] ~w in resources with verify ~w~n', [HPreF, Verify]),
        append(UsedEff, [del(HPreF)], NewUsedEff),
        achiever([], [HPreF|TPreF], Verify, TEff, NewUsedEff)
    ).

% When the head of the preconditions is not enabled by the head of the effects
achiever([HPreT|TPreT], PreF, Verify, [HEff|TEff], UsedEff):-
    HEff \= add(HPreT), 
    debug_format('[achiever6.1] Called with~n'),
    debug_format('\t[achiever6.1] PreT:   ~w~n', [[HPreT|TPreT]]),
    debug_format('\t[achiever6.1] PreF:   ~w~n', [PreF]),
    debug_format('\t[achiever6.1] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever6.1] Eff:    ~w~n', [[HEff|TEff]]),
    append(UsedEff, [HEff], NewUsedEff),
    achiever([HPreT|TPreT], PreF, Verify, TEff, NewUsedEff).

achiever([], [HPreF|TPreF], Verify, [HEff|TEff], UsedEff):-
    HEff \= del(HPreF), 
    debug_format('[achiever7.1] Called with~n'),
    debug_format('\t[achiever7.1] PreT:   []~n'),
    debug_format('\t[achiever7.1] PreF:   ~w~n', [[HPreF|TPreF]]),
    debug_format('\t[achiever7.1] Verify: ~w~n', [Verify]),
    debug_format('\t[achiever7.1] Eff:    ~w~n', [[HEff|TEff]]),
    append(UsedEff, [HEff], NewUsedEff),
    achiever([], [HPreF|TPreF], Verify, TEff, NewUsedEff).

% When we don't find an achiever
achiever([], [], _, _, _) :- 
    % leash(-all),trace,
    debug_format('[achiever0] Called~n'),
    fail.

% This wrapper functions are used to call the achiever function with the correct
% arguments and check whether the action is a high-level or a low-level one.
achiever(PreT, PreF, Verify, Action):-
    % mapping(Action, _),
    action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    % debug_format('Calling func achiever with ~w ~w ~w ~w~n', [PreT, PreF, Verify, Effects]),
    (
        achiever(PreT, PreF, Verify, Effects, [])
        ->(
            % debug_format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            true
        );(
            % debug_format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            fail
        )
    ),
    true.
achiever(PreT, PreF, Verify, Action):-
    ll_action(Action, _PreT, _PreF, _FinalConditionsF, _Verify, Effects),
    (
        achiever(PreT, PreF, Verify, Effects, [])
        ->(
            % debug_format('Action ~w ~w is an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
            true
        );(
            % debug_format('Action ~w ~w is not an achiever ~w ~w~n', [Action, Effects, PreT, PreF]),
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
    disable_debug,
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
    disable_debug,
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
