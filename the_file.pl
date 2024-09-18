gen_first_row(TmpRow, 1, Row) :-
  append([0], TmpRow, Row).
gen_first_row(TmpRow, Length, Row) :-
  NewLength is Length - 1,
  append([1], TmpRow, NewTmpRow),
  gen_first_row(NewTmpRow, NewLength, Row).

append_first_row(Matrix, Length, RetMatrix) :-
  gen_first_row([], Length, Row),
  append([Row], Matrix, RetMatrix).

gen_last_row(Row, 0, Row).
gen_last_row(TmpRow, Length, Row) :-
  NewLength is Length - 1,
  append([0], TmpRow, NewTmpRow),
  gen_last_row(NewTmpRow, NewLength, Row).

append_last_row(Matrix, Length, RetMatrix) :-
  gen_last_row([], Length, Row),
  append([Row], Matrix, RetMatrix).

extract_adj_matrix_actions(LastAchievers, RetMatrix, Actions):-
  length(LastAchievers, PlanLength),
  ActualLength is PlanLength + 2,
  append_first_row([], ActualLength, TmpMatrix),
  extract_adj_matrix_actions(LastAchievers, TmpMatrix, TmpMatrix2, PlanLength, 0, [], Actions),
  append_last_row(TmpMatrix2, ActualLength, TmpMatrix3),
  reverse(TmpMatrix3, RetMatrix).  

extract_adj_matrix_actions(_LastAchievers, RetMatrix, RetMatrix, PlanLength, PlanLength, RetActions, RetActions).
extract_adj_matrix_actions(LastAchievers, TmpMatrix, RetMatrix, PlanLength, PlanID, TmpActions, RetActions):-
  extract_row_achievers(LastAchievers, PlanID, PlanLength, [1], Row),
  append([Row], TmpMatrix, NewTmpMatrix),
  NewPlanID is PlanID + 1,
  nth0(PlanID, LastAchievers, _ID-Action-_Achievers),
  MatrixPos is PlanLength - PlanID,
  append([MatrixPos-Action], TmpActions, NewTmpActions),
  extract_adj_matrix_actions(LastAchievers, NewTmpMatrix, RetMatrix, PlanLength, NewPlanID, NewTmpActions, RetActions).

extract_row_achievers(_, _RefActionID, 0, TmpRow, RetRow) :-
  append([0], TmpRow, RetRow),
  length(TmpRow, Length).
extract_row_achievers([_ID-_A-AAchievers|T], RefActionID, I, TmpRow, RetRow):-
  (
    member(RefActionID, AAchievers) 
    -> 
      append([1], TmpRow, NewTmpRow)
    ; 
      append([0], TmpRow, NewTmpRow)
  ),
  NewI is I - 1,
  extract_row_achievers(T, RefActionID, NewI, NewTmpRow, RetRow).

% Extract the dictionary of time-triggered actions
extract_tt_action_list(Actions, TTActionList) :-
  length(Actions, PlanLength),
  FinalAction is PlanLength + 1,
  extract_tt_action_list(Actions, 1, [0-[0-init_s()]-[FinalAction-end_e()]], TmpTTActionList),
  reverse(TmpTTActionList, TTActionList).

start_action(Action, ActionName, Args) :-
  Action =.. [StartActionName | Args],
  % format('Start action: ~w ~w~n', [StartActionName, Args]),
  sub_string(StartActionName, Before, Length, After, '_start'),
  sub_string(StartActionName, 0, Before, Length, ActionName),
  % format('Action name ~w~n', [ActionName]),
  true.

end_action(Action, ActionName, Args) :-
  Action =.. [EndActionName | Args],
  % format('End action: ~w ~w~n', [EndActionName, Args]),
  sub_string(EndActionName, Before, Length, After, '_end'),
  sub_string(EndActionName, 0, Before, Length, ActionName),
  % format('Action name ~w~n', [ActionName]),
  true.

extract_tt_action_list([], _, TTActionList, TTActionList).
extract_tt_action_list([ID-Action|T], TT_ID, TmpTTActionList, TTActionList) :-
  % format('Action: ~w~n', [Action]),
  start_action(Action, ActionName, Args),
  find_tt_end_action(T, ActionName, Args, [EndID-EndAction]),
  % format('End action: ~w~n', [EndAction]),
  NewTT_ID is TT_ID + 1,
  append([TT_ID-[ID-Action]-[EndID-EndAction]], TmpTTActionList, NewTmpTTActionList),
  extract_tt_action_list(T, NewTT_ID, NewTmpTTActionList, TTActionList).
extract_tt_action_list([ID-Action|T], TT_ID, TmpTTActionList, TTActionList) :-
  % format('Action: ~w~n', [Action]),
  \+start_action(Action, ActionName, Args),
  extract_tt_action_list(T, TT_ID, TmpTTActionList, TTActionList).

find_tt_end_action([], _ActionName, _Args, _EndAction) :- 
  % format('THIS SHOULD NOT HAPPEN'), 
  fail.
find_tt_end_action([ID-EndAction|T], ActionName, Args, [ID-EndAction]) :-
  % format('Testing end action: ~w against ~w ~w ~n', [EndAction, ActionName, Args]),
  end_action(EndAction, ActionName, Args).
find_tt_end_action([_ID-EndAction|T], ActionName, Args, [RetID-RetEndAction]) :-
  % format('Testing end action: ~w against ~w ~w ~n', [EndAction, ActionName, Args]),
  \+end_action(EndAction, ActionName, Args),
  find_tt_end_action(T, ActionName, Args, [RetID-RetEndAction]).
  
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

%%%%%%%%%%%%%%%%%%%% stack operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, list based implementation of stacks

    % empty stack generates/tests an empty stack

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

empty_stack([]).

    % member_stack tests if an element is a member of a stack

member_stack(E, S) :- member(E, S).

    % stack performs the push, pop and peek operations
    % to push an element onto the stack
        % ?- stack(a, [b,c,d], S).
    %    S = [a,b,c,d]
    % To pop an element from the stack
    % ?- stack(Top, Rest, [a,b,c]).
    %    Top = a, Rest = [b,c]
    % To peek at the top element on the stack
    % ?- stack(Top, _, [a,b,c]).
    %    Top = a 

stack(E, S, [E|S]).

length_stack(S, Len) :- length(S, Len).

%%%%%%%%%%%%%%%%%%%% queue operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, list based implementation of 
    % FIFO queues

    % empty queue generates/tests an empty queue


empty_queue([]).

    % member_queue tests if an element is a member of a queue

member_queue(E, S) :- member(E, S).

    % add_to_queue adds a new element to the back of the queue

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

    % remove_from_queue removes the next element from the queue
    % Note that it can also be used to examine that element 
    % without removing it
    
remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :- 
    append(First, Second, Concatenation).

%%%%%%%%%%%%%%%%%%%% set operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, 
    % list based implementation of sets
    
    % empty_set tests/generates an empty set.

empty_set([]).

member_set(E, S) :- member(E, S).

    % add_to_set adds a new member to a set, allowing each element
    % to appear only once

add_to_set(X, S, S) :- member(X, S).
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T).
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new).
    
union([], S, S).
union([H|T], S, S_new) :- 
    union(T, S, S2),
    add_to_set(H, S2, S_new).   
    
intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
    member_set(H, S),
    intersection(T, S, S_new).
intersection([_|T], S, S_new) :-
    intersection(T, S, S_new).
    
set_diff([], _, []).
set_diff([H|T], S, T_new) :- 
    member_set(H, S), 
    set_diff(T, S, T_new).
set_diff([H|T], S, [H|T_new]) :- 
    set_diff(T, S, T_new).

subset([], _).
subset([H|T], S) :- 
    member_set(H, S), 
    subset(T, S).

equal_set(S1, S2) :- 
    subset(S1, S2), subset(S2, S1).
    
%%%%%%%%%%%%%%%%%%%%%%% priority queue operations %%%%%%%%%%%%%%%%%%%

    % These predicates provide a simple list based implementation
    % of a priority queue.
    
    % They assume a definition of precedes for the objects being handled
    
empty_sort_queue([]).

member_sort_queue(E, S) :- member(E, S).

insert_sort_queue(State, [], [State]).  
insert_sort_queue(State, [H | T], [State, H | T]) :- 
    precedes(State, H).
insert_sort_queue(State, [H|T], [H | T_new]) :- 
    insert_sort_queue(State, T, T_new). 
    
remove_sort_queue(First, [First|Rest], Rest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           CONDITIONS FUNCTIONS                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    notrace,
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
extract_resources_number(Resources) :-
  findall(X, resources(X), AllResources),
  extract_resources_number(AllResources, [], Resources).

extract_resources_number([], Resources, Resources).
extract_resources_number([Resource|T], TmpResources, Resources) :- 
  findall(Resource, Resource, List),
  length(List, Len),
  functor(Resource, ResourceName, _),
  append([ResourceName-Len], TmpResources, NewTmpResources),
  extract_resources_number(T, NewTmpResources, Resources).



extract_resources_per_action(TTActions, Resources, ActionXResources) :-
  [H|T] = TTActions,
  extract_resources_per_action(T, Resources, [], ActionXResources).

extract_resources_per_action([], _Resources, ActionXResources, ActionXResources).
extract_resources_per_action([TT_ID-StartAction-EndAction|T], Resources, TmpActionXResources, ActionXResources) :-
  check_resources_per_action(StartAction, EndAction, Resources, [], RetResources),
  append([TT_ID-RetResources], TmpActionXResources, NewActionXResources),
  extract_resources_per_action(T, Resources, NewActionXResources, ActionXResources).

check_resources_per_action(StartAction, EndAction, [], UsedResources, UsedResources).
check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resource_in_verify(Resource, []) :- fail.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  H =.. [Resource | _],
  true.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  \+ (H =.. [Resource | _]),
  check_resource_in_verify(Resource, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              STATE FUNCTIONS                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	
	change_state(S, T, S2),
	add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	
  change_state(S, T, S2),
	remove_from_set(P, S2, S_new), !.

member_state(S, [H|_]) :- 	equal_set(S, H).
member_state(S, [_|T]) :- 	member_state(S, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             UTILITY FUNCTIONS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Change planner_debug(X) to set the debug mode on or off. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enable_debug  :- retractall(planner_debug(_)), assert(planner_debug(true)).
disable_debug :- retractall(planner_debug(_)), assert(planner_debug(fasle)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints the message with the format provided if the predicate 
% planner_debug(true) is set.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug_format(Format) :- 
	planner_debug(true) -> format(Format); true.

debug_format(Format, Args) :- 
	planner_debug(true) -> format(Format, Args); true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints the list of actions reversed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse_print_actions(Actions) :-
	length_stack(Actions, Len),
	I is Len,
	reverse_print_actions(Actions, I).

reverse_print_actions(Actions, _I) :-
	empty_stack(Actions).

reverse_print_actions(Actions, I) :-
	stack(A, TActions, Actions), 
	NewI is I - 1,
	reverse_print_actions(TActions, NewI),
	format("[~w] ~w~n", [I, A]).

reverse_print_actions_times(Actions, Times) :-
	length_stack(Actions, Len), length_stack(Times, Len),
	I is Len,
	reverse_print_actions_times(Actions, Times, I).

reverse_print_actions_times(Actions, Times, _I) :-
	empty_stack(Actions),
	empty_stack(Times).

reverse_print_actions_times(Actions, Times, I) :-
	stack(A, TActions, Actions), 
	stack(T, TTimes, Times),
	NewI is I - 1,
	reverse_print_actions_times(TActions, TTimes, NewI),
	format("[~w] ~w ~w~n", [I, A, T]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints a list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_list([], _I, _S).
print_list([H|T], I, S) :- 
    format('~w[~w] ~w~n', [S, I, H]),
    NewI is I + 1,
    print_list(T, NewI, S).

print_list(L, S) :- print_list(L, 0, S).

print_list(L) :- print_list(L, 0, "").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: reverses a list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse_list([],Acc,Acc).
reverse_list([H|T],Acc,Ret) :- reverse_list(T,[H|Acc],Ret).
reverse_list(X,Y) :- reverse_list(X,[],Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_state([
  ontable(b1), ontable(b2), ontable(b4),
  on(b5, b1), on(b3, b2),
  at(b1, 2, 2), at(b2, 4, 4), at(b4, 8, 8),
  clear(b3), clear(b4), clear(b5),
  available(a1), available(a2), available(a3), available(a4),
  available(a5), available(a6), available(a7), available(a8)
]).

goal_state([
  on(b1, b5), at(b1, 5, 5), at(b5, 5, 5),
  clear(b1), ontable(b5),
  ontable(b2), at(b2, 4, 4), clear(b2),
  ontable(b3), at(b3, X3, Y3), clear(b3), % X3, Y3 are unspecified positions
  ontable(b4), at(b4, 8, 8), clear(b4),
  available(a1), available(a2), available(a3), available(a4),
  available(a5), available(a6), available(a7), available(a8)
]).

% Define the agents
agent(a1). agent(a2). agent(a3). agent(a4).
agent(a5). agent(a6). agent(a7). agent(a8).

resources(agent(_)).

% Define the blocks
block(b1). block(b2). block(b3). block(b4). block(b5).

% Define the positions on the table
pos(0,0). pos(1,1). pos(2,2). pos(3,3). pos(4,4).
pos(5,5). pos(6,6). pos(7,7). pos(8,8). % ... and so on for all positions on the table

% Agent starts gripping a block on the table
action(
  grip_ontable_start(Agent, Block, X, Y),
  [ontable(Block), at(Block, X, Y), clear(Block), available(Agent)],
  [gripped(_, Block)],
  [],
  [agent(Agent), block(Block), pos(X, Y)],
  [del(available(Agent)), del(clear(Block)), del(ontable(Block)), add(gripping(Agent, Block))]
).

% Agent finishes gripping a block on the table
action(
  grip_ontable_end(Agent, Block),
  [gripping(Agent, Block)],
  [],
  [],
  [agent(Agent), block(Block)],
  [add(gripped(Agent, Block))]
).

% Agent starts gripping a block on top of another block
action(
  grip_onblock_start(Agent, Block, BelowBlock),
  [on(Block, BelowBlock), clear(Block), available(Agent)],
  [gripped(_, Block)],
  [],
  [agent(Agent), block(Block), block(BelowBlock)],
  [del(available(Agent)), del(clear(Block)), del(on(Block, BelowBlock)), add(gripping(Agent, Block)), add(clear(BelowBlock))]
).

% Agent finishes gripping a block on top of another block
action(
  grip_onblock_end(Agent, Block),
  [gripping(Agent, Block)],
  [],
  [],
  [agent(Agent), block(Block)],
  [add(gripped(Agent, Block))]
).

% Agent starts releasing a block on the table
action(
  release_ontable_start(Agent, Block, X, Y),
  [gripped(Agent, Block), pos(X, Y)],
  [at(_, X, Y)],
  [],
  [agent(Agent), block(Block), pos(X, Y)],
  [del(gripped(Agent, Block)), add(releasing(Agent, Block))]
).

% Agent finishes releasing a block on the table
action(
  release_ontable_end(Agent, Block, X, Y),
  [releasing(Agent, Block)],
  [],
  [],
  [agent(Agent), block(Block), pos(X, Y)],
  [add(ontable(Block)), add(at(Block, X, Y)), add(clear(Block)), add(available(Agent))]
).

% Agent starts releasing a block on top of another block
action(
  release_onblock_start(Agent, Block, BelowBlock),
  [gripped(Agent, Block), clear(BelowBlock)],
  [on(_, BelowBlock)],
  [],
  [agent(Agent), block(Block), block(BelowBlock)],
  [del(gripped(Agent, Block)), add(releasing(Agent, Block))]
).

% Agent finishes releasing a block on top of another block
action(
  release_onblock_end(Agent, Block, BelowBlock),
  [releasing(Agent, Block)],
  [],
  [],
  [agent(Agent), block(Block), block(BelowBlock)],
  [add(on(Block, BelowBlock)), del(clear(BelowBlock)), add(available(Agent))]
).

% Agent starts moving a block from one position to another
action(
  move_start(Agent, Block, X1, Y1, X2, Y2),
  [gripped(Agent, Block), at(Block, X1, Y1), pos(X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent), block(Block), pos(X1, Y1), pos(X2, Y2)],
  [del(at(Block, X1, Y1)), add(moving(Agent, Block, X2, Y2))]
).

% Agent finishes moving a block from one position to another
action(
  move_end(Agent, Block, X1, Y1, X2, Y2),
  [moving(Agent, Block, X2, Y2)],
  [],
  [],
  [agent(Agent), block(Block), pos(X1, Y1), pos(X2, Y2)],
  [del(moving(Agent, Block, X2, Y2)), add(at(Block, X2, Y2))]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                  ACTIONS                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                     KB                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                  MAPPINGS                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   TESTS                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_hl_goal(Goal, HLGoal) :-
  findall(Action, action(Action, _, _, _, _, _), ActionsList).
  extract_hl_goal(Actions, Goal, [], HLGoal).

extract_hl_goal([], _Goal, HLGoal, HLGoal).
extract_hl_goal([HAction|TActions], Goal, TempHLGoal, HLGoal) :-
  action(HAction, PreconditionsT, PreconditionsF, _FinalConditionsF, _Verify, Effects),
  check_goal_in_preconditions(Goal, PreconditionsT, RetPredicates),
  % check_goal_in_preconditions(PreconditionsF, Goal),
  % check_goal_in_effects(Effects, Goal)
  true.

check_goal_in_preconditions(Goal, Preconditions, RetPredicates) :-
  check_goal_in_preconditions(Goal, Preconditions, [], RetPredicates).

check_goal_in_preconditions([], _Preconditions, RetPredicates, RetPredicates).
check_goal_in_preconditions([HGoal|TGoals], Preconditions, Predicates, RetPredicates) :-
  (
    match_goal(HGoal, Preconditions)
    ->(
      append([HGoal], Predicates, NewPredicates),
      debug_format('Matched goal ~w in ~w\n', [HGoal, Preconditions])
    ); (
      append([], Predicates, NewPredicates),
      debug_format('Goal not matched ~w in ~w\n', [HGoal, Preconditions])
    )
  ),
  check_goal_in_preconditions(TGoals, Preconditions, NewPredicates, RetPredicates).

match_goal(Goal, []) :- false.
match_goal(Goal, [HPrecondition|TPreconditions]) :-
  functor(Goal, Name, Arity),
  (
    functor(HPrecondition, Name, Arity)
    -> true
    ;  match_goal(Goal, TPreconditions)
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function is used to add the last achievers for end actions.
% For each end action, the start action is an achiever and if the action is not
% low-level, then all the lower-level action between the start action and the 
% end action are achievers of the end action
% TODO I should also check that the actions have the same arguments here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_achievers_end(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end(PrevActionName, [[_ID-HAction]|_TActions], LastAchievers, LastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName).
add_achievers_end(PrevActionName, [[ID-HAction]|TActions], LastAchievers, RetLastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  \+sub_string(ActionName, _, _, _, PrevActionName),
  append([ID], LastAchievers, TempLastAchievers),
  add_achievers_end(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).

add_achievers_end_ll(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end_ll(PrevActionName, [_ID-HAction|_TActions], LastAchievers, LastAchievers, Pre) :-
  debug_format('~w[add_achievers_end] Is ~w the start action ~w_start\n', [Pre, HAction, PrevActionName]),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName),
  sub_string(ActionName, _, _, _, '_start'),
  debug_format('~w[add_achievers_end] Found start action ~w for action ~w\n', [Pre, ActionName, PrevActionName]),
  true.
add_achievers_end_ll(PrevActionName, [ID-HAction|TActions], LastAchievers, RetLastAchievers, Pre) :-
  debug_format('~w[add_achievers_end] ~w is not the start action of ~w_end\n', [Pre, HAction, PrevActionName]),
  % functor(HAction, ActionName, _),
  % \+((
  %   sub_string(ActionName, _, _, _, PrevActionName),
  %   sub_string(ActionName, _, _, _, '_start')
  % )),
  append([ID], LastAchievers, TempLastAchievers),
  debug_format('~w[add_achievers_end] Adding achiever ~w for action ~w\n', [Pre, HAction, ID]),
  debug_format('~w[add_achievers_end] to ~w\n', [Pre, LastAchievers]),
  debug_format('~w[add_achievers_end] obtaining ~w\n', [Pre, TempLastAchievers]),
  add_achievers_end_ll(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As a first iteration, all the low-level actions that are not mapped to any 
% lower level action share the same resources and hence are achievers of the 
% following low-level actions in the same mapping. This function adds all the
% previous low-level actions as achievers of the current low-level action, up 
% until the previous high-level action.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_no_mapping_achievers(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre) :-
  \+mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] No mapping for action ~w adding prev resource constraints\n', [Pre, HAction]),
  debug_format('~w[add_no_mapping_achievers] Length: ~w\n', [Pre, Lenght]),
  debug_format('~w[add_no_mapping_achievers] IDHLAction: ~w\n', [Pre, IDHLAction]),
  debug_format('~w[add_no_mapping_achievers] Plan: ~w\n', [Pre, Plan]),
  debug_format('~w[add_no_mapping_achievers] HAction: ~w\n', [Pre, HAction]),
  add_no_mapping_achievers_wrapped(Lenght-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre),
  debug_format('~w[add_no_mapping_achievers] New achievers ~w\n', [Pre, RetLastAchievers]).

add_no_mapping_achievers(_Lenght-HAction, _Plan, _IDHLAction, TempLastAchievers, TempLastAchievers, Pre) :-
  mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] Mapping for action ~w, not adding prev resource constraints\n', [Pre, HAction]),
  true.
add_no_mapping_achievers_wrapped(_ID-_Action, [IDHLAction-_PrevAction|_], IDHLAction, RetAchievers, RetAchievers, Pre) :-
  debug_format('~w[add_no_mapping_achievers_wrapped single] New mappings ~w\n', [Pre, RetAchievers]),
  true.
add_no_mapping_achievers_wrapped(ID-Action, [PrevID-PrevAction|T], IDHLAction, TmpAchievers, RetAchievers, Pre) :-
  IDHLAction \= PrevID,
  append([PrevID], TmpAchievers, NewTmpAchievers),
  debug_format('~w[add_no_mapping_achievers_wrapped single] Adding achiever ~w ~w for action ~w ~w, achievers: ~w\n', [Pre, PrevID, PrevAction, ID, Action, NewTmpAchievers]),
  add_no_mapping_achievers_wrapped(ID-Action, T, IDHLAction, NewTmpAchievers, RetAchievers, Pre).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_mappings(Init, Goal, HL_Plan, HL_Achievers, LL_Plan, LL_Achievers) :-
  debug_format('[apply_mappings] The HL plan is ~w\n', [HL_Plan]),
  apply_mappings(Init, Goal, HL_Plan, HL_Achievers, [], [], LL_Plan, LL_Achievers).

apply_mappings(Init, Goal, [], _HL_Achievers, LL_Plan, LL_Achievers, LL_Plan, LL_Achievers) :-
  debug_format('[apply_mappings] Reached this point ~w\n', [LL_Plan]),
  true.

apply_mappings(Init, Goal, [[IDHLAction-HL_Action]|T_HL_Actions], [IDHLAction-HL_Action-HL_Achievers|T_HL_Achievers], Plan, LastAchievers, RetPlan, RetLastAchievers) :-
  enable_debug,
  debug_format('\n\n[apply_mappings] HL_Action: ~w-~w\n', [IDHLAction,HL_Action]), 
  disable_debug,
  % debug_format('[apply_mappings] HL_Achievers:\n'), 
  print_list([HL_Achievers]),
  length(Plan, Length),
  action(HL_Action, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
  append([Length-HL_Action], Plan, TempPlan),
  change_state(Init, Effects, CurrentState),
  % debug_format('[apply_mappings] Finding last achievers for: ~w\n', [HL_Action]),
  % debug_format('[apply_mappings] PreconditionsT: ~w\n', [PreconditionsT]),
  % debug_format('[apply_mappings] PreconditionsF: ~w\n', [PreconditionsF]),
  % debug_format('[apply_mappings] Verify: ~w\n', [Verify]),
  % debug_format('[apply_mappings] Plan: ~w\n', [Plan]),
  (
    IDHLAction = 2
    -> (
      enable_debug,
      last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, TempActionLastAchievers),
      disable_debug,
      debug_format('[apply_mappings] Last achievers for ~w: ~w\n', [HL_Action, TempActionLastAchievers])
    );(
      true
    )
  ),
  % format(atom(Pre), '\t~w', [HL_Action]),
  Pre = '\t',
  (
    mapping(HL_Action, Mappings) 
    ->(
      append([Length-HL_Action-TempActionLastAchievers], LastAchievers, TempLastAchievers),
      % debug_format('[apply_mappings] Found mapping for action ~w ~w ~w\n', [HL_Action, Mappings, Length]),
      % debug_format('[apply_mappings] Calling apply_action_map with'),
      % debug_format('[apply_mappings] Mappings: ~w\n', [Mappings]), 
      % debug_format('[apply_mappings] Length: ~w\n', [Length]), 
      % debug_format('[apply_mappings] CurrentState: ~w\n', [CurrentState]), 
      % debug_format('[apply_mappings] TempPlan: ~w\n', [TempPlan]),
      apply_action_map(Mappings, Length, CurrentState, TempPlan, TempLastAchievers, NewState, NewPlan, NewLastAchievers, Pre),
      % debug_format('[apply_mappings] NewLastAchievers: ~w\n', [NewLastAchievers]),
      true
    );(
      NewPlan = TempPlan,
      NewState = CurrentState,
      functor(HL_Action, ActionNameFull, _), 
      sub_string(ActionNameFull, Value, _, _, '_end'), 
      sub_string(ActionNameFull, _, Value, _, ActionName)
      ->(
        % debug_format('[apply_action_map] Calling add_achievers_end_ll for ~w ~w\n', [ActionName, TempLastAchievers]),
        % debug_format('[apply_mappings] TempPlan: \n'),
        print_list(TempPlan),
        add_achievers_end_ll(ActionName, TempPlan, TempActionLastAchievers, NewActionLastAchievers, Pre),
        % append(TempTempLastAchievers, TempLastAchievers, NewLastAchievers),
        % debug_format('[apply_mappings] NewActionLastAchievers: ~w\n', [NewActionLastAchievers]),
        % (
        %   NewActionLastAchievers = [31,32,33,34,35,36,37,38,39,30,29,19,10,9]
        %   -> (leash(-all), trace)
        %   ;  true
        % ),
        append([Length-HL_Action-NewActionLastAchievers], LastAchievers, NewLastAchievers),
        true
      );(
        append([Length-HL_Action-TempActionLastAchievers], LastAchievers, NewLastAchievers),
        true
      )
    )
  ),
  debug_format('[apply_mappings] Action name: ~w\n', [HL_Action]),
  debug_format('[apply_mappings] Current state: ~w\n', [NewState]),
  debug_format('[apply_mappings] NewPlan: ~w\n', [NewPlan]),
  % (
  %   NewPlan = [19-move_onblock_to_table_end(a1,b1,2,2,0,0),18-release_end(a1),17-release_start(a1),16-move_arm_end(a1,2,2,0,0),15-move_arm_start(a1,2,2,0,0),14-grip_end(a1),13-grip_start(a1),12-move_arm_end(a1,0,0,2,2),11-move_arm_start(a1,0,0,2,2),10-move_onblock_to_table_start(a1,b1,2,2,0,0),9-move_table_to_block_end(a1,b1,b2,1,1,2,2),8-release_end(a1),7-release_start(a1),6-move_arm_end(a1,1,1,2,2),5-move_arm_start(a1,1,1,2,2),4-grip_end(a1),3-grip_start(a1),2-move_arm_end(a1,0,0,1,1),1-move_arm_start(a1,0,0,1,1),0-move_table_to_block_start(a1,b1,b2,1,1,2,2)]
  %   -> (leash(-all), trace)
  %   ;  true
  % ),
  apply_mappings(NewState, Goal, T_HL_Actions, T_HL_Achievers, NewPlan, NewLastAchievers, RetPlan, RetLastAchievers).

apply_mappings(_, _, _, _, _, _, _, _) :-
  debug_format('[apply_mappings] Could not apply mappings\n'),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_action_map([], _IDHLAction, State, Plan, LastAchievers, State, Plan, LastAchievers, _).
apply_action_map([HAction|TActions], IDHLAction, State, Plan, LastAchievers, RetState, RetPlan, RetLastAchievers, Pre) :-
  debug_format('\n~w[apply_action_map] Adding map ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('~w[apply_action_map] found action ~w ~w ~w ~w ~w ~w \n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  (
    is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify)
    -> (
      debug_format('~w[apply_action_map] applicable ~w\n', [Pre, HAction]),
      length(Plan, Length),

      % Find last achievers
      debug_format('~w[apply_action_map] Finding last achievers for ~w-~w\n', [Pre, Length, HAction]),
      % last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, Achievers),
      Achievers = [],
      append([IDHLAction], Achievers, TempLastAchievers),
      debug_format('~w[apply_action_map] Achievers ~w\n', [Pre, TempLastAchievers]),
      (
        functor(HAction, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
        ->(
          debug_format('~w[apply_action_map] Calling add_achievers_end_ll for ~w ~w ~w\n', [Pre, ActionName, Plan, TempLastAchievers]),
          % Create NewPre by concatenating Pre and a tab
          format(atom(NewPre), '\t~w', [Pre]),
          add_achievers_end_ll(ActionName, Plan, TempLastAchievers, TempTempLastAchievers, NewPre),
          true
        );( 
          append([], TempLastAchievers, TempTempLastAchievers)  
        )
      ),
      debug_format('~w[apply_action_map] TempLastAchievers: ~w\n', [Pre, TempLastAchievers]),
      % NewPre is the concatenation of Pre and a tab
      format(atom(NewPre), '\t~w', [Pre]),
      add_no_mapping_achievers(Length-HAction, Plan, IDHLAction, TempTempLastAchievers, TempTempTempLastAchievers, NewPre),
      % TempTempTempLastAchievers = TempLastAchievers,
      debug_format('~w[apply_action_map] Last achievers: ~w\n', [Pre, TempTempTempLastAchievers]),
      append([Length-HAction-TempTempTempLastAchievers], LastAchievers, NewLastAchievers),

      stack(Length-HAction, Plan, NewPlan),
      debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
      % Change state.
      change_state(State, Effects, NewState),
      debug_format('~w[apply_action_map] changed to ~w\n', [Pre, NewState]),
      format(atom(NewPre), '\t~w', [Pre]),
      (
        mapping(HAction, Mappings)
        ->(
          debug_format('~w[apply_action_map] Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
          append(Mappings, TActions, NewActionList),
          apply_action_map(NewActionList, Length, NewState, NewPlan, NewLastAchievers, RetState, RetPlan, RetLastAchievers, NewPre)
        )
        ; (
          debug_format('~w[apply_action_map] No mappings for action ~w\n', [Pre, HAction]),
          debug_format('~w[apply_action_map] Applying next action\n', [Pre]), 
          debug_format('~w[apply_action_map] TActions: ~w\n', [Pre, TActions]),
          debug_format('~w[apply_action_map] IDHLAction: ~w\n', [Pre, IDHLAction]),
          debug_format('~w[apply_action_map] NewState: ~w\n', [Pre, NewState]),
          debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
          debug_format('~w[apply_action_map] NewLastAchievers: ~w\n', [Pre, NewLastAchievers]),
          apply_action_map(TActions, IDHLAction, NewState, NewPlan, NewLastAchievers, RetState, RetPlan, RetLastAchievers, Pre)
        )
      ),
      true
    );(
      debug_format('~w[apply_action_map] Not applicable ~w\n', [Pre, HAction]),
      why_not_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
      fail
    )
  ),
  true
  .

apply_action_map(_, _, _, _, _, _, _, _, _) :-
  debug_format('[apply_action_map] Could not apply action map\n'),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function generates a plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plan(Init, Goal, Plan, LastAchievers) :-
  debug_format('Checking if the initial state is the goal state ~w~w\n', [Init, Goal]),
  \+equal_set(Init, Goal),
  debug_format('Generating the high-level temporal plan from ~w to ~w\n', [Init, Goal]),
  (
    generate_plan_hl(Init, Goal, [], [], [], 6, HL_Plan, HL_Achievers)
    ->(
      % Print information on the high-level part
      debug_format('High-level plan generated\n~w\n', [HL_Plan]),
      debug_format('High-level achievers found\n'),
      print_list(HL_Achievers),
      extract_adj_matrix_actions(HL_Achievers, AdjMatrix, Actions),
      debug_format('Adjacency matrix:\n'),
      print_list(AdjMatrix),
      print_list(Actions),
      % Find the low-level plan
      % debug_format('Applying the mappings to obtain the low-level temporal plan from ~w to ~w\n', [Init, Goal]),
      % findall(Mapping, mapping(Mapping, _), Mappings),
      % debug_format('Mappings available: ~w\n', [Mappings]),
      % reverse(HL_Plan, HL_PlanReversed),
      % reverse(HL_Achievers, HL_AchieversReversed),
      % (
      %   apply_mappings(Init, Goal, HL_PlanReversed, HL_AchieversReversed, Plan, LL_Achievers)
      %   ->(
      %     debug_format('Plan generated~w\n', [Plan]),
      %     debug_format('LL achievers:\n'),
      %     print_list(LL_Achievers),
      %     clean_achievers(LL_Achievers, LastAchievers)
      %   );(
      %     format('Could not apply mappings\n'), fail
      %   )
      % ),
      true
    );(
      format('Could not generate a HL plan\n'), fail
    )
  ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function generates a HL plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plan_hl(State, Goal, _Been_list, Plan, LastAchievers, _MaxDepth, Plan, LastAchievers) :-
  equal_set(State, Goal).
generate_plan_hl(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
  \+equal_set(State, Goal),
  length(Plan, Length), Length < MaxDepth,
  debug_format('\n\nCurrent plan: ~w\n', [Plan]),

  % choose_action(State, Goal, Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('Checking action ~w for state: ~w\n', [Name, State]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Goal),
  debug_format('Action ~w is applicable for state ~w\n', [Name, State]),

  change_state(State, Effects, NewState),
  debug_format('Obtained new state ~w\n', [NewState]),
  
  \+member_state(NewState, Been_list),

  % % Find last achievers
  debug_format('Finding last achievers for ~w ~w ~w ~w\n', [Name, PreconditionsT, PreconditionsF, Plan]),
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, Plan, Achievers),
  (
    functor(Name, ActionNameFull, _), sub_string(ActionNameFull, Value, _, _, '_end'), sub_string(ActionNameFull, _, Value, _, ActionName) 
    ->(
      debug_format('Adding achievers for ~w ~w ~w ~w\n', [Name, ActionNameFull, ActionName, Value]),
      add_achievers_end(ActionName, Plan, Achievers, TempLastAchievers, '\t')
    );(
      append([], Achievers, TempLastAchievers)
    )
  ),
  append([Length-Name-TempLastAchievers], LastAchievers, NewLastAchievers),
  debug_format('Last achievers: ~w\n', [TempLastAchievers]),
  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  debug_format('New state: ~w\n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  debug_format('New plan: ~w\n', [NewPlan]),
  generate_plan_hl(NewState, Goal, NewBeen_list, NewPlan, NewLastAchievers, MaxDepth, FinalPlan, FinalLastAchievers),
  true.

generate_plan_hl(State, Goal, Been_list, Plan, LastAchievers, MaxDepth, FinalPlan, FinalLastAchievers) :-
  fail. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function takes a list of achievers, removes the duplicates and reverses the list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_achievers(LastAchievers, RetLastAchievers) :-
  clean_achievers(LastAchievers, [], TmpLastAchievers),
  reverse(TmpLastAchievers, [], RetLastAchievers).

clean_achievers([], LastAchievers, LastAchievers).
clean_achievers([ID-Action-Achievers|TActions], TempLastAchievers, RetLastAchievers) :-
  move_to_set(Achievers, NewAchievers),
  debug_format('Cleaned achievers for ~w ~w ~w from ~w\n', [ID, Action, NewAchievers, Achievers]),
  append([ID-Action-NewAchievers], TempLastAchievers, NewLastAchievers),
  clean_achievers(TActions, NewLastAchievers, RetLastAchievers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function removes duplicates from a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_to_set(Ach, Ret) :-
  move_to_set(Ach, [], TmpRet),
  reverse(TmpRet, Ret).
  
move_to_set([], R, R).
move_to_set([H|T], Temp, Ret) :-
  member(H, Temp),
  move_to_set(T, Temp, Ret).
move_to_set([H|T], Temp, Ret) :-
  \+member(H, Temp),
  append([H], Temp, NewTemp),
  move_to_set(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function reverses a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse([], Ret, Ret).
reverse([H|T], Temp, Ret) :-
  append([H], Temp, NewTemp),
  reverse(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plan(Actions, AdjMatrix, TTActionList, Resources, ActionXResources) :-
  % disable_debug,
  enable_debug,
  init_state(Init),
  goal_state(Goal),
  % trace(apply_action_map),
  debug_format('Planning from: ~w to: ~w~n', [Init, Goal]),
  % leash(-all), etrace,
  % extract_hl_goal(Goal, HLGoal),
  generate_plan(Init, Goal, TOActions, LastAchievers),
  debug_format('Total-order plan: ~n'),
  reverse(TOActions, TOActionsReversed),
  print_list(TOActionsReversed),
  debug_format('Last achievers: ~n'),
  reverse(LastAchievers, LastAchieversReversed),
  print_list(LastAchieversReversed),
  nl,nl,nl,
  
  extract_adj_matrix_actions(LastAchievers, AdjMatrix, Actions),
  debug_format('Adjacency matrix:~n'),
  print_list(AdjMatrix),
  debug_format('Actions:~n'),
  print_list(Actions),
  nl,nl,nl,

  extract_tt_action_list(Actions, TTActionList),
  debug_format('Time-triggered actions:~n'),
  print_list(TTActionList),
  nl,nl,nl,

  extract_resources_number(Resources),
  debug_format('Resources:~n'),
  print_list(Resources),
  nl,nl,nl,

  extract_resources_per_action(TTActionList, Resources, ActionXResources),
  debug_format('Resources per action:~n'),
  print_list(ActionXResources),
  nl,nl,nl,

  format('Finished planning.~n'),

  true.

plan(Actions, AdjMatrix, TTActionList, Resources, ActionXResources) :-
  init_state(Init),
  goal_state(Goal),
  % trace(apply_action_map),
  debug_format('Planning from: ~w to: ~w~n', [Init, Goal]),
  % leash(-all), etrace,
  % extract_hl_goal(Goal, HLGoal),
  \+generate_plan(Init, Goal, TOActions, LastAchievers),
  format('Could not generate TO plan').

plan :-
  plan(_, _, _, _, _).  

