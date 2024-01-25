:- include('adts.pl').
:- include('actions.pl').
:- include('kb.pl').

:- discontiguous action/6.

ground_g([]).
ground_g([_H|T]) :-
	% assertz(H),
	ground_g(T).


achiever([HP|_TP], [add(HP)|_TE], _).
achiever([_HP|TP], [], E) :-
	achiever(TP, E, []).
achiever([HP|TP], [HE|TE], U) :-
	append([HE], U, NewU),
	achiever([HP|TP], TE, NewU).

partial_order(_PT, [], [], 0).

partial_order(PT, [AH|AT], [I|Times], I) :-
  action(AH, _, _, _, _, E),
  achiever(PT, E, []),
  NewI is I-1,
  partial_order(PT, AT, Times, NewI).

partial_order(PT, [AH|AT], Times, I) :-
  action(AH, _, _, _, _, E),
  \+achiever(PT, E, []),
  NewI is I-1,
  partial_order(PT, AT, Times, NewI).

partial_order(PT, Actions, Times) :-
  length(Actions, NActions),
  N is NActions,
  partial_order(PT, Actions, Times, N).

verify([]).
verify([H|T]) :-
	H,
	verify(T),
	H.

plan(State, Goal, _, Actions, _MaxDepth, Actions) :- 	
	equal_set(State, Goal),
	write('actions are'), nl,
	reverse_print_actions(Actions),
	% reverse_print_actions_times(Actions, Times),
	true
	.

plan(State, Goal, Been_list, Actions, MaxDepth, _RetActions) :- 	
	length(Actions, Len), Len < MaxDepth,
	action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
	verify(Verify),
	conditions_met(PreconditionsT, State),
	conditions_not_met(PreconditionsF, State),
	conditions_not_met(FinalConditionsF, Goal),
	change_state(State, Effects, Child_state),
	testPlan(Child_state, Goal, Been_list, Name, Actions, MaxDepth)
	.	

testPlan(Child_state, Goal, Been_list, Name, Actions, MaxDepth) :-
	\+equal_set(Child_state, Goal),
	not(member_state(Child_state, Been_list)),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	plan(Child_state, Goal, New_been_list, New_actions, MaxDepth, New_actions).

testPlan(Child_state, Goal, Been_list, Name, Actions, MaxDepth) :-
	equal_set(Child_state, Goal),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	plan(Child_state, Goal, New_been_list, New_actions, MaxDepth, New_actions).

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
	conditions_not_met(T, S)
	.

member_state(S, [H|_]) :- 	equal_set(S, H).
member_state(S, [_|T]) :- 	member_state(S, T).


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

% test_plan_result(R, _S, _G, _StateList, _Actions, _MaxTime) :-
% 	R.
% test_plan_result(R, S, G, _StateList, _Actions, MaxTime) :-
% 	\+ R,
% 	try_plan(S, G, [S], [], [], MaxTime, []).

% try_plan(S, G, StateList, Actions, Times, MaxTime, RetActions) :-
% 	NewMaxTime is MaxTime + 10, 
% 	write("Testing for "), write(NewMaxTime), nl,
% 	NewMaxTime < 50,
% 	(plan(S, G, StateList, Actions, NewMaxTime) -> Res = true; Res = false),
% 	test_plan_result(Res, S, G, StateList, Actions, NewMaxTime).

go(S, G, Actions, _Times) :- 
	plan(S, G, [S], [], 6, Actions).

% go(S, G, Actions, Times, MaxDepth) :- 
% 	retractall(ontable(_, _, _)),
% 	retractall(on(_, _, _, _)),
% 	retractall(clear(_)),
% 	retractall(available(_)),
% 	ground_g(G), 
% 	plan(S, G, [S], [], [], MaxDepth, Actions).
% 	try_plan(S, G, [S], [], [], 0, Actions).


