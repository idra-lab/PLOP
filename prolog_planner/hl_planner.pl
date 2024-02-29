:- ensure_loaded('includes.pl').
:- ensure_loaded('utility/utility.pl').

:- use_module(library(ugraphs)).

max_actions(50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             PARTIAL HL ORDER                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO Given an action a_i and an action a_j, a_i is an achiever of a_j also if 
% a_i removes a predicate that blocks a_j from being executed.

partial_hl_order(_Action, _PT, [], Graph, Graph).

partial_hl_order(Action, PT, [HA|TA], Graph, RetGraph) :-
  achiever(PT, HA),
  add_edges(Graph, [Action-HA], NewGraph),
  partial_hl_order(Action, PT, TA, NewGraph, RetGraph).

partial_hl_order(Action, PT, [HA|TA], Graph, RetGraph) :-
  \+achiever(PT, HA),
  partial_hl_order(Action, PT, TA, Graph, RetGraph).

  
partial_hl_plan([], _Actions, Graph, Graph).
partial_hl_plan([Action|TOActions], UsedActions, Graph, RetGraph) :-
  add_vertices(Graph, [Action], NewGraph),
  action(Action, PT, _, _, _, _),
  partial_hl_order(Action, PT, UsedActions, NewGraph, NewNewGraph),
  append(UsedActions, [Action], NewUsedActions),
  partial_hl_plan(TOActions, NewUsedActions, NewNewGraph, RetGraph),
  true.

partial_hl_plan(Actions, RetGraph):-
  vertices_edges_to_ugraph([], [], Graph),
  partial_hl_plan(Actions, [], Graph, RetGraph).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              TOTAL HL ORDER                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_hl_order(State, Goal, _, Actions, _MaxDepth, Actions) :- 	
	equal_set(State, Goal),
	% write('Actions are'), nl,
	% reverse_print_actions(Actions),
	true
	.

total_hl_order(State, Goal, Been_list, Actions, MaxDepth, RetActions) :- 	
	length(Actions, Len), Len < MaxDepth,
	action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
	verify(Verify),
	conditions_met(PreconditionsT, State),
	conditions_not_met(PreconditionsF, State),
	conditions_not_met(FinalConditionsF, Goal),
	change_state(State, Effects, Child_state),
  (
	  (
      \+equal_set(Child_state, Goal),
	    not(member_state(Child_state, Been_list))
    );
    equal_set(Child_state, Goal)
  ),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	total_hl_order(Child_state, Goal, New_been_list, New_actions, MaxDepth, RetActions).

total_hl_plan(Init, Goal, Actions) :-
	\+equal_set(Init, Goal),
	total_hl_order(Init, Goal, [Init], [], 20, Actions).
total_hl_plan(_Init, _Goal, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   TEST                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These functions are used to test the creation of a total order plan
test_hl_to_no_trace :- 
	hl_init(Init),
	hl_goal(Goal),
	total_hl_plan(Init, Goal, _A).

test_hl_to_trace :- 
  leash(-all), 
  trace, 
  test_hl_to_no_trace. 

test_hl_to_small_trace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(partial_hl_order, all),
	trace(achiever, all),
	trace(stack, all),
	trace(test_total_hl_order, all),
	trace(total_hl_order, all),
	test_hl_to_no_trace.

% These functions are used to test the creation of a partial order plan
test_hl_po_no_trace :- 
	hl_init(Init),
	hl_goal(Goal),
  total_hl_plan(Init, Goal, Actions),
	partial_hl_plan(Init, Actions, Graph),
  format('Partial order plan is ~w~n', [Graph]).

test_hl_po_trace :- 
  leash(-all), 
  trace, 
  test_hl_po_no_trace. 

test_hl_po_small_trace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(partial_hl_order, all),
	trace(achiever, all),
	trace(stack, all),
	trace(test_total_hl_order, all),
	trace(total_hl_order, all),
	test_hl_po_no_trace.

