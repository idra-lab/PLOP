:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              TOTAL LL ORDER                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This functions allows to take a high-level plan (total order) and extract 
% the corresponding low-level actions from the mappings providing a simple
% 1 to 1 mapping.

extract_mapping(Action, TotalOrder, TotalOrder) :-
  functor(Action, ActionName, _),
  \+sub_string(ActionName, _, _, _, '_start').

extract_mapping(Action, TotalOrder, NewTotalOrder) :-
  mappings(Action, LLActions),
  print_list(LLActions, "\t"),
  append(TotalOrder, LLActions, NewTotalOrder).


total_ll_order([], TotalOrder, _I, TotalOrder).
total_ll_order([HA|TA], TotalOrder, I, NewT) :-
  format('[~w] ~w~n', [I, HA]),
  NewI is I + 1,
  append(TotalOrder, [HA], NewTotalOrder),
  extract_mapping(HA, NewTotalOrder, NewNewTotalOrder),
  total_ll_order(TA, NewNewTotalOrder, NewI, NewT),
  true.

total_ll_plan(Plan, NewT):- 
  total_ll_order(Plan, [], 0, NewT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             PARTIAL LL ORDER                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_add_edges(Graph, Edge, NewGraph) :-
  format('Adding edge ~w~n', [Edge]),
  add_edges(Graph, Edge, NewGraph).

add_end_action_edges(_Action, _LastStartHLAction, [], Graph, Graph).
add_end_action_edges(_Action, LastStartHLAction, [LastStartHLAction|_T], Graph, Graph).
add_end_action_edges(Action, LastStartHLAction, [HUsedActions|TUsedActions], Graph, RetGraph):-
  my_add_edges(Graph, [Action-HUsedActions], NewLLGraph),
  add_end_action_edges(Action, LastStartHLAction, TUsedActions, NewLLGraph, RetGraph).


% If it is a HL action, then for sure it's in the graph
partial_ll_order(_Action, _PT, [], Graph, Graph) :-
  write('Finshed partial_ll_order'), nl.

partial_ll_order(Action, PT, [HA|TA], Graph, RetGraph):-
  achiever(PT, HA),
  format('Action ~w achiever of ~w~n', [HA, Action]),
  my_add_edges(Graph, [Action-HA], NewGraph),
  partial_ll_order(Action, PT, TA, NewGraph, RetGraph).

partial_ll_order(Action, PT, [HA|TA], Graph, RetGraph):-
  \+achiever(PT, HA),
  format('Action ~w NOT achiever of ~w, checking ~w~n', [HA, Action, TA]),
  partial_ll_order(Action, PT, TA, Graph, RetGraph).




% When all the actions have been checked, then we have finished and we have the graph
partial_ll_plan([], _, _LastStartHLAction, LLGraph, LLGraph, _I):-
  format('Finished partial_ll_plan correctly~n').
% partial_ll_plan(_, _, _LastStartHLAction, LLGraph, LLGraph, 25).

% If the action is a low-level action, then we need to add it to the graph, then
% add an edge from the last high-level action, and then check its achievers.
partial_ll_plan([Action|TOActions], UsedActions, LastStartHLAction, LLGraph, RetGraph, HL_I, LL_I) :-
  format('Testing action ~w as ll~n', [Action]),
  trace,
  ll_action(Action, PT, _, _, _, _),
  format('~n~w Checking achievers for ll action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [tta(Action, HL_I, LL_I)], NewLLGraph),
  my_add_edges(NewLLGraph, [tta(Action, HL_I, LL_I)-LastStartHLAction], NewNewLLGraph),
  format('Added edge ~w ~w getting ~w ~n', [tta(Action, I), LastStartHLAction, NewNewLLGraph]),
  partial_ll_order(tta(Action, HL_I, LL_), PT, UsedActions, NewNewLLGraph, NewNewNewLLGraph),
  format('Completed partial order ~n'),
  append(UsedActions, [tta(Action, HL_I, LL_I)], NewUsedActions),
  format('Added actions to used ones ~n'),
  NewLL_I is LL_I+1,
  partial_ll_plan(TOActions, NewUsedActions, LastStartHLAction, NewNewNewLLGraph, RetGraph, HL_I, NewLL_I).

% When the action is a high-level action AND it is a start action, then we need
% to add it to the graph and check its achievers. Also we shall store the action
% since all the low-level actions that follow will depend on it until the next
% high-level end action is found.
partial_ll_plan([Action|TOActions], UsedActions, _LastStartHLAction, HLGraph, LLGraph, RetGraph, HL_I, LL_I) :-
  format('Testing action ~w as hl start~n', [Action]),
  action(Action, PT, _, _, _, _),
  functor(Action, ActionName, _),
  sub_string(ActionName, _, _, _, '_start'),
  format('~n~w Checking achievers for hl start action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [tta(Action, HL_I, LL_I)], NewLLGraph),
  % add_HL_dependencies(NewLLGraph, Action, I, NewNewLLGraph),
  append(UsedActions, [tta(Action, HL_I, LL_I)], NewUsedActions),
  NewHL_I is HL_I+1,
  partial_ll_plan(TOActions, NewUsedActions, tta(Action, I), NewLLGraph, RetGraph, NewHL_I, LL_I).

% When the action is a high-level action AND it is an end action, then we need 
% to add it to the graph, add a dependency to all the low-level action between 
% this action and the previous start action and check its achievers.
partial_ll_plan([Action|TOActions], UsedActions, LastStartHLAction, HLGraph, LLGraph, RetGraph, HL_I, LL_I) :-
  format('Testing action ~w as hl end~n', [Action]),
  action(Action, PT, _, _, _, _),
  functor(Action, ActionName, _),
  sub_string(ActionName, _, _, _, '_end'),
  format('~n~w checking achievers for hl end action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [tta(Action, HL_I, LL_I)], NewLLGraph),
  reverse(UsedActions, ReversedUsedActions),
  format('Reversed used actions ~w~n', [ReversedUsedActions]),
  add_end_action_edges(tta(Action, HL_I, LL_I), LastStartHLAction, ReversedUsedActions, NewLLGraph, NewNewLLGraph),
  add_HL_dependencies(HLGraph, NewNewLLGraph, tta(Action, HL_I, LL_I), NewNewNewLLGraph),
  append(UsedActions, [tta(Action, HL_I, LL_I)], NewUsedActions),
  NewHL_I is HL_I+1,
  format('Still missing ~w~n', [TOActions]),
  partial_ll_plan(TOActions, NewUsedActions, LastStartHLAction, NewNewLLGraph, RetGraph, NewHL_I, LL_I).

% Wrapper function
partial_ll_plan(LLActions, HLGraph, LLGraph) :-
  write('Starting partial_ll_plan'), nl,
  vertices_edges_to_ugraph([], [], Graph),
  partial_ll_plan(LLActions, [], _, HLGraph, Graph, TmpGraph, 0),
  my_ugraph_union(HLGraph, TmpGraph, LLGraph),
  true.


% So basically to check the partial order we need to check the type of action we
% are dealing with, and then divide the function in two. If it is a high-level
% action we need to set that as a check-point and then check all the low-level 
% that follow keeping in mind that they cannot be executed before the high-level
% action. 
% To set a partial order between low-level actions belonging to different 
% high-level actions, we can produce a high-level partial order and then use it
% as the actual checkpoint. BUT the partial-order only gives us a logical 
% precedence between actions, so it may not be sufficient. I'm still thinking 
% about a graph... Each node would have as connected nodes, those actions onto 
% which it depends. 