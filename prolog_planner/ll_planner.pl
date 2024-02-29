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
  
add_end_action_edges(_Action, [], Graph, Graph).
add_end_action_edges(Action, [HUsedActions|TUsedActions], Graph, RetGraph):-
  add_edges(Graph, [HUsedActions-Action], NewLLGraph),
  add_end_action_edges(Action, TUsedActions, NewLLGraph, RetGraph).






% add_hl_neighbours(LLGraph, _, [], LLGraph).
% add_hl_neighbours(LLGraph, Action, [HNeighbour|TNeighbours], RetGraph):-
%   add_edges(LLGraph, [Action-HNeighbour], NewLLGraph),
%   add_hl_neighbours(NewLLGraph, Action TNeighbours, RetGraph).

% add_hl_neighbours(HLGraph, Action, LLGraph, RetGraph):-
%   neighbours(HLGraph, Action, Neighbours),
%   add_hl_neighbours(LLGraph, Action, Neighbours, RetGraph).






% If it is a HL action, then for sure it's in the graph
partial_ll_order(_Action, _PT, [], Graph, Graph) :-
  write('Finshed partial_ll_order'), nl.

partial_ll_order(Action, PT, [HA|TA], Graph, RetGraph):-
  achiever(PT, HA),
  format('Action ~w achiever of ~w~n', [HA, Action]),
  add_edges(Graph, [Action-HA], NewGraph),
  partial_ll_order(Action, PT, TA, NewGraph, RetGraph).

partial_ll_order(Action, PT, [HA|TA], Graph, RetGraph):-
  \+achiever(PT, HA),
  format('Action ~w NOT achiever of ~w, checking ~w~n', [HA, Action, TA]),
  partial_ll_order(Action, PT, TA, Graph, RetGraph).







% When all the actions have been checked, then we have finished and we have the graph
partial_ll_plan([], _, _LastStartHLAction, LLGraph, LLGraph, _I):-
  format('Finished partial_ll_plan correctly~n',).
% partial_ll_plan(Action, UsedActions, LastStartHLAction, LLGraph, RetGraph, 40).% :- trace.
partial_ll_plan(_, _, _LastStartHLAction, LLGraph, LLGraph, 22).

% If the action is a low-level action, then we need to add it to the graph, then
% add an edge from the last high-level action, and then check its achievers.
partial_ll_plan([Action|TOActions], UsedActions, LastStartHLAction, LLGraph, RetGraph, I) :-
  ll_action(Action, PT, _, _, _, _),
  format('~n~w Checking achievers for ll action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [Action], NewLLGraph),
  add_edges(NewLLGraph, [LastStartHLAction-Action], NewNewLLGraph),
  format('Added edge ~w ~w getting ~w ~n', [LastStartHLAction, Action, NewNewLLGraph]),
  partial_ll_order(Action, PT, UsedActions, NewNewLLGraph, NewNewNewLLGraph),
  format('Completed partial order ~n'),
  append(UsedActions, [Action], NewUsedActions),
  format('Added actions to used ones ~n'),
  NewI is I+1,
  partial_ll_plan(TOActions, NewUsedActions, LastStartHLAction, NewNewNewLLGraph, RetGraph, NewI).

% When the action is a high-level action AND it is a start action, then we need
% to add it to the graph and check its achievers. Also we shall store the action
% since all the low-level actions that follow will depend on it until the next
% high-level end action is found.
partial_ll_plan([Action|TOActions], UsedActions, _LastStartHLAction, LLGraph, RetGraph, I) :-
  action(Action, PT, _, _, _, _),
  functor(Action, ActionName, _),
  sub_string(ActionName, _, _, _, '_start'),
  format('~n~w Checking achievers for hl start action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [Action], NewLLGraph),
  append(UsedActions, [Action], NewUsedActions),
  NewI is I+1,
  partial_ll_plan(TOActions, NewUsedActions, Action, NewLLGraph, RetGraph, NewI).

% When the action is a high-level action AND it is an end action, then we need 
% to add it to the graph, add a dependency to all the low-level action between 
% this action and the previous start action and check its achievers.
partial_ll_plan([Action|TOActions], UsedActions, LastStartHLAction, LLGraph, RetGraph, I) :-
  action(Action, PT, _, _, _, _),
  functor(Action, ActionName, _),
  sub_string(ActionName, _, _, _, '_end'),
  format('~n~w checking achievers for hl end action ~w ~w~n', [I, Action, PT]),
  add_vertices(LLGraph, [Action], NewLLGraph),
  reverse(UsedActions, ReversedUsedActions),
  add_end_action_edges(Action, ReversedUsedActions, NewLLGraph, NewNewLLGraph),
  append(UsedActions, [Action], NewUsedActions),
  NewI is I+1,
  partial_ll_plan(TOActions, NewUsedActions, LastStartHLAction, NewNewLLGraph, RetGraph, NewI).

% Wrapper function
partial_ll_plan(LLActions, HLGraph, LLGraph) :-
  vertices_edges_to_ugraph([], [], Graph),
  partial_ll_plan(LLActions, [], _, Graph, TmpGraph, 0),
  ugraph_union(HLGraph, TmpGraph, LLGraph),
  format('TmpGraph ~w~n', [TmpGraph]),
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