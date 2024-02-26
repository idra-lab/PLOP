:- ensure_loaded('utility/utility.pl').
:- use_module(library(ugraphs)).

graph(G) :-
    vertices_edges_to_ugraph([], [1-3,1-5,2-4,4-5], G).

print_graph() :-
    graph(G),
    edges(G, Edges),
    vertices(G, Nodes),    
    format('Edges: ~w~n', [Edges]),
    format('Nodes: ~w~n', [Nodes]),
    format('Graph: ~w~n', [G]),
    true.

my_append([], _Cost, List, List) :-
my_append(Nodes, Cost, OldList, NewList) :-
    append(OldList, [Nodes-Cost], NewList).

dijkstra(_Graph, Dest, Dest, _, Path, Path).
dijkstra(_Graph, _Curr, _Dest, [], _P, _RetP) :- fail.
dijkstra(Graph, Curr, Dest, OPEN, Cost, Path, RetPath) :-
    neighbors(Curr, Graph, Neighbors),
    length(Neighbors, L), L > 0,
    ord_union(OPEN, Neighbors, NewOpen),

    NewCost is Cost,
    append(OPEN, Neighbors, [Next|NewOpen]),
    
    dijkstra(Graph, H, Dest, NewOpen, [Next|Path], RetPath).

dijkstra(Graph, Start, Dest, Path) :-
    dijkstra(Graph, Start, Dest, [Start], 0, [], Path).




