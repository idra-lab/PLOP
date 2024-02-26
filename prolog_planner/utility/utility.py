import networkx as nx
from matplotlib import pyplot as plt

ugraph = """
[
    make_coffee_end(jack,espresso,to_go)-[make_coffee_start(jack,espresso,to_go)],
    make_coffee_start(jack,espresso,to_go)-[payment_end(jack,espresso,to_go)],
    order_end(jack,espresso,to_go)-[order_start(jack,espresso,to_go)],
    order_start(jack,espresso,to_go)-[],
    payment_end(jack,espresso,to_go)-[payment_start(jack,espresso,to_go)],
    payment_start(jack,espresso,to_go)-[order_end(jack,espresso,to_go)],
    serve_end(jack,espresso,to_go)-[serve_start(jack,espresso,to_go)],
    serve_start(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go)]
]
"""

def draw_graph(graph_str:str) -> None:
    """
    Takes a ugraph from Prolog and draws it using networkx
    """
    graph = nx.DiGraph()
    nodes_edges = graph_str.split('],')
    for node_edges in nodes_edges:
        node_edges = node_edges.strip()
        if node_edges:
            node, edges = node_edges.split('-')
            node = "".join(node.strip('[]').split())
            edges = "".join(edges.strip('[]').split()).replace("]","")
            
            graph.add_nodes_from([node])
            graph.add_edges_from([(node,edges)])
    nx.draw(graph, with_labels=True, font_weight='bold')
    plt.show()



if __name__ == '__main__':
    draw_graph(ugraph)