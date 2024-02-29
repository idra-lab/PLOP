import networkx as nx
from matplotlib import pyplot as plt
import re

import pydot
from networkx.drawing.nx_pydot import graphviz_layout


ugraph = """
[
    activate_register_end()-[activate_register_start(),make_coffee_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    activate_register_start()-[make_coffee_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    queue_order_end()-[queue_order_start(),make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    queue_order_start()-[make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    make_coffee_end(jack,espresso,to_go)-[make_coffee_start(jack,espresso,to_go)],
    make_coffee_start(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2506,cup_dispender),move_base_start(_2270,_2506,cup_dispender),payment_end(jack,espresso,to_go),move_arm_end(_2270,4.75,-1,5)],
    move_base_end(_2270,_2272,service_area)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2272,service_area),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    move_base_end(_2270,_2330,register)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2330,register),payment_end(jack,espresso,to_go)],
    move_base_end(_2270,_2506,cup_dispender)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2506,cup_dispender)],
    move_base_end(_2270,_2782,coffee_machine)-[move_base_start(_2270,_2782,coffee_machine)],
    move_base_start(_2270,_2272,service_area)-[make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    move_base_start(_2270,_2330,register)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),payment_end(jack,espresso,to_go)],
    move_base_start(_2270,_2506,cup_dispender)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register)],
    move_base_start(_2270,_2782,coffee_machine)-[move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register),move_base_end(_2270,_2506,cup_dispender),move_arm_end(_2270,4.75,-1,5)],
    order_end(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),order_start(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    order_start(jack,espresso,to_go)-[queue_order_end(),queue_order_start(),make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),move_base_start(_2270,_2272,service_area),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    payment_end(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),payment_start(jack,espresso,to_go)],
    payment_start(jack,espresso,to_go)-[activate_register_end(),activate_register_start(),make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2330,register),move_base_start(_2270,_2330,register),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],
    serve_end(jack,espresso,to_go)-[serve_start(jack,espresso,to_go)],
    serve_start(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2782,coffee_machine),move_base_start(_2270,_2782,coffee_machine),move_arm_start(_2270,3,1,5)],
    move_arm_end(_2270,4.75,-1,5)-[make_coffee_end(jack,espresso,to_go)],
    move_arm_start(_2270,3,1,5)-[move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register),move_base_end(_2270,_2506,cup_dispender),move_base_end(_2270,_2782,coffee_machine),move_arm_end(_2270,4.75,-1,5)]
]
"""

# ugraph = "[activate_register_end()-[activate_register_start(),make_coffee_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],activate_register_start()-[make_coffee_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],queue_order_end()-[queue_order_start(),make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],queue_order_start()-[make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],make_coffee_end(jack,espresso,to_go)-[],make_coffee_start(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2506,cup_dispender),move_base_start(_2270,_2506,cup_dispender),move_arm_end(_2270,4.75,-1,5)],move_base_end(_2270,_2272,service_area)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2272,service_area),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],move_base_end(_2270,_2330,register)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2330,register),payment_end(jack,espresso,to_go)],move_base_end(_2270,_2506,cup_dispender)-[make_coffee_end(jack,espresso,to_go),move_base_start(_2270,_2506,cup_dispender)],move_base_end(_2270,_2782,coffee_machine)-[move_base_start(_2270,_2782,coffee_machine)],move_base_start(_2270,_2272,service_area)-[make_coffee_end(jack,espresso,to_go),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],move_base_start(_2270,_2330,register)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),payment_end(jack,espresso,to_go)],move_base_start(_2270,_2506,cup_dispender)-[make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register)],move_base_start(_2270,_2782,coffee_machine)-[move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register),move_base_end(_2270,_2506,cup_dispender),move_arm_end(_2270,4.75,-1,5)],order_end(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],order_start(jack,espresso,to_go)-[queue_order_end(),queue_order_start(),make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2272,service_area),move_base_start(_2270,_2272,service_area),order_end(jack,espresso,to_go),payment_end(jack,espresso,to_go)],payment_end(jack,espresso,to_go)-[make_coffee_end(jack,espresso,to_go)],payment_start(jack,espresso,to_go)-[activate_register_end(),activate_register_start(),make_coffee_end(jack,espresso,to_go),move_base_end(_2270,_2330,register),move_base_start(_2270,_2330,register),payment_end(jack,espresso,to_go)],serve_start(jack,espresso,to_go)-[move_base_end(_2270,_2782,coffee_machine),move_base_start(_2270,_2782,coffee_machine),move_arm_start(_2270,3,1,5)],move_arm_end(_2270,4.75,-1,5)-[make_coffee_end(jack,espresso,to_go)],move_arm_start(_2270,3,1,5)-[move_base_end(_2270,_2272,service_area),move_base_end(_2270,_2330,register),move_base_end(_2270,_2506,cup_dispender),move_base_end(_2270,_2782,coffee_machine),move_arm_end(_2270,4.75,-1,5)]]"


def drawBokeh(Graph):
    from bokeh.plotting import figure, show, output_file
    from bokeh.models import ColumnDataSource, HTMLLabelSet
    min_node_size = 70000  # Minimum node size
    max_node_size = 10000  # Maximum node size

    # Create a Bokeh plot
    plot = figure(
        width=1600, height=1200
    )

    # Create a layout using from_networkx
    layout = nx.nx_agraph.graphviz_layout(Graph, prog="dot")  # You can use other layout algorithms as well
    # layout = nx.spring_layout(G)

    # Extract edge endpoints
    edge_start = [edge[0] for edge in Graph.edges]
    edge_end = [edge[1] for edge in Graph.edges]

    # Extract node attributes
    Xs = []
    Ys = []
    node_labels = []
    label_lengths = []
    node_sizes = []

    for node_id in range(len(Graph.nodes)):
        node = list(Graph.nodes)[node_id]
        pos = list(layout.values())[node_id]
        Xs.append(pos[0])
        Ys.append(pos[1])
        node_labels.append(node)#Graph.nodes[node].get('label', ''))
        label_lengths.append(len(node_labels[-1]))

    for labelLen in label_lengths:
        node_sizes.append((labelLen / max_node_size) * min_node_size)

    # Create a data source for nodes
    node_source = ColumnDataSource(data=dict(
        x=Xs,
        y=Ys,
        labels=node_labels,
        sizes=node_sizes,
    ))

    # Create a data source for edges
    edge_source = ColumnDataSource(data=dict(
        xs=[[layout[start][0], layout[end][0]] for start, end in zip(edge_start, edge_end)],
        ys=[[layout[start][1], layout[end][1]] for start, end in zip(edge_start, edge_end)]
    ))

    # Customize node rendering with dynamically calculated sizes
    nodes = plot.ellipse('x', 'y', width='sizes', height=7, source=node_source, color='white')

    # Display 'label' attribute inside the nodes using LabelSet
    labels = HTMLLabelSet(x='x', y='y', text='labels', source=node_source, level='glyph',
                            text_align='center', text_baseline='middle', text_color='black')

    # Customize edge rendering
    edges = plot.multi_line('xs', 'ys', source=edge_source, line_color='#AAAAAA', line_width=1)

    # Add tooltips to display 'title' attributes for nodes
    # hover = HoverTool()
    # hover.tooltips = [("Label", "@labels")]

    # Remove tooltips for edges
    edges.hover_glyph = None
    edges.nonselection_glyph = None

    # plot.add_tools(hover)

    # Hide Bokeh axes and grid
    plot.axis.visible = False
    plot.grid.visible = False

    # Add nodes, labels, and edge to the same renderers list to ensure proper layering
    plot.renderers.extend([nodes, labels, edges])

    # Show the plot
    output_file(filename="planner.html")
    show(plot)

def draw_graph(graph_str:str) -> None:
    """
    Takes a ugraph from Prolog and draws it using networkx
    """
    graph = nx.DiGraph()
    nodes_edges = graph_str.split('],')
    for node_edges in nodes_edges:
        node_edges = node_edges.strip()
        if node_edges:
            try:
                node, edges = node_edges.split('-[')
            except Exception as e:
                print(node_edges)
                raise e
            node = "".join(node.strip('[]').split())
            edges = "".join(edges.strip('[]').split()).replace("]","")
            
            graph.add_nodes_from([node])
            for item in re.finditer(r"(?P<action>[_a-zA-Z0-9]+\([_a-zA-Z0-9,]*\))", edges):
                action = item.groupdict()['action']
                print(action)
                graph.add_edge(node, action)
    # pos = graphviz_layout(graph, prog="dot")
    # nx.draw(graph, pos=pos, with_labels=True)
    # plt.show()
    drawBokeh(graph)


if __name__ == '__main__':
    draw_graph(ugraph)

