import networkx as nx
import matplotlib.pyplot as plt
from copy import deepcopy

class Graph(nx.DiGraph):
    def __init__(self, nodes, edges):
        super().__init__()
        self.add_nodes_from(nodes)
        self.add_edges_from(edges)

    def __init__(self, edges=[]):
        super().__init__()
        for (node1, node2) in edges:
            self.add_nodes_from([node1, node2])
        self.add_edges_from(edges)

    def __remove_redundant_edges(self):
        """
        Removes redundant edges from the graph
        """
        ret = deepcopy(self)
        edges = deepcopy(ret.edges)
        for node1, node2 in edges:
            ret.remove_edge(node1, node2)
            if not nx.has_path(ret, node1, node2):
                ret.add_edge(node1, node2)
        return ret

    def __str__(self) -> str:
        """
        @return Returns a string representation of the graph as a ugraph
        """
        ret = ""
        for node_id, node in enumerate(self.nodes):
            ret += f"{node}-["
            for edge in self.edges(node):
                print(f"\t{edge}")
                for edge_id, edge in enumerate(self.edges(node)):
                    ret += f"{edge}"
                    if edge != len(self.edges(node))-1:
                        ret += ", "
            if node_id != len(self.nodes)-1:
                ret += "], "
            else:
                ret += "]"
        return 

    def draw(self, mode="nx", **kwargs):
        if mode == "nx":
            self.__draw_nx(**kwargs)
        elif mode == "pyvis":
            self.__draw_pyvis(**kwargs)
        elif mode == "nx_bokeh":
            self.__draw_nx_bokeh(**kwargs)
        else:
            raise ValueError(f"Invalid mode {mode}")

    def __draw_nx_bokeh(self, bokeh=False):
        import bokeh
        import pydot
        from networkx.drawing.nx_pydot import graphviz_layout
        pass

    def __draw_pyvis(self, title="nx.html", height="2100px", width="3000px", notebook=False, open_browser=False, rm_redundant=True):
        from pyvis.network import Network

        if rm_redundant:
            graph = self.__remove_redundant_edges()
        else:
            graph = self
        net = Network(height=height, width=width, directed=True)
        net.from_nx(graph)
        net.toggle_physics(False)
        net.show_buttons()
        net.write_html(title, notebook=notebook, open_browser=open_browser)


        

