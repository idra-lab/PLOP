from copy import deepcopy

import matplotlib.pyplot as plt
import networkx as nx

NODES_POS = {
    0: {"x": 2, "y": 5},
    1: {"x": 0, "y": 0},
    2: {"x": 1, "y": 0},
    3: {"x": 1, "y": -4},
    4: {"x": 1, "y": -3},
    5: {"x": 1, "y": -2},
    6: {"x": 1, "y": -1},
    7: {"x": 1, "y": 1},
    8: {"x": 1, "y": 2},
    9: {"x": 1, "y": 3},
    10: {"x": 1, "y": 4},
    11: {"x": 3, "y": 0},
    12: {"x": 4, "y": 0},
    13: {"x": 5, "y": 0},
    14: {"x": 6, "y": 0},
    15: {"x": 7, "y": -4},
    16: {"x": 7, "y": -3},
    17: {"x": 7, "y": -2},
    18: {"x": 7, "y": -1},
    19: {"x": 7, "y": 1},
    20: {"x": 7, "y": 2},
    21: {"x": 7, "y": 3},
    22: {"x": 7, "y": 4},
    23: {"x": 8, "y": 0},
    24: {"x": 9, "y": 0},
    25: {"x": 10, "y": 0},
    26: {"x": 11, "y": 0},
    27: {"x": 12, "y": -4},
    28: {"x": 12, "y": -3},
    29: {"x": 12, "y": -2},
    30: {"x": 12, "y": -1},
    31: {"x": 12, "y": 1},
    32: {"x": 12, "y": 2},
    33: {"x": 12, "y": 3},
    34: {"x": 12, "y": 4},
    35: {"x": 13, "y": 0},
    36: {"x": 14, "y": 0},
    37: {"x": 7, "y": -5},
}


class Graph(nx.DiGraph):
    def __init__(self, nodes, edges):
        super().__init__()
        self.add_nodes_from(nodes)
        self.add_edges_from(edges)

    def __init__(self, edges=[]):
        super().__init__()
        # for (node1, node2) in edges:
        #     if node1 not in self.nodes:
        #         self.add_node(str(node1))
        #     if node2 not in self.nodes:
        #         self.add_node(str(node2))
        #     self.add_edge(str(node1), str(node2))
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
    
    def find_cycle(self, node) -> bool:
        """
        Find a cycle in the graph starting from a node

        Args:
            node (int): The node to start the search from

        Returns:
            bool: True if a cycle is found, False otherwise
        """
        try:
            nx.find_cycle(self, node)
        except nx.exception.NetworkXNoCycle:
            return False
        return True

    def __str__(self) -> str:
        """
        Returns:
            a string representation of the graph as a ugraph
        """
        ret = ""
        for node_id, node in enumerate(self.nodes):
            ret += f"{node}-["
            for edge in self.edges(node):
                print(f"\t{edge}")
                for edge_id, edge in enumerate(self.edges(node)):
                    ret += f"{edge}"
                    if edge != len(self.edges(node)) - 1:
                        ret += ", "
            if node_id != len(self.nodes) - 1:
                ret += "], "
            else:
                ret += "]"
        return ret

    def draw(self, mode="pyvis", **kwargs):
        if mode == "nx":
            self.__draw_nx(**kwargs)
        elif mode == "pyvis":
            self.__draw_pyvis(**kwargs)
        elif mode == "nx_bokeh":
            self.__draw_nx_bokeh(**kwargs)
        else:
            raise ValueError(f"Invalid mode {mode}")

    def __draw_nx(self, **kwargs):
        pass

    def __draw_nx_bokeh(self, bokeh=False):
        import bokeh
        import pydot
        from networkx.drawing.nx_pydot import graphviz_layout

        pass

    def __draw_pyvis(
        self,
        title="nx.html",
        height="2100px",
        width="3000px",
        notebook=False,
        open_browser=False,
        rm_redundant=True,
    ):
        from pyvis.network import Network

        if rm_redundant:
            graph = self.__remove_redundant_edges()
        else:
            graph = self
        net = Network(height=height, width=width, directed=True)
        net.from_nx(graph)

        # for node in NODES_POS.keys():
        #     net.get_node(node)["x"] = NODES_POS[node]["x"] * 100
        #     net.get_node(node)["y"] = -NODES_POS[node]["y"] * 100
        #     net.get_node(node)["label"] = str(node)

        net.toggle_physics(False)
        net.show_buttons()
        net.set_options(
            """
var options = {
  "configure": {
    "enabled": true
  },
  "nodes": {
    "borderWidth": null,
    "borderWidthSelected": null,
    "opacity": null,
    "size": null
  },
  "edges": {
    "color": {
      "inherit": true
    },
    "selectionWidth": 3.5,
    "selfReferenceSize": null,
    "selfReference": {
      "angle": 0.7853981633974483
    },
    "smooth": false
  },
  "physics": {
    "enabled": false,
    "minVelocity": 0.75
  }
}
        """
        )
        net.write_html(title, notebook=notebook, open_browser=open_browser)
