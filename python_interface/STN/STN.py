import networkx as nx
import matplotlib.pyplot as plt

try:
    from python_interface.utility.Graph.Graph import Graph
except:
    from utility.Graph.Graph import Graph


class SimpTempNet(Graph):
    def __init__(self, actions, times):
        super(SimpTempNet, self).__init__()

        # Add nodes
        super(SimpTempNet, self).add_nodes_from([(0, {
            'label': 'init',
            'type': 'init'
        })])
        for a in range(len(actions)):
            super(SimpTempNet, self).add_nodes_from([(a + 1, {
                'label': "[{}] {} {}".format(a + 1, str(actions[a]), times[a]),
                'type': "start" if "start" in str(actions[a]) else "end"
            })])

        self.addDurations(actions, times)

        # Add edges
        for it in range(len(times)):
            t = 0
            if len(times[it]) > 0:
                t = max([int(time) for time in times[it]])
            if not super(SimpTempNet, self).has_edge(t, it + 1):
                super(SimpTempNet, self).add_edge(t, it + 1, weight=0, label="from {} to {}".format(t, it + 1))

    def getEnd(self, node_id):
        return self.nodes[node_id]['related'][0]

    def checkConsistency(self):
        def w(u, v, d):
            return d['weight']

        found = False
        for n in self.nodes:
            try:
                res = nx.find_negative_cycle(self, n, w)
                found = True
                print("Found negative cycle: ")
                for e in res:
                    print(e)
                break
            except Exception:
                pass

        return not found
