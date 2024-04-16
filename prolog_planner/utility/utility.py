from Graph.Graph import Graph
import re

ugraph = "[grip_end(r1)-[grip_start(r1),build_pillar_start(r1,a),build_pillar_start(r1,b),place_architrave_start(r1,a,b)],grip_start(r1)-[release_end(r1),build_pillar_end(r1,a),build_pillar_end(r1,b),build_pillar_start(r1,a),build_pillar_start(r1,b),place_architrave_start(r1,a,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,3,3,1),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1),move_arm_end(r1,9,9,0)],release_end(r1)-[release_start(r1),build_pillar_start(r1,a),build_pillar_start(r1,b),place_architrave_start(r1,a,b)],release_start(r1)-[grip_end(r1),release_end(r1),build_pillar_end(r1,a),build_pillar_end(r1,b),build_pillar_start(r1,a),build_pillar_start(r1,b),place_architrave_start(r1,a,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,3,3,1),move_arm_end(r1,4,4,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1),move_arm_end(r1,9,9,0)],build_pillar_end(r1,a)-[grip_end(r1),grip_start(r1),release_end(r1),release_start(r1),build_pillar_start(r1,a),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1),move_arm_start(r1,1,1,0),move_arm_start(r1,1,2,0),move_arm_start(r1,5,5,0),move_arm_start(r1,5,5,1)],build_pillar_end(r1,b)-[grip_end(r1),grip_start(r1),release_end(r1),release_start(r1),build_pillar_start(r1,b),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,3,3,1),move_arm_start(r1,2,1,0),move_arm_start(r1,2,2,0),move_arm_start(r1,3,3,0),move_arm_start(r1,3,3,1)],build_pillar_start(r1,a)-[],build_pillar_start(r1,b)-[build_pillar_end(r1,a)],place_architrave_end(r1,a,b)-[grip_end(r1),grip_start(r1),release_end(r1),release_start(r1),place_architrave_start(r1,a,b),move_arm_end(r1,4,4,0),move_arm_end(r1,9,9,0),move_arm_start(r1,4,4,0),move_arm_start(r1,9,9,0)],place_architrave_start(r1,a,b)-[build_pillar_end(r1,a),build_pillar_end(r1,b)],move_arm_end(r1,1,1,0)-[build_pillar_start(r1,a),move_arm_start(r1,1,1,0)],move_arm_end(r1,1,2,0)-[build_pillar_start(r1,a),move_arm_start(r1,1,2,0)],move_arm_end(r1,2,1,0)-[build_pillar_start(r1,b),move_arm_start(r1,2,1,0)],move_arm_end(r1,2,2,0)-[build_pillar_start(r1,b),move_arm_start(r1,2,2,0)],move_arm_end(r1,3,3,0)-[build_pillar_start(r1,b),move_arm_start(r1,3,3,0)],move_arm_end(r1,3,3,1)-[build_pillar_start(r1,b),move_arm_start(r1,3,3,1)],move_arm_end(r1,4,4,0)-[place_architrave_start(r1,a,b),move_arm_start(r1,4,4,0)],move_arm_end(r1,5,5,0)-[build_pillar_start(r1,a),move_arm_start(r1,5,5,0)],move_arm_end(r1,5,5,1)-[build_pillar_start(r1,a),move_arm_start(r1,5,5,1)],move_arm_end(r1,9,9,0)-[place_architrave_start(r1,a,b),move_arm_start(r1,9,9,0)],move_arm_start(r1,1,1,0)-[build_pillar_start(r1,a)],move_arm_start(r1,1,2,0)-[release_end(r1),build_pillar_start(r1,a),move_arm_end(r1,1,1,0),move_arm_end(r1,5,5,0)],move_arm_start(r1,2,1,0)-[release_end(r1),build_pillar_end(r1,a),build_pillar_start(r1,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1)],move_arm_start(r1,2,2,0)-[release_end(r1),build_pillar_end(r1,a),build_pillar_start(r1,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,3,3,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1)],move_arm_start(r1,3,3,0)-[release_end(r1),build_pillar_end(r1,a),build_pillar_start(r1,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1)],move_arm_start(r1,3,3,1)-[release_end(r1),build_pillar_end(r1,a),build_pillar_start(r1,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1)],move_arm_start(r1,4,4,0)-[release_end(r1),build_pillar_end(r1,a),build_pillar_end(r1,b),place_architrave_start(r1,a,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,3,3,1),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1),move_arm_end(r1,9,9,0)],move_arm_start(r1,5,5,0)-[build_pillar_start(r1,a),move_arm_end(r1,1,1,0)],move_arm_start(r1,5,5,1)-[release_end(r1),build_pillar_start(r1,a),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,5,5,0)],move_arm_start(r1,9,9,0)-[release_end(r1),build_pillar_end(r1,a),build_pillar_end(r1,b),place_architrave_start(r1,a,b),move_arm_end(r1,1,1,0),move_arm_end(r1,1,2,0),move_arm_end(r1,2,1,0),move_arm_end(r1,2,2,0),move_arm_end(r1,3,3,0),move_arm_end(r1,3,3,1),move_arm_end(r1,5,5,0),move_arm_end(r1,5,5,1)]]"

ugraph = """
[
    tta(grip_end(r1),4)-[tta(build_pillar_start(r1,a),0)],
    tta(grip_end(r1),12)-[tta(build_pillar_start(r1,a),0)],
    tta(grip_end(r1),22)-[tta(build_pillar_start(r1,b),18)],
    tta(grip_end(r1),30)-[tta(build_pillar_start(r1,b),18)],
    tta(grip_end(r1),40)-[tta(place_architrave_start(r1,a,b),36)],
    tta(grip_start(r1),3)-[tta(build_pillar_start(r1,a),0)],
    tta(grip_start(r1),11)-[tta(build_pillar_start(r1,a),0)],
    tta(grip_start(r1),21)-[tta(build_pillar_start(r1,b),18)],
    tta(grip_start(r1),29)-[tta(build_pillar_start(r1,b),18)],
    tta(grip_start(r1),39)-[tta(place_architrave_start(r1,a,b),36)],
    tta(release_end(r1),8)-[tta(build_pillar_start(r1,a),0)],
    tta(release_end(r1),16)-[tta(build_pillar_start(r1,a),0)],
    tta(release_end(r1),26)-[tta(build_pillar_start(r1,b),18)],
    tta(release_end(r1),34)-[tta(build_pillar_start(r1,b),18)],
    tta(release_end(r1),44)-[tta(place_architrave_start(r1,a,b),36)],
    tta(release_start(r1),7)-[tta(build_pillar_start(r1,a),0)],
    tta(release_start(r1),15)-[tta(build_pillar_start(r1,a),0)],
    tta(release_start(r1),25)-[tta(build_pillar_start(r1,b),18)],
    tta(release_start(r1),33)-[tta(build_pillar_start(r1,b),18)],
    tta(release_start(r1),43)-[tta(place_architrave_start(r1,a,b),36)],
    tta(build_pillar_end(r1,a),17)-[tta(grip_end(r1),4),tta(grip_end(r1),12),tta(grip_start(r1),3),tta(grip_start(r1),11),tta(release_end(r1),8),tta(release_end(r1),16),tta(release_start(r1),7),tta(release_start(r1),15),tta(move_arm_end(r1,1,1,0),2),tta(move_arm_end(r1,1,2,0),10),tta(move_arm_end(r1,5,5,0),6),tta(move_arm_end(r1,5,5,1),14),tta(move_arm_start(r1,1,1,0),1),tta(move_arm_start(r1,1,2,0),9),tta(move_arm_start(r1,5,5,0),5),tta(move_arm_start(r1,5,5,1),13)],
    tta(build_pillar_end(r1,b),35)-[tta(grip_end(r1),22),tta(grip_end(r1),30),tta(grip_start(r1),21),tta(grip_start(r1),29),tta(release_end(r1),26),tta(release_end(r1),34),tta(release_start(r1),25),tta(release_start(r1),33),tta(move_arm_end(r1,2,1,0),20),tta(move_arm_end(r1,2,2,0),28),tta(move_arm_end(r1,3,3,0),24),tta(move_arm_end(r1,3,3,1),32),tta(move_arm_start(r1,2,1,0),19),tta(move_arm_start(r1,2,2,0),27),tta(move_arm_start(r1,3,3,0),23),tta(move_arm_start(r1,3,3,1),31)],
    tta(build_pillar_start(r1,a),0)-[],
    tta(build_pillar_start(r1,b),18)-[],
    tta(place_architrave_end(r1,a,b),45)-[tta(grip_end(r1),40),tta(grip_start(r1),39),tta(release_end(r1),44),tta(release_start(r1),43),tta(move_arm_end(r1,4,4,0),42),tta(move_arm_end(r1,9,9,0),38),tta(move_arm_start(r1,4,4,0),41),tta(move_arm_start(r1,9,9,0),37)],
    tta(place_architrave_start(r1,a,b),36)-[],
    tta(move_arm_end(r1,1,1,0),2)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_end(r1,1,2,0),10)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_end(r1,2,1,0),20)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_end(r1,2,2,0),28)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_end(r1,3,3,0),24)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_end(r1,3,3,1),32)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_end(r1,4,4,0),42)-[tta(place_architrave_start(r1,a,b),36)],
    tta(move_arm_end(r1,5,5,0),6)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_end(r1,5,5,1),14)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_end(r1,9,9,0),38)-[tta(place_architrave_start(r1,a,b),36)],
    tta(move_arm_start(r1,1,1,0),1)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_start(r1,1,2,0),9)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_start(r1,2,1,0),19)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_start(r1,2,2,0),27)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_start(r1,3,3,0),23)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_start(r1,3,3,1),31)-[tta(build_pillar_start(r1,b),18)],
    tta(move_arm_start(r1,4,4,0),41)-[tta(place_architrave_start(r1,a,b),36)],
    tta(move_arm_start(r1,5,5,0),5)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_start(r1,5,5,1),13)-[tta(build_pillar_start(r1,a),0)],
    tta(move_arm_start(r1,9,9,0),37)-[tta(place_architrave_start(r1,a,b),36)]
]
"""

def draw_graph(graph_str:str) -> None:
    """
    Takes a ugraph from Prolog and draws it 
    """
    graph = Graph()
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
            for item in re.finditer(r"(?P<tta>tta\((?P<action>[a-zA-Z_]+\([a-zA-Z0-9,_]*\)),(?P<time>[0-9]+)\))", edges):
                action = item.groupdict()['tta']
                # print(action)
                graph.add_edge(node, action)
                print("adding edge ", node, action)
    graph.draw(mode="pyvis", title="ll_po_plan_red.html", rm_redundant=False)
    graph.draw(mode="pyvis", title="ll_po_plan.html", rm_redundant=True)

def draw_other():
    achievers=[
        [],
        [0],
        [1],
        [2],
        [2, 3],
        [2, 4],
        [2, 5],
        [2, 4, 6],
        [2, 3, 7],
        [2, 4, 6, 8],
        [2, 9],
        [2, 3, 4, 5, 6, 7, 8, 9, 10],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
        [0, 12],
        [13],
        [4, 6, 8, 10, 14],
        [3, 7, 14, 15],
        [4, 6, 8, 10, 14, 16],
        [5, 14, 17],
        [4, 6, 8, 10, 14, 16, 18],
        [3, 7, 14, 15, 19],
        [4, 6, 8, 10, 14, 16, 18, 20],
        [9, 14, 21],
        [14, 15, 16, 17, 18, 19, 20, 21, 22],
        [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
        [12, 24],
        [25],
        [4, 6, 8, 10, 16, 18, 20, 22, 26],
        [3, 7, 15, 19, 26, 27],
        [4, 6, 8, 10, 16, 18, 20, 22, 26, 28],
        [5, 17, 26, 29],
        [4, 6, 8, 10, 16, 18, 20, 22, 26, 28, 30],
        [3, 7, 15, 19, 26, 27, 31],
        [4, 6, 8, 10, 16, 18, 20, 22, 26, 28, 30, 32],
        [9, 21, 26, 33],
        [26, 27, 28, 29, 30, 31, 32, 33, 34],
        [25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35],
        [1, 12, 13, 24, 25, 36]
    ]
    edges=[]
    for i in range(len(achievers)):
        for e in achievers[i]:
            edges.append((i,e))
    graph = Graph(edges)
    graph.add_node("0")
    graph.draw(mode="pyvis", title="test2.html", rm_redundant=False)

if __name__ == '__main__':
    # draw_graph(ugraph)
    draw_other()

