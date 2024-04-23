from sys import exit

from Prolog import prolog as PrologLib
from MILP.MILP import MILPSolver
# from STN import SimpTempNet
# from BT.BT import BehaviourTree


def main():
    data_dict = PrologLib.execTest()

    for i in range(len(data_dict["adj_matrix"][0])):
        if i not in [1,13]:
            data_dict["adj_matrix"][0][i] = 0
    
    for i in range(len(data_dict["adj_matrix"])):
        if i != 36:
            data_dict["adj_matrix"][i][37] = 0
    
    # data_dict["adj_matrix"][12][25] = 1
    # data_dict["adj_matrix"][24][25] = 1

    data_dict["resources"]['agent'] = 2

    milp_solver = MILPSolver(
        data_dict["tt_actions"],
        data_dict["actions"],
        data_dict["adj_matrix"],
        data_dict["resources"],
    )

    milp_solver.solve()
    milp_solver.draw_graph_from_matrix("MILP.html", open_browser=False)

    # stn = SimpTempNet(Actions, Times)

    # if stn.checkConsistency():
    #     print("No negative cycle, continuing")
    # else:
    #     exit(-1)

    # stn.draw()

    # bt = BehaviourTree(stn)
    # bt.draw()
    # bt.tick()


if __name__ == "__main__":
    main()
