import os
import sys

try:
    from python_interface.BT.BT import BehaviourTree
    from python_interface.MILP.MILP import MILPSolver
    from python_interface.Prolog import prolog as PrologLib
except:
    from BT.BT import BehaviourTree
    from MILP.MILP import MILPSolver
    from Prolog import prolog as PrologLib

# from STN import SimpTempNet
# from BT.BT import BehaviourTree


def main():
    data_dict = PrologLib.execTest(kb_path="/home/enrico/Projects/prolog_planner/kb.pl")

    #  for i in range(len(data_dict["adj_matrix"][0])):
    #      if i not in [1, 13]:
    #          data_dict["adj_matrix"][0][i] = 0

    #  for i in range(len(data_dict["adj_matrix"])):
    #      if i != 36:
    #          data_dict["adj_matrix"][i][37] = 0

    # return

    milp_solver = MILPSolver(
        data_dict["tt_actions"],
        data_dict["actions"],
        data_dict["adj_matrix"],
        data_dict["resources"],
    )

    milp_solver.solve()
    milp_solver.draw_graph_from_matrix("MILP.html", open_browser=False)

    # bt = BehaviourTree(stn)
    # bt.draw()
    # bt.tick()


if __name__ == "__main__":
    main()
