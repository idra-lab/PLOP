from sys import exit

from python_interface.Prolog import prolog as PrologLib
from python_interface.STN import SimpTempNet
from python_interface.BT import BehaviourTree

def main():
    Actions, Times = PrologLib.execTest("testScan")

    stn = SimpTempNet(Actions, Times)

    if stn.checkConsistency():
        print("No negative cycle, continuing")
    else:
        exit(-1)

    stn.draw()

    bt = BehaviourTree(stn)
    bt.draw()
    bt.tick()


if __name__ == "__main__":
    main()
