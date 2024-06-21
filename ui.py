# This file is the main file which coordinates the different aspects. 

from python_interface import planner
from LLM.LLM import LLM


QUERY = """
test
"""


def hl_llm(query) -> str:
    # Extract HL knowledge base
    print("Extracting HL knowledge base")
    llm = LLM(examples_yaml_file = ["./LLM/few-shots-new.yaml"])
    
    kb = ""
    valid = True

    # Generate initial state
    succ, response = llm.query(query)
    assert succ == True, "Failed to generate initial state"
    kb += response
    print(succ, response)

    # # Generate final state
    # succ, response = llm.query(query)
    # assert succ == True, "Failed to generate final state"
    # kb += response
    # print(succ, response)

    # # Generate action set
    # succ, response = llm.query(query)
    # assert succ == True, "Failed to generate action set"
    # kb += response
    # print(succ, response)

    return kb


def ll_llm(query) -> str: 
    # Extract LL knowledge base
    print("Extract LL knowledge base")
    llm = LLM(examples_yaml_file = ["./LLM/few-shots-ll.yaml"])
    
    kb = ""
    valid = True

    # Generate initial state
    succ, response = llm.query(query)
    assert succ == True, "Failed to generate LL KB"
    kb += response
    print(succ, response)

    return kb
    


def find_plan(knowledge_base) -> planner.BehaviourTree:
    # Find plan
    print("Find plan")
    


def execute_plan(plan):
    # Execute plan
    print("Execute plan")
    


def main():
    # query = input("Describe your problem: ")
    query = """
    Write the initial state for the following situation: there are 3 blocks on the table. Block b1 is in position (2,2),
    block b2 is in position (4,4) and block b5 is in position (5,5).
    """

    query = """
    Write the initial state for the following situation: there are 3 blocks. Block b1 is in position (2,2), block b2 is
    in position (4,4) and block b5 is on top of b1.
    """

    query = """
    Write the initial state for the following situation: there are 3 blocks. Block b1 is in position (2,2), block b2 is
    in position (4,4) and block b5 is on top of b1 and block b5 is on the table in position (5,5). If you think that 
    there is a problem with the description, then write 'PROBLEM' and describe the problem.
    """

    query = """
    Write the final state in which we want to end: there are 3 blocks. Block b1 is in position (2,2), block b2 is in
    position (4,4) and block b5 is on top of b1.
    """

    query = """
    Write the action set for the following scenario. There are two robotics agents that have an arm with a gripper. They
    can move the arm from a position (x1,y1) to another position (x2,y2). They can also pick up a block from the table
    or pick up a block from on top of another block. They can also put a block on the table or put a block on top of
    another block. Finally, they can use the gripper to grasp a block or release a block.
    """

    query_hl = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. The
    agents can move the arm from a position (x1,y1) to another position (x2,y2). They can also pick up a block from the 
    table or pick up a block from on top of another block. They can also put a block on the table or put a block on top
    of another block. Finally, they can use the gripper to grasp a block or release a block. The goal is to put block b1
    on top of block b5 in position (5,5). Write the knowledge base for this scenario.
    """

    query_hl = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. 
    They can:
     - move a block from on top of a block to the table;
     - move a block from the table to another position on the table;
     - move a block from on top of a block and place it on top of another block;
     - move a block from the table and place it on top of another block.
    The goal is to put block b1 on top of block b5 in position (5,5). Write the knowledge base for this scenario.
    """

    query_ll = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. The
    agents can move the arm from a position (x1,y1) to another position (x2,y2). They can also pick up a block from the 
    table or pick up a block from on top of another block. They can also put a block on the table or put a block on top
    of another block. Finally, they can use the gripper to grasp a block or release a block. The goal is to put block b1
    on top of block b5 in position (5,5). The high-level actions are:\n{}\n
    The API that we have available are:
    move_arm(arm, x1, y1, x2, y2), which moves a robotic arm from position (x1,y1) to position (x2,y2).
    close(arm), which makes the gripper close.
    open(arm), which makes the gripper open.
    Could you:
    - update the knowledge base containing the new low-level predicates and the resources,
    - identify the low-level actions,
    - provide the mappings from high-level actions to low-level actions.
    """

    # Use HL LLM to extract HL knowledge base
    hl_kb = hl_llm(query_hl)

    # use LL LLM to extract LL knowledge base
    kb = ll_llm(query_ll.format(hl_kb))

    # Take the whole knowledge base and find plan
    bt = find_plan(kb)

    # Execute the plan
    execute_plan(bt)


if __name__ == "__main__":
    main()