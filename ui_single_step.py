# This file is the main file which coordinates the different aspects of the planner. 

from python_interface import planner
from LLM.LLM import LLM
import re, os

def scan_and_extract(kb, response, hierarchy=False):
    pattern = re.compile(r'\`\`\`\s*(\w+)\s*([^\`]*?)\`\`\`', re.DOTALL)
    matches = pattern.findall(response)
    for Key, value in matches:
        key = Key.lower().replace(" ", "")
        value = value.strip()

        for state in ["init", "goal"]:
            if key == state:
                if not re.match(r"hl_{}\(.*\)\.".format(state), value):
                    new_value = f"{state}_init({value})."
                    if not value.endswith("."):
                        new_value += "."
                    value = new_value

        kb[key] = value
            

def hl_llm(query) -> str:
    # Extract HL knowledge base
    print("Extracting HL knowledge base")
    llm = LLM(
        llm_connection_config_file=os.path.join(os.path.dirname(__file__), 'LLM', 'conf/gpt40-128k.yaml'),
        examples_yaml_file = ["./LLM/few-shots-hl.yaml"])
    
    kb = {}
    valid = True

    planner_line = open("the_file.pl", "r").read()

    query = "Knowing that this is the structure of the planner written in Prolog.\n" + planner_line +\
        "Considering the following scenario:\n" + query + "\nCould you:" +\
        "\n- write the initial and final states for this scenario;" +\
        "\n- write the general static knowledge base, highlighting which are the resources;" +\
        "\n- write the required actions;" +\
        "\nMind not to delete any necessary predicate between the initial and final states." +\
        "\nEven if the set of actions was previously provided, please repeat it. DO NOT OMIT ANYTHING even if the pattern is the same." +\
        "\nIf you think that there is a problem with the description, then write 'PROBLEM' and describe the problem." +\
        "\nWhen generating the output, please provide a description of your choices."

    # Generate initial state
    succ, response = llm.query(query)
    assert succ == True, "Failed to generate initial state"
    print(succ, response)

    scan_and_extract(kb, response)

    return kb, response


def ll_llm(query, kb) -> str: 
    # Generate low-level knowledge base
    print("[LL] Generating low-level knowledge base")
    llm = LLM(examples_yaml_file = ["./LLM/few-shots-ll.yaml"])

    hl_kb = """
    ```kb
    {}
    ```
    ```init
    {}
    ```
    ```goal
    {}
    ```
    ```actions
    {}
    ```""".format(kb["kb"], kb["init"], kb["goal"], kb["actions"])

    ll_kb_query = "\nGiven the high-level knowledge base:\n```kb\n{}\n```".format(hl_kb) + \
        "\n{}\nCould you:".format(query) + \
        "\n- update the knowledge base containing the new low-level predicates and the resources;" + \
        "\n- identify the low-level actions;" + \
        "\n- update the initial and final states;" + \
        "\n- provide the mappings from high-level actions to low-level actions."
    
    succ, response = llm.query(ll_kb_query)
    assert succ == True, "Failed to generate LL KB"
    # print(succ, response)
    scan_and_extract(kb, response)

    return kb, response
    

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
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 8 agents with a robotic arm. 
    The goal is to put block b1 on top of block b5 in position (5,5). The agents can:
    - move a block from a position on the table to another position on the table. The block moved must not have another 
      block on top. 
    - move a block from a position on the table to the top of another block that is on the table. Neither blocks can 
      have another block on top.
    - move a block B1 from the top of a block B2 to the top of another block B3. Neither B1 nor B3 can have a block on 
      top of them and B2 will become free after the start of the action.
    - move a block from the top of another block to a position on the table. The block moved must not have another block 
      on top and the final position must be free.
    """

    query_hl = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 8 agents with a robotic arm. 
    The goal is to put block b1 on top of block b5 in position (5,5). The agents can:
    - grip a block that is on the table: the block must be free to be gripped.
    - grip a block that is on top of another block: the block must be free to be gripped and the block below will become
      free to be gripped afterwards.
    - release a block on table: the block must be gripped and the position on the table must be free.
    - release a block on top of another block: the block must be gripped and the block below must be free to receive the
      block. After that, the block below will not be free to be gripped.
    - move a block from a position to another position.
    """
    # Remember to prepend the high-level predicates with 'hl_'.

    query_ll = """
    There are 5 blocks on a table. Block b1 is in position (2,2), block b2 is in position (4,4), block b4 is in position
    (8,8). Block b5 is on top of block b1 and block b3 is on top of block b2. There are 2 agents with a robotic arm. The
    agents can move the arm from a position (x1,y1) to another position (x2,y2). They can also pick up a block from the 
    table or pick up a block from on top of another block. They can also put a block on the table or put a block on top
    of another block. Finally, they can use the gripper to grasp a block or release a block. The goal is to put block b1
    on top of block b5 in position (5,5).
    The API that we have available are:
    move_arm(arm, x1, y1, x2, y2), which moves a robotic arm from position (x1,y1) to position (x2,y2).
    close(arm), which makes the gripper close.
    open(arm), which makes the gripper open.
    """
    # Remember to prepend the low-level predicates with 'll_'.

    # Use HL LLM to extract HL knowledge base
    # hl_kb, response = hl_llm(query_hl)
    hl_kb, response = hl_llm(query_hl)

    # use LL LLM to extract LL knowledge base
    # kb, _ = ll_llm(query_ll.format(response), hl_kb)

    # for key, value in kb.items():
    #     print(key, value)

    if not os.path.isdir("output"): 
        os.makedirs("output")
    with open(os.path.join("output", "kb.pl"), "w") as file:
        file.write("% This file was automatically generated by the LLM system\n")
        first_line = True
        for key, value in hl_kb.items():
            if not first_line:
                file.write("\n")
            file.write(f"%%%%%%%%%%%%%%%%%%%%%%%\n% {key}\n%%%%%%%%%%%%%%%%%%%%%%%\n{value}\n")
            first_line = False

    # Take the whole knowledge base and find plan
    # bt = find_plan(kb)

    # Execute the plan
    # execute_plan(bt)


if __name__ == "__main__":
    main()