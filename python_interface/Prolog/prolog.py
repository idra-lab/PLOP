from pyswip import Prolog, Functor, Variable, Query
import os

from . import PLANNER_PATH

def extract_actions(actions) -> list:
    """
    Actions have the form ID-Action(Args), which, when obtained by PySWI, becomes
    -(ID, Action), where ID and Action are part of the args member of Functor. 
    
    Args:
        actions (list): A list of Functors containing the actions.

    Returns:
        list: A list of strings containing the actions.
    """
    ret = list()
    for i in range(len(actions)):
        action = f"{actions[i].args[0]}_{actions[i].args[1]}"

        ret.append(action)
    return ret

def extract_resources(resources) -> dict:
    """
    Resource have the form N-Resource, which, when obtained by PySWI, become
    -(N, Resource), so we need to extract these values as a dictionary where 
    Resource is the key and N is the value. 

    Args:
        resources (list): A list of Functors containing the resources.

    Returns:
        dict: A dictionary containing the resources as keys and how many as values.
    """
    ret = dict()
    for i in range(len(resources)):
        resource = str(resources[i].args[0])
        if resource not in ret:
            ret[resource] = int(str(resources[i].args[1]))
        else:
            raise ValueError(f"Resource {resource} is duplicated")
    return ret

def extract_tt_actions(tt_actions, res_x_action) -> dict:
    """
    Time Triggered Actions have the form ID-[IDStart-Start]-[IDEnd-End], which, 
    when obtained by PySWI, becomes 
    -(ID, TTA, Args), where ID, TTA and Args are all part of the args
    member of Functor. So we need to extract these values. 
    """
    res_x_action_dict = dict()
    for i in range(len(res_x_action)):
        key = str(res_x_action[i].args[0])
        value = [str(x) for x in res_x_action[i].args[1]]
        res_x_action_dict[key] = value

    ret = dict()
    for i in range(len(tt_actions)):
        tt_action_id = str(tt_actions[i].args[0].args[0])

        start_id = str(tt_actions[i].args[0].args[1][0].args[0])
        start_action = str(tt_actions[i].args[0].args[1][0].args[1])

        end_id = str(tt_actions[i].args[1][0].args[0])
        end_action = str(tt_actions[i].args[1][0].args[1])

        ret[tt_action_id] = {
            "s": f"{start_id}_{start_action}",
            "e": f"{end_id}_{end_action}",
            "l": 10 if tt_action_id != "0" else 0,
            "u": 10 if tt_action_id != "0" else int(1e12),
            "R": res_x_action_dict[tt_action_id] if tt_action_id in res_x_action_dict else []
        }

    return ret

def execTest(test = "plan") -> dict:
    """
    Looks for the planner in the prolog_planner directory and executes the test specified in the argument.

    Args:
        test (str): The test to be executed. Defaults to "plan/5".
    
    Returns:
        dict: A dictionary containing the snap actions ('actions'), 
            the time triggered actions ('tt_actions'), the adjacency matrix ('adj_matrix'), 
            the list of resources ('resources'), 
            and the dictionary of resources used per action ('res_x_actions').
    """
    print(f"Executing {test} from Prolog")
    
    prolog = Prolog()
    # planner_path = os.path.join(os.getcwd(), "..", "prolog_planner")
    print(f"Looking for planner at {PLANNER_PATH}")
    prolog.consult(PLANNER_PATH)

    planner = Functor(test, 5)
    actions_var = Variable()
    tt_actions_var = Variable()
    adj_matrix_var = Variable()
    resources_var = Variable()
    res_x_actions_var = Variable()

    sol = Query(planner(actions_var, adj_matrix_var, tt_actions_var, resources_var, res_x_actions_var))

    sol.nextSolution()
    
    print("Executed prolog")

    # Convert the following code into a dictionary 
    data_dict = {
        "actions":       extract_actions(actions_var.get_value()),
        "tt_actions":    extract_tt_actions(tt_actions_var.get_value(), res_x_actions_var.get_value()),
        "adj_matrix":    adj_matrix_var.get_value(),
        "resources":     extract_resources(resources_var.get_value())
    }

    data_dict["actions"].insert(0, data_dict["tt_actions"]["0"]["s"])
    data_dict["actions"].append(data_dict["tt_actions"]["0"]["e"])

    for key, value in data_dict.items():
        print(key, value)
        print()

    return data_dict



