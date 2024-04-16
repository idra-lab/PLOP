from MILP.MILP import MILPSolver

tt_actions = {
    '0' : {'s' : '0_s', 'e' : '0_e', 'l' : 0, 'u' : int(1e12), 'R' : []},
    '1' : {'s' : '1_s', 'e' : '1_e', 'l' : 4, 'u' : 6, 'R' : ['A']},
    '2' : {'s' : '2_s', 'e' : '2_e', 'l' : 3, 'u' : 6, 'R' : ['A', 'B']},
    '3' : {'s' : '3_s', 'e' : '3_e', 'l' : 2, 'u' : 5, 'R' : ['B']},
}

actions = [
    '0_s',
    '1_s', 
    '1_e',
    '2_s',
    '2_e',
    '3_s',
    '3_e',
    '0_e' 
]

# Adjancey matrix for the action initialized to 0
adj_matrix = [
    #0, 1, 2, 3, 4, 5, 6, 7
    [0, 1, 0, 1, 0, 0, 0, 0],  # 0
    [0, 0, 1, 0, 0, 0, 0, 0],  # 1
    [0, 0, 0, 0, 1, 1, 0, 0],  # 2
    [0, 0, 0, 0, 1, 0, 0, 0],  # 3
    [0, 0, 0, 0, 0, 1, 0, 0],  # 4
    [0, 0, 0, 0, 0, 0, 1, 0],  # 5
    [0, 0, 0, 0, 0, 0, 0, 1],  # 6
    [0, 0, 0, 0, 0, 0, 0, 0],  # 7
]

resources = {
    'A' : 2, 
    'B' : 1
}


if __name__ == "__main__":
    solver = MILPSolver(tt_actions, actions, adj_matrix, resources)
    solver.solve()
