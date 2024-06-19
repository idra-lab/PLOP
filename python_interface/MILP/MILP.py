import collections
from ortools.sat.python import cp_model

try:
    from python_interface.utility.Graph.Graph import Graph
except:
    from utility.Graph.Graph import Graph

class MILPSolver(cp_model.CpSolver): 
    def __init__(self, tta_actions, actions, adj_matrix, resources):
        super().__init__()
        self.status = cp_model.UNKNOWN

        self.tta_actions = tta_actions
        self.actions = actions
        self.adj_matrix = adj_matrix
        self.resources = resources

        print("Creating MILP model")
        print(self.tta_actions)
        print(self.actions)
        for row_id in range(len(self.adj_matrix)):
            #print row_id occupying as many spaces as math.log(len(self.adj_matrix), 10) + 1
            print(f"{row_id:>{len(str(len(self.adj_matrix)))}}", end=" ")
            for col_id in range(len(self.adj_matrix[row_id])):
                print(f"{self.adj_matrix[row_id][col_id]:>{len(str(len(self.adj_matrix)))}}", end=" ")
            print()
        print(self.resources)

        # Create dependency graph
        edges = []
        for i in range(len(self.adj_matrix)):
            for j in range(len(self.adj_matrix[i])):
                if self.adj_matrix[i][j] == 1:
                    edges.append(
                        (
                            (self.actions[i]),
                            (self.actions[j]),
                        )
                    )

        self.graph = Graph(edges)
        
        self.__verify__()

        # Create model
        self.model = cp_model.CpModel()

        task_type = collections.namedtuple("tta_type", "start end interval")
        
        horizon = int(1e12)
        self.all_tta = {}
        self.all_resources = collections.defaultdict(list)

        for tta in self.tta_actions.keys():
            prefix = f"{tta}_"
            start_var = self.model.NewIntVar(0, horizon, prefix + "s")
            end_var = self.model.NewIntVar(0, horizon, prefix + "e")
            duration = self.model.NewIntVar(self.tta_actions[tta]['l'], self.tta_actions[tta]['u'], prefix + "d")
            interval_var = self.model.NewIntervalVar(start_var, duration, end_var, prefix + "i")
            self.all_tta[tta] = task_type(start=start_var, end=end_var, interval=interval_var)
        
            if self.tta_actions[tta]['R'] != []:
                for res in self.tta_actions[tta]['R']:
                    self.all_resources[res].append(self.all_tta[tta].interval)

        self.__set_constraints__()

        # Objective function 
        self.model.minimize(self.all_tta['0'].end)


    def __verify__(self):
        """
        Verify the consistency of the adjacency matrix and actions list.

        Raises:
            Exception: - If the length of the adjacency matrix does not match the length of the actions list,
                       - If the length of the adjacency matrix does not match the length of the ttas list multiplied by 2.
                       - If a snap actions was not found in the ttas.
                       - If a snap action is found in more than one tta.
                       - All nodes must have a path to the final node.
        """        
        assert len(self.adj_matrix) == len(self.actions) and len(self.adj_matrix[0]) == len(self.actions), \
            "Error: Adjacency matrix and actions list length mismatch"
        
        assert len(self.actions) == 2*len(self.tta_actions), "Error: Actions and TTAs length mismatch"

        for snap_action in self.actions:
            snap_id = snap_action.split("_")[0]
            counter = 0
            for tta_action, value in self.tta_actions.items():
                if  value['s'].split('_')[0] == snap_id or \
                    value['e'].split('_')[0] == snap_id:
                    counter += 1
            if counter < 1:
                raise Exception(f"Error: Snap action {snap_action} not found in TTAs, {self.tta_actions}")
            elif counter > 1:
                raise Exception(f"Error: Snap action {snap_action} found in more than one TTA, {self.tta_actions}")

        for node in self.graph.nodes:
            if self.graph.find_cycle(node):
                raise Exception("Error: Graph has at least one cycle")
        
        import networkx as nx
        for node in self.graph.nodes:
            if node == self.tta_actions['0']['e']:
                continue
            assert nx.has_path(self.graph, node, self.tta_actions['0']['e']), f"Error: No path from {node} to final node"


    def draw_graph_from_matrix(self, title, rm_redundant=False, open_browser=False):
        self.graph.draw(title=title, rm_redundant=rm_redundant, open_browser=open_browser)


    def __tta_from_snap_action(self, action : str) -> str:
        """
        Get the TTA from a snap action.

        Since at the beginning we verify the consistency of the data, this method should always return a valid TTA.

        Args:
            action (str): The snap action.

        Returns:
            str: The TTA.
        """
        snap_id = action.split("_")[0]
        for tta_action, value in self.tta_actions.items():
            if  value['s'].split('_')[0] == snap_id or \
                value['e'].split('_')[0] == snap_id:
                return tta_action
        
        return ""


     ######   #######  ##    ##  ######  ######## ########     ###    #### ##    ## ########  ######  
    ##    ## ##     ## ###   ## ##    ##    ##    ##     ##   ## ##    ##  ###   ##    ##    ##    ## 
    ##       ##     ## ####  ## ##          ##    ##     ##  ##   ##   ##  ####  ##    ##    ##       
    ##       ##     ## ## ## ##  ######     ##    ########  ##     ##  ##  ## ## ##    ##     ######  
    ##       ##     ## ##  ####       ##    ##    ##   ##   #########  ##  ##  ####    ##          ## 
    ##    ## ##     ## ##   ### ##    ##    ##    ##    ##  ##     ##  ##  ##   ###    ##    ##    ## 
     ######   #######  ##    ##  ######     ##    ##     ## ##     ## #### ##    ##    ##     ######  
    def __set_constraints__(self):
        # The first task must start at 0
        self.model.Add(self.all_tta['0'].start == 0)
        
        # Snap actions can start after all the previous snap actions have started
        for idr in range(len(self.adj_matrix)):
            for idc in range(len(self.adj_matrix[idr])):
                if self.adj_matrix[idr][idc] == 1:
                    tta_id1 = self.__tta_from_snap_action(self.actions[idr].split("_")[0])
                    tta_id2 = self.__tta_from_snap_action(self.actions[idc].split("_")[0])
                    # print(f"Checking {idr},{tta_id1} -> {idc},{tta_id2}")
                    print(f"Adding constraint that {self.actions[idc]} starts after {self.actions[idr]}")
        
                    if "_s" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].start)
                            # print(f"Adding constraint that {tta_id2}_s starts after {tta_id1}_s")
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].end)
                            # print(f"Adding constraint that {tta_id2}_s starts after {tta_id1}_e")
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}, no _s or _e in {self.actions[idr]}")
                    
                    elif "_e" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].start)
                            # print(f"Adding constraint that {tta_id2}_e starts after {tta_id1}_s")
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].end)
                            # print(f"Adding constraint that {tta_id2}_e starts after {tta_id1}_e")
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}")
                    
                    # else:
                    #     raise Exception(f"Invalid action {idc} {self.actions[idc]}, no _s or _e in {self.actions[idc]}")
                    
        # The last task must end at the end
        for tta in self.all_tta.keys():
            if tta != '0':
                self.model.add(self.all_tta[tta].end <= self.all_tta['0'].end)
        
        # The number of tasks using a resource must be less than the resource capacity
        for res in self.all_resources.keys():
            self.model.AddCumulative(self.all_resources[res], [1 for n_tta in self.all_resources[res]], self.resources[res])

    def solve(self) -> None:
        """
        Solve the MILP problem.

        This method solves the MILP problem using the underlying solver. It sets the status of the solution and prints the solution if it is found. It also prints statistics about the solving process.

        Returns: 
            None
        """
        self.status = super().solve(self.model)
        
        if self.status == cp_model.OPTIMAL or self.status == cp_model.FEASIBLE:
            print("Solution found.")
            self.print_solution()
        else:
            print("No solution found.", self.solution_info())

        # Statistics.
        print("\nStatistics")
        print(f"  - conflicts: {self.num_conflicts}")
        print(f"  - branches : {self.num_branches}")
        print(f"  - wall time: {self.wall_time}s")

    def print_solution(self) -> None:
        """
        Prints the solution of the MILP problem.

        If the MILP problem has an optimal or feasible solution, this method prints the solution
        by iterating over all tasks and printing their start time, end time, and duration.

        Returns:
            None
        """
        if self.status == cp_model.OPTIMAL or self.status == cp_model.FEASIBLE:
            print("Solution:")

            output = ""

            for tta in self.all_tta.keys():
                start = self.Value(self.all_tta[tta].start)
                end = self.Value(self.all_tta[tta].end)
                duration = self.Value(self.all_tta[tta].interval.size_expr())
                output += f"{tta} : [{start} -> {end}, {duration}]\n"

            # Finally print the solution found.
            print(f"Optimal Schedule Length: {self.objective_value}")
            print(output)

            output = ""

            for tta in self.all_tta.keys():
                start = self.Value(self.all_tta[tta].start)
                end = self.Value(self.all_tta[tta].end)
                duration = self.Value(self.all_tta[tta].interval.size_expr())
                output += f"{self.tta_actions[tta]['s']}({start}) -> {self.tta_actions[tta]['e']}({end}), {duration}]\n"

            print(output)

            self.draw_intervals()

    def draw_intervals(self):
        """
        Given the solution of the MILP problem, this method draws the intervals of the tasks using matplotlib.
        """

        import matplotlib.pyplot as plt

        for tta in self.all_tta.keys():
            start = self.Value(self.all_tta[tta].start)
            end = self.Value(self.all_tta[tta].end)
            duration = self.Value(self.all_tta[tta].interval.size_expr())

            plt.plot([start, end], [float(tta), float(tta)], 'k-')
            plt.plot([start, start], [float(tta)-0.1, float(tta)+0.1], 'k-')
            plt.plot([end, end], [float(tta)-0.1, float(tta)+0.1], 'k-')

            plt.text(start, float(tta)+0.2, self.tta_actions[tta]['s'], ha='center')
            plt.text(end, float(tta)+0.2, self.tta_actions[tta]['e'], ha='center')

        plt.show()