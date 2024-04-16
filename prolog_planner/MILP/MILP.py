import collections
from ortools.sat.python import cp_model

from utility.Graph import Graph

class MILPSolver(cp_model.CpSolver): 
    def __init__(self, tta_actions, actions, adj_matrix, resources):


        super().__init__()
        self.status = cp_model.UNKNOWN

        self.tta_actions = tta_actions
        self.actions = actions
        self.adj_matrix = adj_matrix
        self.resources = resources

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
            Exception: If the length of the adjacency matrix does not match the length of the actions list,
                        or if the length of the adjacency matrix does not match the length of the actions list multiplied by 2.
        """        
        if len(self.adj_matrix) != len(self.actions) or len(self.adj_matrix[0]) != len(self.actions):
            raise Exception("Error: Adjacency matrix and actions list length mismatch")
        
        if 2*len(self.actions) != len(self.tta_actions):
            raise Exception("Error: Actions and TTAs length mismatch")
        

    # TODO This is not working since the import is wrong, but for God sake I 
    # cannot make this work and I sincerely do not understand how Python modules
    # should be imported reliably.
    def draw_graph_from_matrix(self, title, rm_redundant=False, open_browser=False):
        edges = []
        for i in range(len(self.adj_matrix)):
            for j in range(len(self.adj_matrix[i])):
                if self.adj_matrix[i][j] == 1:
                    edges.append(
                        (
                            "{}_{}".format(str(i), self.actions[i]),
                            "{}_{}".format(str(j), self.actions[j]),
                        )
                    )

        graph = Graph(edges)
        graph.draw(title=title, rm_redundant=False, open_browser=False)


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
                    tta_id1 = self.actions[idr].split("_")[0]
                    tta_id2 = self.actions[idc].split("_")[0]
                    # print(f"Checking {idr},{tta_id1} -> {idc},{tta_id2}")
        
                    if "_s" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].start)
                            print("Adding constraint that {}_s starts after {}_s".format(tta_id2, tta_id1))
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].start >= self.all_tta[tta_id1].end)
                            print("Adding constraint that {}_s starts after {}_e".format(tta_id2, tta_id1))
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}, no _s or _e in {self.actions[idr]}")
                    
                    elif "_e" in self.actions[idc]:
                        if "_s" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].start)
                            print("Adding constraint that {}_e starts after {}_s".format(tta_id2, tta_id1))
                        elif "_e" in self.actions[idr]:
                            self.model.Add(self.all_tta[tta_id2].end >= self.all_tta[tta_id1].end)
                            print("Adding constraint that {}_e starts after {}_e".format(tta_id2, tta_id1))
                        else:
                            raise Exception(f"Invalid action {idr} {self.actions[idr]}")
                    
                    else:
                        raise Exception(f"Invalid action {idc} {self.actions[idc]}, no _s or _e in {self.actions[idc]}")
                    
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
