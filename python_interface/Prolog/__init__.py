import os 
import sys

# Get directory of the current file
_dir_path = os.path.dirname(os.path.realpath(__file__))
PLANNER_PATH = os.path.join(_dir_path, "..", "..", "prolog_planner", "planner.pl")