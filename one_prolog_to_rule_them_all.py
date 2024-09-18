import sys, os
import re
import networkx as nx
import matplotlib.pyplot as plt
import art 


########################################################################################################################


def parse_file_old(parent_file, G, path):
    # Read the file to a single string 
    with open(parent_file, "r") as file:
        for line in file:
            # Check if the line is a comment
            if not line.strip().startswith("%"):
                # Check if the line is in the form ":- ensure_loaded('filename')" and extract the filename
                filenames = re.findall(r"\s*(?<!^%)\s*:- ensure_loaded\('(.+?)'\)", line)

                for filename in filenames:
                    filename = os.path.join(path, filename)
                    new_path = os.path.dirname(filename)
                    print("Found filename: ", filename)

                    # Check if node already exists
                    if filename in G.nodes:
                        print("Node already exists")
                        return

                    # Add node and edge 
                    G.add_node(filename)
                    print('Adding edge from `{}` to `{}`'.format(parent_file, filename))
                    G.add_edge(parent_file, filename)
                    parse_file_old(filename, G, new_path)


def create_one_file(G, the_file):
    with open(the_file, "w") as file:
        for node in G.nodes:
            with open(node, "r") as f:
                content = f.read()

                # Replace all occurrences of ":- ensure_loaded('filename')." with an empty string
                content = re.sub(r"\s*(?<!^%)\s*:- ensure_loaded\('(.+?)'\)\.", "", content)
                
                file_name = art.text2art(node)
                # Replace all '\n' with '\n%' in file_name
                file_name = re.sub(r"\n", "\n%", file_name)

                file.write("% {}\n".format(file_name))
                file.write(content)
                file.write("\n")


########################################################################################################################


def parse_file(parent_file, G, path, the_file="the_file.pl"):
    print("Parsing file: ", parent_file)
    with open(parent_file, "r") as file:
        with open(the_file, "a") as f:
            for line in file:
                # Check if this line is in the form ":- ensure_loaded('filename')." and extract the filename.
                filename_import = re.search(r"\s*(?<!^%)\s*:- ensure_loaded\('(.+?)'\)\.", line)
                if filename_import:
                    # Check if the line is a comment
                    if not line.strip().startswith("%"):
                        filename = os.path.join(path, filename_import.group(1))
                        new_path = os.path.dirname(filename)
                        print("Found filename: ", filename)

                        # Check if node already exists
                        if filename in G.nodes:
                            print("Node already exists")
                            continue
                        


                        # Add node and edge 
                        G.add_node(filename)
                        print('Adding edge from `{}` to `{}`'.format(parent_file, filename))
                        G.add_edge(parent_file, filename)

                        parse_file(filename, G, new_path, the_file)

                else:
                    f.write(line)


########################################################################################################################


def main(root_file = ""):
    open("the_file.pl", "w+").close()
    if root_file == "":
        print("No root file provided")
        return
    else:
        # Check that root_file exists
        assert os.path.isfile(root_file), "File {} does not exist".format(root_file)

        # Extract path
        path = os.path.dirname(root_file)

        # Initialize a directed graph
        G = nx.DiGraph()
        # Add the root file to the graph
        G.add_node(root_file)

        # Parse the root file
        parse_file(root_file, G, path)

        # Draw graph
        # nx.draw(G, with_labels=True)
        # plt.show()

        # create_one_file(G, "the_file.pl")
        print("Done")

                
########################################################################################################################


if __name__ == "__main__":
    if len(sys.argv) == 1:
        sys.exit("Usage: python3 one_prolog_to_rule_them_all.py <root_file>")
    main(sys.argv[1])