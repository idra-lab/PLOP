import os
import argparse, yaml
import random
import json
from datetime import datetime

#===================================================================================================================#
#                                               YAML2JSONL FUNCTION                                                 #
#===================================================================================================================#
def yaml2jsonl(file_name, file_id, isShuffle):
    msg_list = []
    # Open yaml file
    print("Adding examples from file:", file_name)
    with open(file_name) as file:
        yaml_file = yaml.load(file, Loader=yaml.FullLoader)['entries']

        for msg_id in yaml_file["convo"]:
            entry = {"messages":[]}
            #=======================================#
            #               USER QUERY              #
            #=======================================#
            user_query = {"role":"user"}
            user_query["content"] = yaml_file['convo'][msg_id]["Q"]["content"]
            entry["messages"].append(user_query)
            #=======================================#
            #           ASSISTANT RESPONSE          #
            #=======================================#
            assistant_query = {"role":"assistant"}
            assistant_query["content"] = yaml_file["convo"][msg_id]["A"]["content"]
            entry["messages"].append(assistant_query)
            
            msg_list.append(entry)

    # Shuffle messages
    if(isShuffle):
        random.shuffle(msg_list)
    
    current_datetime = datetime.now().strftime("%Y_%m_%d-%H_%M_%S")
    filename = "fine_tuning/fine_tuning_{}_".format(file_id) + current_datetime + ".jsonl"

    with open(filename, "w+") as outfile: 
        for msg in msg_list:
            json.dump(msg, outfile)
            outfile.write('\n')


#===================================================================================================================#
#                                               	MAIN FUNCTION                                                   #
#===================================================================================================================#
def main(yaml_files=[], isShuffle=False):

    if len(yaml_files)==0:
        print("Please provide the path to the YAML file using the '--yaml_files' argument")
        exit()

    for file_id in range(len(yaml_files)):
        yaml2jsonl(file_name=yaml_files[file_id], file_id=file_id, isShuffle=isShuffle)


if __name__ == '__main__':

    # Initialize parser
    parser = argparse.ArgumentParser()
    
    # Adding optional argument
    parser.add_argument("-y", "--yaml_files", nargs='+', help = "A space defined list of YAML files containing the examples")
    parser.add_argument("-s", "--shuffle_data", type=bool, help = "Flag determining whether the data will be shuffled before conversion")

    # Read arguments from command line
    args = parser.parse_args()

    if args.yaml_files != None:
        for file in args.yaml_files:
            assert(os.path.isfile(file)), "File {} does not exist".format(file)
        print("args.yaml_files : ", args.yaml_files)
    else:
        print("Please provide the path to the YAML file using the '--yaml_files' argument.")
        exit()

    main(yaml_files=args.yaml_files, isShuffle=args.shuffle_data)