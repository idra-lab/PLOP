import argparse
from flask import Flask, request, Response
import json
import sys
sys.path.append('..')
from llm_kb_gen.gpt_convo import *


app = Flask(__name__)

@app.route("/llm_response_hl", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_hl
def llm_response_hl():
    """
        generates high-level knowledge base in response to the query sent via web-UI
    """
    llm_output_HL = "Invalid Response"
    #===========================================================#
    #                   Query Received from UI                  #
    #===========================================================#
    req_body = request.get_data(as_text=True)
    #print("Query: ", req_body)
    req_body_json = json.loads(req_body)
    received_query = req_body_json["query_HL"]
    #===========================================================#
    #               Open a new session with the LLM             #
    #===========================================================#
    llm_gpt = GPT_model(engine=params.LLM_VERSION)
    messages = []
    system_msg = ""


    file_name = "../llm_kb_gen/few_shot_hl.yaml"    

    with open(file_name) as file:
        yaml_file = yaml.load(file, Loader=yaml.FullLoader)["entries"]
        yaml_files = yaml_file["files"]

    # Open yaml files
    for file in yaml_files:
        includeYAML("../llm_kb_gen/"+file, messages, system_msg)

    # Here put the final query
    _, llm_output_HL = llm_gpt.get_response(
        received_query,
        messages=messages,
        end_when_error=True,
    )

    return{"response": "{}".format(llm_output_HL)}



@app.route("/llm_response_ll", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_ll
def llm_response_ll():
    """
        generates low-level knowledge base in response to the query sent via web-UI
    """    
    llm_output_LL = "Invalid Response"
    #===========================================================#
    #                   Query Received from UI                  #
    #===========================================================#
    req_body = request.get_data(as_text=True)
    #print("Query: ", req_body)
    req_body_json = json.loads(req_body)
    received_query = req_body_json["query_LL"]
    #===========================================================#
    #               Open a new session with the LLM             #
    #===========================================================#
    llm_gpt = GPT_model(engine=params.LLM_VERSION)
    messages = []
    system_msg = ""

    file_name = "../llm_ll_gen/few_shots_ll.yaml"

    with open(file_name) as file:
        yaml_file = yaml.load(file, Loader=yaml.FullLoader)["entries"]
        yaml_files = yaml_file["files"]

    # Open yaml files
    for file in yaml_files:
        includeYAML("../llm_ll_gen/"+file, messages, system_msg)

    # Here put the final query
    _, llm_output_LL = llm_gpt.get_response(
        received_query,
        messages=messages,
        end_when_error=True,
    )

    return{"response": "{}".format(llm_output_LL)}



@app.route("/llm_response_fix_hl", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_fix_hl
def llm_response_fix_hl():
    """
        corrects detected error(s) in the high-level knowledge base
    """
    try:        
        llm_output_fix = "Correcting HL error . . ."
        req_body = request.get_data(as_text=True)       # 
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
        pass

    if "specification_hl" in req_body_json:
        print("category_hl      : " + req_body_json["category_hl"])
        print("type_hl          : " + req_body_json["type_hl"])
        print("specification_hl : " + req_body_json["specification_hl"])

        #llm_output = processQuery(query)
        return{"response": "{}".format(llm_output_fix)}

    return{"response": "{}".format("Invalid Response")}


@app.route("/llm_response_fix_ll", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_fix_ll
def llm_response_fix_ll():
    """
        corrects detected error(s) in the low-level knowledge base
    """    
    try:        
        llm_output_fix = "Correcting LL error . . ."
        req_body = request.get_data(as_text=True)       #
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
        pass

    if "specification_ll" in req_body_json:
        print("category_ll      : " + req_body_json["category_ll"])
        print("type_ll          : " + req_body_json["type_ll"])
        print("specification_ll : " + req_body_json["specification_ll"])

        #llm_output = processQuery(query)
        return{"response": "{}".format(llm_output_fix)}

    return{"response": "{}".format("Invalid Response")}


@app.route("/kb_hl", methods = ['POST', 'GET']) # endpoint  => localhost:5001/kb_hl
def receive_kb_hl():
    """
        receives the high-level knowledge base
        TODO : parse Prolog code and dump into a Prolog file
    """
    try:        
        req_body = request.get_data(as_text=True)       #
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
        pass

    # Print the generated KB in console
    if "kb_hl" in req_body_json:
        print("kb_hl      : " + req_body_json["kb_hl"])

    return Response(status=200)


@app.route("/kb_ll", methods = ['POST', 'GET']) # endpoint  => localhost:5001/kb_ll
def receive_kb_ll():
    """
        receives the low-level knowledge base
        TODO : parse Prolog code and dump into a Prolog file
    """
    try:        
        req_body = request.get_data(as_text=True)       #
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
         pass

    # Print the generated KB in console
    if "kb_ll" in req_body_json:
        print("kb_ll      : " + req_body_json["kb_ll"])

    return Response(status=200)


if __name__ == "__main__":
    # Initialize parser
    parser = argparse.ArgumentParser()

    # Adding optional argument
    parser.add_argument("-L", "--LLM", help="ChatGPT configuration file, default is ./llm_kb_gen/conf/gpt40-8k.yaml", default="../llm_kb_gen/conf/gpt40-8k.yaml")

    # Read arguments from command line
    args = parser.parse_args()

    llm_conf_file = args.LLM
    print("LLM configuration file: ", llm_conf_file)
    if llm_conf_file.endswith(".yaml") and os.path.isfile(llm_conf_file):
        with open(llm_conf_file) as file:
            print("Opened")
            llm_conf = yaml.load(file, Loader=yaml.FullLoader)

            params.LLM_VERSION  = llm_conf["LLM_VERSION"]
            params.API_KEY_NAME = llm_conf["API_KEY_NAME"]
            params.ENDPOINT     = llm_conf["ENDPOINT"]
            params.API_VERSION  = llm_conf["API_VERSION"]

            print("LLM_VERSION: ", params.LLM_VERSION)
            print("API_KEY_NAME: ", params.API_KEY_NAME)
            print("ENDPOINT: ", params.ENDPOINT)
            print("API_VERSION: ", params.API_VERSION)

    else:
        print("The selected file {} does not exist or is not a yaml file".format(llm_conf_file))
        exit()

    # Run server
    app.run(debug=True, port=5001)
