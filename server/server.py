#from llm_model import processQuery
import os
from flask import Flask, request, Response
import json
import time


app = Flask(__name__)

@app.route("/llm_response", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response
def llm_response():
    """
        Sends *hardcoded/predefined* messsages in response to received queries
        TODO: connect it to an LLM class => processQuery
    """
    try:
        #print("Processing the request  . . .")
        #===========================================================#
        #                    HARD-CODED Messages                    #
        #===========================================================#
        llm_output_HL = open("./msg/msg_hl.txt",'r').read()         #
        llm_output_LL = open("./msg/msg_ll.txt",'r').read()         #
        #===========================================================#
        
        req_body = request.get_data(as_text=True)       # 
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
        pass

    if "query_HL" in req_body_json:
        #print("Query : " + req_body_json["query"])
        #llm_output = processQuery(query)
        return{"response": "{}".format(llm_output_HL)}
    
    if "query_LL" in req_body_json:
        print("===========================================")
        return{"response": "{}".format(llm_output_LL)}

    return{"response": "{}".format("Invalid Response")}


@app.route("/plan", methods = ['POST', 'GET']) # endpoint  => localhost:5001/plan
def generatePlan():
    print("Processing the request  . . .")
    #os.system("python /../..")
    return Response(status=200)


if __name__ == "__main__":
    app.run(debug=True, port=5001)
