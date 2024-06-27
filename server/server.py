#from llm_model import processQuery
import os
from flask import Flask, request, Response
import json


app = Flask(__name__)

@app.route("/llm_response_hl", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_hl
def llm_response_hl():
    """
        generates high-level knowledge base in response to the query sent via web-UI
    """
    try:
        #===========================================================#
        #                    HARD-CODED Messages                    #
        #===========================================================#
        llm_output_HL = open("./msg/msg_hl.txt",'r').read()         #
        #===========================================================#
        req_body = request.get_data(as_text=True)
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    except:
        pass

    if "query_HL" in req_body_json:
        #print("Query : " + req_body_json["query"])
        #llm_output = processQuery(query)
        return{"response": "{}".format(llm_output_HL)}

    return{"response": "{}".format("Invalid Response")}



@app.route("/llm_response_ll", methods = ['POST', 'GET']) # endpoint  => localhost:5001/llm_response_ll
def llm_response_ll():
    """
        generates low-level knowledge base in response to the query sent via web-UI
    """    
    try:
        #===========================================================#
        #                    HARD-CODED Messages                    #
        #===========================================================#
        llm_output_LL = open("./msg/msg_ll.txt",'r').read()         #
        #===========================================================#
        req_body = request.get_data(as_text=True)
        #print("Query: ", req_body)
        req_body_json = json.loads(req_body)
    
    except:
        pass
    
    if "query_LL" in req_body_json:
        print("===========================================")
        return{"response": "{}".format(llm_output_LL)}

    return{"response": "{}".format("Invalid Response")}



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
    app.run(debug=True, port=5001)
