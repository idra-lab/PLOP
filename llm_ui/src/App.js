import { useState } from 'react'; 
import 'bootstrap/dist/css/bootstrap.min.css';
import "./styles.css";

import Header from "./components/header/Header";
import Main from "./components/main/Main";
import Subheader from "./components/subheader/subheader";
import Footer from "./components/footer/Footer";
import FooterBottom from "./components/footerBottom/FooterBottom";
import FetchDimensions from "./useWindowDimensions"


function App(){
  
  const [query_HL, setQuery_HL]   = useState('');                         // Message to prompt
  const [query_LL, setQuery_LL]   = useState('');                         // Message to prompt
  
  const [response_LL, setResponse_LL] = useState('');                     // LLM response
  const [response_HL, setResponse_HL] = useState('');                     // LLM response
  
  //                          Error Handling - HL                         //
  const [selectedCategory_HL, setSelectedCategory_HL] = useState(null);   //
  const [selectedType_HL, setSelectedType_HL] = useState(null);           //
  const [fixedCode_HL, setFixedCode_HL] = useState('');                   //
  const [specification_HL, setSpecification_HL] = useState('');           // Specification of the error detected in HL-KB
  const [isValidKB_HL, setKBValidity_HL] = useState(true);                // Validity of the HL-KnowledgeBase
  const [isSolvable_HL, setSolvable_HL] = useState(false);                // Model Solvability - HL

  //                          Error Handling - LL                         //
  const [selectedCategory_LL, setSelectedCategory_LL] = useState(null);   //
  const [selectedType_LL, setSelectedType_LL] = useState(null);           //
  const [fixedCode_LL, setFixedCode_LL] = useState('');                   //
  const [specification_LL, setSpecification_LL]   = useState('');         // Specification of the error detected in LL-KB
  const [isValidKB_LL, setKBValidity_LL] = useState(true);                // Validity of the LL-KnowledgeBase
  const [isSolvable_LL, setSolvable_LL] = useState(false);                // Model Solvability - LL

  //============================================================================//
  //                      Error Descriptions - High Level                       //
  //============================================================================//
  const categories = ["Semantic error", "Syntax error"]; // Array of options
  const types = [ "Wrong number of predicate definitions",
                  "Mismatch between the Effects of the start action and the ValidConditions of the end action",
                  "Wrong number of literals in the predicates",
                  "Wrong number of arguments in the actions"];   // Array of options
  
  //============================================================================//
  //                      Error Descriptions - Low Level                        //
  //============================================================================//
  const categories_ll = ["Semantic error", "Syntax error"]; // Array of options
  const types_ll = ["Wrong mapping",
                    "Wrong number of predicate definitions",
                    "Mismatch between the Effects of the start action and the ValidConditions of the end action",
                    "Wrong number of literals in the predicates",
                    "Wrong number of arguments in the actions"]; // Array of options

  //============================================================================//
  //                            Window Dimensions                               //
  //============================================================================//
  const win_dim = FetchDimensions()                                             
  //console.log("Window dimensions : ", win_dim["width"])

  //============================================================================//
  //                            Handle Text Inputs                              //
  //============================================================================//
  // QUARY CHANGE
  const handleChangeQuery_HL = (event) => {
    setQuery_HL(event.target.value);
  };

  const handleChangeQuery_LL = (event) => {
    setQuery_LL(event.target.value);
  };

  // CATEGORY CHANGE
  const handleCategoryHLChange = (event) => {
    setSelectedCategory_HL(event.target.value);
  };

  const handleCategoryLLChange = (event) => {
    setSelectedCategory_LL(event.target.value);
  };

  // TYPE CHANGE
  const handleTypeHLChange = (event) => {
    setSelectedType_HL(event.target.value);
  };

  const handleTypeLLChange = (event) => {
    setSelectedType_LL(event.target.value);
  };

  // SPECIFICATION
  const handleSpecification_HL = (event) => {
    setSpecification_HL(event.target.value);
  };

  const handleSpecification_LL = (event) => {
    setSpecification_LL(event.target.value);
  };

  // FIX
  const handleFix_HL = (event) => {
    setFixedCode_HL(event.target.value);
  };

  const handleFix_LL = (event) => {
    setFixedCode_LL(event.target.value);
  };

  //============================================================================//
  //                            Handle Buttons                                  //
  //============================================================================//
  // Button: Submit - HL
  const handleClickSubmit_HL = () => {
  /*
    Submit user query
  */

    setResponse_HL("LLM is processing your query . . .")
    const prompt = {
      query_HL
    }

    fetch("/llm_response_hl",{
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      method: "POST",
      body: JSON.stringify(prompt)
    }).then(
      res => res.json()).then(
      data => {
        if(data.response !== undefined){
          setResponse_HL(data.response)
          //console.log("RESPONSE : " + data.response)
        }else
          setResponse_HL("LLM is not reachable !")
        }
    )
   };

  // Button: Submit - LL
  const handleClickSubmit_LL = () => {
    /*
      Submit user query
    */
      setResponse_LL("LLM is processing your query . . .")
      const prompt = {
        query_LL
      }
      fetch("/llm_response_ll",{
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify(prompt)
      }).then(
        res => res.json()).then(
        data => {
          if(data.response !== undefined){
            setResponse_LL(data.response)
            console.log("RESPONSE : " + data.response)
          }else
            setResponse_LL("LLM is not reachable !")
          }
      )
    };


  // Button: Complete - LL
  const handleClickComplete_HL = () => {
    setSolvable_HL(true);
  /*
    Sends high-level KB
  */
    const prompt = {
      kb_hl: fixedCode_HL
    }
    fetch("/kb_hl",{
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      method: "POST",
      body: JSON.stringify(prompt)
    })
  };

  // Button: Complete - LL
  const handleClickComplete_LL = () => {
    setSolvable_LL(true);
  /*
    Sends high-level KB
  */
    const prompt = {
      kb_ll: fixedCode_LL
    }
    fetch("/kb_ll",{
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      method: "POST",
      body: JSON.stringify(prompt)
    })
  };


  // Button: Fix - HL
  const handleClickFix_HL = () => {
    /*
      Submit user query
    */
      setResponse_HL("LLM is processing your query . . .")
      const prompt = {
        category_hl: selectedCategory_HL,
        type_hl: selectedType_HL,
        specification_hl: specification_HL
      }
      fetch("/llm_response_fix_hl",{
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify(prompt)
      }).then(
        res => res.json()).then(
        data => {
          if(data.response !== undefined){
            setResponse_HL(data.response)
            console.log("RESPONSE : " + data.response)
          }else
            setResponse_HL("LLM is not reachable !")
          }
      )
    };

  // Button: Fix - LL

  const handleClickFix_LL = () => {
    /*
      Submit user query
    */
      setResponse_LL("LLM is processing your query . . .")
      const prompt = {
        category_ll: selectedCategory_LL,
        type_ll: selectedType_LL,
        specification_ll: specification_LL
      }
      fetch("/llm_response_fix_ll",{
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify(prompt)
      }).then(
        res => res.json()).then(
        data => {
          if(data.response !== undefined){
            setResponse_LL(data.response)
            console.log("RESPONSE : " + data.response)
          }else
            setResponse_LL("LLM is not reachable !")
          }
      )
    };

  
  //========================================================//
  //                     VALIDITY CHECK                     //
  //========================================================//
  // HL
  const validKB_HL = () => {                               
    setKBValidity_HL(true)
    setFixedCode_HL(response_HL)
  };                                                      

  const invalidKB_HL = () => {
    setSolvable_HL(false);                           
    setKBValidity_HL(false)
    setFixedCode_HL("")
  };

  // LL
  const validKB_LL = () => {
    setKBValidity_LL(true)
    setFixedCode_LL(response_LL)
  };
  
  const invalidKB_LL = () => {                             
    setSolvable_LL(false)
    setKBValidity_LL(false)
    setFixedCode_LL("")
  };


  //=======================================================================================//
  //                              Error Handling - High-Level                              //
  //=======================================================================================//  
  // Component: HL-Error Categories
  function displayErrorCategories_HL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/8}}>
        <h2 style={{fontSize: "28px", color:"black"}}> Error Category : </h2>
          <div class="error-category">
            <select style={{marginRight:"55px", marginLeft:"50px", fontSize: '25px', height: "40px", width: "400px", borderRadius: '3px' }} value={selectedCategory_HL} onChange={handleCategoryHLChange}>
              <option value="">Select error category...</option>
              {categories.map((cat, index) => (
                <option key={index} value={cat}>{cat}</option>
              ))}
            </select>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }

  // Component: Error Types
  function displayErrorTypes_HL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/8}}>
        <h2 style={{fontSize: "28px", color: "black"}}> Error Type     : </h2>
          <div class="error-type">
            <select style={{marginRight:"55px", marginLeft:"105px", fontSize: '25px', height: "40px", width: "400px", borderRadius: '5px' }} value={selectedType_HL} onChange={handleTypeHLChange}>
              <option value="">Select error type...</option>
              {types.map((type, index) => (
                <option key={index} value={type}>{type}</option>
              ))}
            </select>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }    

  // Component: Error Specifications
  function displaySpecification_HL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/6.3}}>
          <h2 style={{fontSize: "28px", color: "black"}}> Specification     : </h2>
          <div class="prompt-holder">
            <textarea
              style={{height: win_dim["height"]/15, width: win_dim["width"]/4.5, fontSize: win_dim["width"]/175., fontWeight:"bolder", marginLeft: win_dim["width"]/60.}}
              type="text"
              id="query"
              name="query"
              onChange={handleSpecification_HL}
              value={specification_HL}
            />
          </div>
          <button class="btn btn-primary" style={{fontSize: "16px",  marginLeft: win_dim["width"]/60 }} onClick={handleClickFix_HL}>Fix</button>
        </div>
      );
    } else {
      return null;
    }
  }    


  
  //=======================================================================================//
  //                              Error Handling - Low-Level                               //
  //=======================================================================================//  
  // Component: HL-Error Categories
  function displayErrorCategories_LL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/8}}>
        <h2 style={{fontSize: "28px", color:"black"}}> Error Category : </h2>
          <div class="error-category">
            <select style={{marginRight:"55px", marginLeft:"50px", fontSize: '25px', height: "40px", width: "400px", borderRadius: '3px' }} value={selectedCategory_LL} onChange={handleCategoryLLChange}>
              <option value="">Select error category...</option>
              {categories_ll.map((cat, index) => (
                <option key={index} value={cat}>{cat}</option>
              ))}
            </select>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }

  // Component: Error Types
  function displayErrorTypes_LL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/8}}>
        <h2 style={{fontSize: "28px", color: "black"}}> Error Type     : </h2>
          <div class="error-type">
            <select style={{marginRight:"55px", marginLeft:"105px", fontSize: '25px', height: "40px", width: "400px", borderRadius: '5px' }} value={selectedType_LL} onChange={handleTypeLLChange}>
              <option value="">Select error type...</option>
              {types_ll.map((type, index) => (
                <option key={index} value={type}>{type}</option>
              ))}
            </select>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }    

  // Component: Error Specifications
  function displaySpecification_LL(isValidKB) {
    if(!isValidKB){
      return (
        <div class="container" style={{marginTop: win_dim["height"]/100, marginLeft: win_dim["width"]/6.3}}>
          <h2 style={{fontSize: "28px", color: "black"}}> Specification     : </h2>
          <div class="prompt-holder">
            <textarea
              style={{height: win_dim["height"]/15, width: win_dim["width"]/4.5, fontSize: win_dim["width"]/175., fontWeight:"bolder", marginLeft: win_dim["width"]/60.}}
              type="text"
              id="query"
              name="query"
              onChange={handleSpecification_LL}
              value={specification_LL}
            />
          </div>
          <button class="btn btn-primary" style={{fontSize: "16px",  marginLeft: win_dim["width"]/60 }} onClick={handleClickFix_LL}>Fix</button>
        </div>
      );
    } else {
      return null;
    }
  }    




  // Render
  return (
    <div style={{height: "auto", backgroundColor: "lightgray",  padding:"40px", marginTop: "-65px"}}>

      <Header title="PROLOG CODE GENERATOR" subtitle="for Bi-level Task and Motion Planning" />
      <Main message="High-Level Knowledge Base" />
      

      <div class="container" style={{backgroundColor:"#4181A4", width: win_dim["width"]/1.75, height: win_dim["height"]/5.5, borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/18}}>
        <h2 style={{textAlign: "center", color:"white"}}> High-Level Task Description</h2>
        <div class="prompt-holder">
          <textarea
            style={{height:win_dim["height"]/8,width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
            type="text"
            id="query"
            name="query"
            onChange={handleChangeQuery_HL}
            value={query_HL}
          />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px", marginTop: win_dim["height"]/12,  marginLeft: win_dim["width"]/80 }} onClick={handleClickSubmit_HL}>SUBMIT</button>
      </div>

      <div class="container" style={{backgroundColor:"#4181A4", width: win_dim["width"]/1.75, height: win_dim["height"]/4., borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/300.}}>
        <h2 style={{textAlign: "center", color:"white",marginLeft:"120px"}}> LLM Response </h2>
        <div class="prompt-holder">
        <textarea
          style={{height:win_dim["height"]/6, width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
          type="text"
          id="response"
          name="response"
          onChange={handleChangeQuery_HL}
          value={response_HL}
          readOnly="True"
        />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px" , marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/80 }} onClick={validKB_HL}> VALID </button>
        <button class="btn btn-primary" style={{fontSize: "16px" , marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/100 }} onClick={invalidKB_HL}> INVALID </button>
      </div>


      {displayErrorCategories_HL(isValidKB_HL)}
      {displayErrorTypes_HL(isValidKB_HL)}
      {displaySpecification_HL(isValidKB_HL)}


      <div class="container" style={{backgroundColor:"#4181A4", width: win_dim["width"]/1.75, height: win_dim["height"]/4., borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/300.}}>
        <h2 style={{color:"white", paddingLeft:"25px", textAlign:'center'}} > High-Level Knowledge Base </h2>
        <div class="prompt-holder">
        <textarea
          style={{height:win_dim["height"]/6, width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
          type="text"
          id="fixedCode"
          name="fixedCode"
          onChange={handleFix_HL}
          value={fixedCode_HL}
        />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px", marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/80 }} onClick={handleClickComplete_HL}>COMPLETE</button>
      </div>

      <Footer solvability={isSolvable_HL} validity={isValidKB_HL} />

      <Subheader message="Low-Level Knowledge Base" />
      <div class="container" style={{backgroundColor:"#308F9C", width: win_dim["width"]/1.75, height: win_dim["height"]/5.5, borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/14.}}>
        <h2 style={{textAlign: "center", color: "white"}}> Low-Level Task Description</h2>
        <div class="prompt-holder">
          <textarea
            style={{height:win_dim["height"]/8,width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
            type="text"
            id="query"
            name="query"
            onChange={handleChangeQuery_LL}
            value={query_LL}
          />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px", marginTop: win_dim["height"]/12,  marginLeft: win_dim["width"]/80 }} onClick={handleClickSubmit_LL}>SUBMIT</button>
      </div>

      <div class="container" style={{backgroundColor:"#308F9C", width: win_dim["width"]/1.75, height: win_dim["height"]/4., borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/300.}}>
        <h2 style={{marginLeft:"120px", textAlign: "center", color: "white"}}> LLM Response </h2>
        <div class="prompt-holder">
        <textarea
          style={{height:win_dim["height"]/6, width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
          type="text"
          id="response"
          name="response"
          onChange={handleChangeQuery_LL}
          value={response_LL}
          readOnly="True"
        />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px" , marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/80 }} onClick={validKB_LL}> VALID </button>
        <button class="btn btn-primary" style={{fontSize: "16px" , marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/100 }} onClick={invalidKB_LL}> INVALID </button>
      </div>


      {displayErrorCategories_LL(isValidKB_LL)}
      {displayErrorTypes_LL(isValidKB_LL)}
      {displaySpecification_LL(isValidKB_LL)}


      <div class="container" style={{backgroundColor:"#308F9C", width: win_dim["width"]/1.75, height: win_dim["height"]/4., borderStyle: "solid", borderRadius: win_dim["width"]/150., borderColor: "white", paddingRight: win_dim["width"]/5., paddingLeft: win_dim["width"]/6.5, borderWidth: win_dim["width"]/400., marginTop: win_dim["width"]/300.}}>
        <h2 style={{color:"white", paddingLeft:"25px", textAlign:'center'}} > Low-Level Knowledge Base </h2>
        <div class="prompt-holder">
        <textarea
          style={{height:win_dim["height"]/6, width: win_dim["width"]/3., fontSize: win_dim["width"]/175., fontWeight:"bolder",}}
          type="text"
          id="fixedCode"
          name="fixedCode"
          onChange={handleFix_LL}
          value={fixedCode_LL}
        />
        </div>
        <button class="btn btn-primary" style={{fontSize: "16px", marginTop: win_dim["height"]/8,  marginLeft: win_dim["width"]/80 }} onClick={handleClickComplete_LL}>COMPLETE</button>
      </div>

      <FooterBottom solvability={isSolvable_LL} validity={isValidKB_LL} />

    </div>
  );
}



export default App;