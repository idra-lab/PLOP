import React from "react";
import "./FooterBottom.css";
import { MdVerifiedUser } from "react-icons/md";
import { BiSolidError } from "react-icons/bi";

export default function FooterBottom(props) {

  if(props.solvability && props.validity){
    return (
      <div className="footerBottom">
        <MdVerifiedUser />
        <text> Model Solvability (Low-Level) </text>
      </div>
    );
  }else{
    return (
      <div className="footerBottom">
          <BiSolidError />
          <text> Model Solvability (Low-Level) </text>
      </div>
    );    
  }

}