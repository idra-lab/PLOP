import React from "react";
import "./subheader.css";

export default function Subheader(props) {
  return (
    <div className="subheader">
      <h1>{props.message}</h1>
    </div>
  );
}
