import React from 'react';
import { render } from 'react-dom';
import MonacoEditor from 'react-monaco-editor';

const axios = require('axios');

const initialResponse = [[], {
            allToML: null,
            allToSexp: null,
            allToHR: null,
            allToIR: null,
            allToErased: null,
            allToBackend: null
        }]
class Editor extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      code: `mod MainMultArgs where

open Prelude
open Circuit

sig main : field
let main = 5
`,
        currentView: "allToML",
        response: initialResponse
    }
  }
  editorDidMount(editor, monaco) {
    console.log('editorDidMount', editor);
    editor.focus();
  }
  onChange(newValue, e) {
    console.log('onChange', newValue, e);
    this.setState({ code: newValue })
  }
  parse() {
    console.log(this.state.code)
    axios.post('http://localhost:3001/pipeline/parse',
        JSON.stringify({ reqBody: this.state.code }),
        { headers: {
            'Content-Type': 'application/json',
        },
        })
      .then((response) => {
        this.setState({response: response.data})
      })
      .catch((error) => {
        console.log(error);
        this.setState({response: [["Invalid script: Fatal error"], initialResponse]})
      });
  }
  typecheck() {
    console.log(this.state.code)
    axios.post('http://localhost:3001/pipeline/typecheck',
        JSON.stringify({ reqBody: this.state.code }),
        { headers: {
            'Content-Type': 'application/json',
        },
        })
      .then((response) => {
        this.setState({response: response.data})
      })
      .catch((error) => {
        console.log(error);
        this.setState({response: [["Invalid script: Fatal error"], initialResponse]})
      });
  }
  compile() {
    console.log(this.state.code)
    axios.post('http://localhost:3001/pipeline/compile',
        JSON.stringify({ reqBody: this.state.code }),
        { headers: {
            'Content-Type': 'application/json',
        },
          responseType: 'json', // default
        })
      .then((response) => {
        this.setState({response: response.data})
      })
      .catch((error) => {
        console.log(error);
        this.setState({response: [["Invalid script: Fatal error"], initialResponse]})
      });
  }
  render() {
    const code = this.state.code;
    const options = {
      selectOnLineNumbers: true
    };
    const errorMsg = () => {
        if (this.state.response[0].length > 0) {
                return <div>{
                    this.state.response[0][0]
                    //     .split('\n').map(str => <p>{str}</p>)
                        }
                    </div>
            }
            else <div></div>
    }
    return (
        <div style={{display: "flex", justifyContent: "space-around"}}>
            <div style={{flex: 1}}>
                <button onClick={(e) => this.parse()}> Parse</button>
                <button onClick={(e) => this.typecheck()}> Typecheck</button>
                <button onClick={(e) => this.compile()}> Compile</button>
                <MonacoEditor
                    width="100%"
                    height="100%"
                    language="javascript"
                    theme="vs-dark"
                    value={code}
                    options={options}
                    onChange={(v, e) => this.onChange(v, e)}
                    editorDidMount={(editor, monaco) => this.editorDidMount(editor, monaco)}
                />
            </div>
            <div style={{flex: 1}}>
                <button style={{background:this.state.response[1]["allToML"] ? "#cddc39": "#e87067"}} onClick={(e) => this.setState({currentView: "allToML"})}> ML</button>
                <button style={{background:this.state.response[1]["allToSexp"] ? "#cddc39": "#e87067"}}  onClick={(e) => this.setState({currentView: "allToSexp"})}> Sexp</button>
                <button style={{background:this.state.response[1]["allToHR"] ? "#cddc39": "#e87067"}} onClick={(e) => this.setState({currentView: "allToHR"})}> HR</button>
                <button style={{background:this.state.response[1]["allToIR"] ? "#cddc39": "#e87067"}} onClick={(e) => this.setState({currentView: "allToIR"})}> IR</button>
                <button style={{background:this.state.response[1]["allToErased"] ? "#cddc39": "#e87067"}} onClick={(e) => this.setState({currentView: "allToErased"})}> Erased</button>
                <button style={{background:this.state.response[1]["allToBackend"] ? "#cddc39": "#e87067"}}  onClick={(e) => this.setState({currentView: "allToBackend"})}> Backend</button>
            {errorMsg()}
            <textarea
                readOnly
                style={{height:"100vh", width:"100%"}}
                value={this.state.response[1][this.state.currentView] ? 
                            JSON.stringify(this.state.response[1][this.state.currentView], null, 2) : "" }
            />
            </div>
        </div>

    );
  }
}

export {Editor}