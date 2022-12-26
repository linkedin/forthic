// TODO: Delete this after I incorporate the useEffect code into the global modul
import logo from './logo.svg';
import './App.css';
import {React, useEffect, useState} from 'react'

function App() {
  const [sample, setSample] = useState(null)

  console.log("Sample used")
  useEffect(() => {
    // TODO: If we use this, we need to sanitize the strings
    // TODO: Move this to the ELEMENT word
    setTimeout(() => setSample(eval("React.createElement(eval('Sample'), {})")), 0)
  }, [sample])

  function get_sample() {
    if (!sample) return
    return sample
  }

  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        {get_sample()}
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  );
}

export default App;
