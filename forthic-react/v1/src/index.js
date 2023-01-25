import React from 'react';
import ReactDOM from 'react-dom/client';
import 'bootstrap/dist/css/bootstrap.min.css';
import './index.css'
import {RouterProvider} from "react-router-dom";
import RechartsModule from "./forthic/modules/recharts_module"
import { Interpreter } from './forthic/interpreter';

let INTERP;

// TODO: Read this from someplace during development
let forthic = ""

export function get_forthic() {
  if (process.env.NODE_ENV === 'development') {
      return forthic
  }
  else {
      return window.FORTHIC
  }
}
export async function get_interp() {
  if (INTERP)   return INTERP

  let result = new Interpreter()
  let forthic = get_forthic()
  await result.register_module(new RechartsModule())
  // await result.register_module(new TableModule())
  await result.run(forthic)
  INTERP = result
  return result
}

setTimeout(async () => {
  let interp = await get_interp()
  await interp.run(`MAIN-ROUTER`)
  let router = interp.stack_pop()
  const root = ReactDOM.createRoot(document.getElementById('root'));
  root.render(
    <React.StrictMode>
        <RouterProvider router={router} />
    </React.StrictMode>
  );
})
