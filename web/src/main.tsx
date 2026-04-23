/* @refresh reload */
import { render } from "solid-js/web";
import "./styles/global.css";
import "./styles/arrangement.css";
import "./styles/detail.css";
import App from "./App";

const root = document.getElementById("app");
if (!root) throw new Error("Root element #app not found");
render(() => <App />, root);
