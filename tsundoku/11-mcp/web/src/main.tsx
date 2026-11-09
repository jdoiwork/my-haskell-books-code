// ANCHOR: all
// The entry point: put the App on the page.
// 入口：App をページに置く
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";

import { App } from "./App";
import "./shelf.css";

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <App />
  </StrictMode>,
);
// ANCHOR_END: all
