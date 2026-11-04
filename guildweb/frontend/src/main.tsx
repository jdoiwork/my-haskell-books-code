// ANCHOR: all
// The entry point: put the App on the page, wrapped in Mantine's theme.
// 入口：App をページに置く。Mantine のテーマで包んで
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { MantineProvider, createTheme } from "@mantine/core";
import "@mantine/core/styles.css";

import { App } from "./App";

// 🎨 Violet like Lambda-kun, and rounder corners.
// ラムダくんの紫と、丸めの角
const theme = createTheme({
  primaryColor: "violet",
  defaultRadius: "md",
});

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <MantineProvider theme={theme} defaultColorScheme="auto">
      <App />
    </MantineProvider>
  </StrictMode>,
);
// ANCHOR_END: all
