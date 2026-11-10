// ANCHOR: all
// The entry point: put the App on the page, wrapped in the shelf's
// Mantine theme. Mantine's own styles come first (core, then charts).
// 入口：App をページに置く。本棚の Mantine のテーマで包んで。Mantine 自身の
// スタイルを先に読む（core、それから charts）
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { MantineProvider } from "@mantine/core";
import "@mantine/core/styles.css";
import "@mantine/charts/styles.css";

import { App } from "./App";
import { theme } from "./theme";

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <MantineProvider theme={theme} defaultColorScheme="auto">
      <App />
    </MantineProvider>
  </StrictMode>,
);
// ANCHOR_END: all
