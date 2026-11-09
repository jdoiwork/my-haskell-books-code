// ANCHOR: all
// Vite: while developing, send /api to `shelf serve` (the Haskell server).
// Vite：開発中は、/api を shelf serve（Haskell のサーバー）に回す
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: { "/api": "http://localhost:8080" },
  },
});
// ANCHOR_END: all
