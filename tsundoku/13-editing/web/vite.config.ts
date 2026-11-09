// ANCHOR: all
// Vite: while developing, send /api to `shelf serve` (the Haskell server).
// TSUNDOKU_API says where it is, if not on port 8080.
// Vite：開発中は、/api を shelf serve（Haskell のサーバー）に回す。
// 8080 番でなければ、TSUNDOKU_API でどこかを言う
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

const api = process.env.TSUNDOKU_API ?? "http://localhost:8080";

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: { "/api": api },
  },
});
// ANCHOR_END: all
