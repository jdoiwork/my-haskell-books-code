// ANCHOR: all
// Run the Haskell WebAssembly module in Node.js and call it.
// Haskell の WebAssembly のモジュールを Node.js で動かして、呼んでみる
import { WASI } from "node:wasi";
import { readFile } from "node:fs/promises";
import ghc_wasm_jsffi from "./build/ghc_wasm_jsffi.js";

// The Haskell runtime wants a program name in args.
// Haskell のランタイムは、args にプログラムの名前を欲しがる
const wasi = new WASI({ version: "preview1", args: ["matchup"], env: {} });
// The JSFFI glue needs the exports, which exist only after instantiating.
// JSFFI のつなぎは、作ったあとにできる exports を必要とする
const exports = {};
const { instance } = await WebAssembly.instantiate(
  await readFile(new URL("./build/matchup.wasm", import.meta.url)),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(exports),
  },
);
Object.assign(exports, instance.exports);
wasi.initialize(instance);

console.log(instance.exports.matchup("fire", "grass"));
console.log(instance.exports.matchup("fire", "water"));
console.log(instance.exports.matchup("light", "dark"));
await instance.exports.hello();
// ANCHOR_END: all
