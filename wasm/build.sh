#!/bin/sh
# ANCHOR: all
# Build the WebAssembly module and the JavaScript glue for it.
# WebAssembly のモジュールと、そのための JavaScript のつなぎを作る
set -eu
cd "$(dirname "$0")"
mkdir -p build
wasm32-wasi-ghc -i../modern/src -outputdir build Matchup.hs \
  -o build/matchup.wasm \
  -no-hs-main -optl-mexec-model=reactor \
  -optl-Wl,--export=matchup,--export=hello
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
  -i build/matchup.wasm -o build/ghc_wasm_jsffi.js
# ANCHOR_END: all
