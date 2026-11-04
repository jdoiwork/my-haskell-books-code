# ANCHOR: all
# A Nix flake: `nix develop` gives the same tools on every machine.
# Nix の flake：`nix develop` で、どの機械でも同じ道具がそろう
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.haskell.compiler.ghc914
          pkgs.cabal-install
          pkgs.hlint
        ];
      };
    };
}
# ANCHOR_END: all
