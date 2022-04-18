{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  rust-env = rust-bin.stable.latest.default.override {
    extensions = ["rust-src"];
    targets = ["wasm32-unknown-unknown"];
  };
in
  mkShell {
    buildInputs = [
      rust-env
      (haskell.packages.ghc884.ghcWithPackages
      (p: with p;  [
        stack
        cabal-install
        cabal2nix
      ]))
      openssl
      pkg-config
      wasm-pack
      wasm-bindgen-cli
    ];
  }
