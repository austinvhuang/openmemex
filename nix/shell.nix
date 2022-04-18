{pkgs}: let
  rust-env = pkgs.rust-bin.stable.latest.default.override {
    extensions = ["rust-src"];
    targets = ["wasm32-unknown-unknown"];
  };
in {
  shell.tools = {
    cabal = {};
    hlint = {};
    haskell-language-server = {};
  };
  buildInputs = with pkgs; [
    rust-env
  ];
}
