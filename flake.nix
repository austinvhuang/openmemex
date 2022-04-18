{
  description = "Open source, local-first knowledge platform";

  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

    npmlock2nix.url = "github:tweag/npmlock2nix";
    npmlock2nix.flake = false;

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";

    hasktorch.url = "github:hasktorch/hasktorch";
    hasktorch.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";

    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  } @ inputs:
  # FIXME: ghc currently doesn't build on aarch64-darwin
    flake-utils.lib.eachSystem (nixpkgs.lib.remove "aarch64-darwin" flake-utils.lib.defaultSystems) (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          inputs.rust-overlay.overlay
        ];
        config = {allowBroken = true;};
      };
    in {
      devShells.default = import ./nix/shell.nix {inherit pkgs;};

      packages = {
        inherit
          (pkgs)
          openmemex-frontend
          ;
          inherit (pkgs.haskell.packages.ghc884)
            openmemex;
      };
    })
    // {
      overlays = import ./nix/overlays.nix {inherit inputs;};
    };
}
