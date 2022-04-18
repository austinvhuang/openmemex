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
    libtorch-nix = {
      url = "github:hasktorch/libtorch-nix";
      flake = false;
    };
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  } @ inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      cudaSupport = false;
      cudaMajorVersion = null;

      overlays = import ./nix/overlays.nix {inherit inputs cudaSupport cudaMajorVersion;};
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          overlays.default
          inputs.rust-overlay.overlay
          inputs.haskell-nix.overlay
          (import ./nix/libtorch.nix {inherit inputs cudaSupport cudaMajorVersion;})
        ];
        inherit (inputs.haskell-nix) config;
      };
    in
      (pkgs.lib.recursiveUpdate pkgs.openmemex {
        shell = import ./nix/shell.nix {inherit pkgs;};
        packages = {
          inherit
            (pkgs)
            openmemex-frontend
            ;
        };
      })
      // {
        # extra flake
      });
}
