{
  inputs,
  cudaSupport,
  cudaMajorVersion,
}: let
  inherit (inputs) nix-filter;
  openmemex-commit = inputs.self.shortRev or "dirty";
  openmemex-date = inputs.self.lastModifiedDate or inputs.self.lastModified or "19700101";
  openmemex-version = "${builtins.substring 0 8 openmemex-date}.${openmemex-commit}";
in {
  default = final: prev: rec {
    npmlock2nix = import inputs.npmlock2nix {pkgs = prev;};
    openmemex-frontend = prev.callPackage ../frontend {inherit openmemex-version nix-filter;};
    openmemex = prev.callPackage ./. {inherit cudaSupport cudaMajorVersion;};
  };
}
