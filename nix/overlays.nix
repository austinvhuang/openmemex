{
  inputs,
  cudaSupport,
  cudaMajorVersion,
}: let
  inherit (inputs) nix-filter;
  openmemex-commit = inputs.self.shortRev or "dirty";
  openmemex-date = inputs.self.lastModifiedDate or inputs.self.lastModified or "19700101";
  openmemex-version = "${cargoTOML.package.version}.${builtins.substring 0 8 openmemex-date}.${openmemex-commit}";

  cargoTOML = builtins.fromTOML (builtins.readFile ../frontend/Cargo.toml);
  WasmBindgenCargoVersion = cargoTOML.dependencies.wasm-bindgen.version;
  WasmBindgenVersion = builtins.substring 1 (builtins.stringLength WasmBindgenCargoVersion) WasmBindgenCargoVersion;
in {
  default = final: prev: rec {
    openmemex-frontend = prev.callPackage ../frontend {inherit openmemex-version nix-filter;};
    openmemex = prev.callPackage ./. {inherit cudaSupport cudaMajorVersion;};
  };
}
