{
  nix-filter,
  rustPlatform,
  openssl,
  rust-bin,
  pkg-config,
  openmemex-version,
  lib,
  stdenv,
  wasm-pack,
}: let
  cargoTOML = builtins.fromTOML (builtins.readFile ./Cargo.toml);
  WasmBindgenCargoVersion = cargoTOML.dependencies.wasm-bindgen.version;
  WasmBindgenVersion = builtins.substring 1 (builtins.stringLength WasmBindgenCargoVersion) WasmBindgenCargoVersion;
  rust-env = rust-bin.stable.latest.default.override {
    extensions = ["rust-src"];
    targets = ["wasm32-unknown-unknown"];
  };
in
  rustPlatform.buildRustPackage {
    pname = "openmemex";
    version = openmemex-version;

    src = nix-filter.lib.filter {
      root = ./.;
      include = [
        (nix-filter.lib.inDirectory ./src)
        ./Cargo.lock
        ./Cargo.toml
      ];
    };

    buildInputs = [openssl wasm-pack];

    nativeBuildInputs = [
      rust-env
      pkg-config
    ];

    cargoLock.lockFile = ./Cargo.lock;

    doCheck = false;

    meta = with lib; {
      description = "Open source, local-first knowledge platform.";
      homepage = "https://github.com/austinvhuang/openmemex";
      license = "Apache-2.0";
      maintainers = [lib.maintainers.gtrunsec];
      platforms = lib.systems.doubles.all;
    };
  }
