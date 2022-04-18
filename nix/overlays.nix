{inputs}: let
  inherit (inputs) nix-filter;
  openmemex-commit = inputs.self.shortRev or "dirty";
  openmemex-date = inputs.self.lastModifiedDate or inputs.self.lastModified or "19700101";
  openmemex-version = "${builtins.substring 0 8 openmemex-date}.${openmemex-commit}";
  compiler = "ghc884";
in {
  default = final: prev: rec {
    npmlock2nix = import inputs.npmlock2nix {pkgs = prev;};
    openmemex-frontend = prev.callPackage ../frontend {inherit openmemex-version nix-filter;};

    haskell = prev.haskell // {
      packages = prev.haskell.packages // {
        "${compiler}" = prev.haskell.packages."${compiler}".override (old:{
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
            openmemex = with final.haskell.lib;
              generateOptparseApplicativeCompletion "openmemex"
                (overrideCabal (hsuper.callPackage ./. { })
                  (drv: {
                    executableHaskellDepends = drv.executableHaskellDepends or [ ] ++ [];
              }));
          });
        });
      };
    };
    openmemex-bin = with final;
      haskell.lib.justStaticExecutables haskellPackages.openmemex;
  };
}
