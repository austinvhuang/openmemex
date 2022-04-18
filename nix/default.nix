{
  haskell-nix,
  torch,
  cudaSupport,
  cudaMajorVersion,
}:
(haskell-nix.stackProject {
  src = ../.;
  compiler-nix-name = "ghc884";
  modules = [
    # Fixes for libtorch-ffi
    {
      packages.libtorch-ffi = {
        configureFlags = [
          "--extra-lib-dirs=${torch}/lib"
          "--extra-include-dirs=${torch}/include"
          "--extra-include-dirs=${torch}/include/torch/csrc/api/include"
        ];
        # flags = {
        #   cuda = cudaSupport;
        #   gcc = !cudaSupport && final.stdenv.hostPlatform.isDarwin;
        # };
      };
    }
  ];
})
.flake {}
