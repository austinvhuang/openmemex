{
  inputs,
  cudaSupport,
  cudaMajorVersion ? null,
}:
# libtorch overlays from pytorch-world
# TODO: pull in libGL_driver and cudatoolkit as done in https://github.com/NixOS/nixpkgs/blob/master/pkgs/games/katago/default.nix
final: prev:
with prev; let
  libtorchSrc = callPackage "${inputs.libtorch-nix}/libtorch/release.nix" {};
in
  if cudaSupport && cudaMajorVersion == "9"
  then let
    libtorch = libtorchSrc.libtorch_cudatoolkit_9_2;
  in {
    c10 = libtorch;
    torch = libtorch;
    torch_cpu = libtorch;
    torch_cuda = libtorch;
  }
  else if cudaSupport && cudaMajorVersion == "10"
  then let
    libtorch = libtorchSrc.libtorch_cudatoolkit_10_2;
  in {
    c10 = libtorch;
    torch = libtorch;
    torch_cpu = libtorch;
    torch_cuda = libtorch;
  }
  else if cudaSupport && cudaMajorVersion == "11"
  then let
    libtorch = libtorchSrc.libtorch_cudatoolkit_11_0;
  in {
    c10 = libtorch;
    torch = libtorch;
    torch_cpu = libtorch;
    torch_cuda = libtorch;
  }
  else let
    libtorch = libtorchSrc.libtorch_cpu;
  in {
    c10 = libtorch;
    torch = libtorch;
    torch_cpu = libtorch;
  }
