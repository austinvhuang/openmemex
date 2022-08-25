{
  inputs,
  cudaSupport,
  cudaMajorVersion ? null,
}:
# libtorch overlays from pytorch-world
final: prev:
with prev; let
  libtorchSrc = inputs.libtorch-nix.packages.${prev.system};
in
  if cudaSupport && cudaMajorVersion == "10"
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
