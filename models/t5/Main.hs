module Main where

import Torch
import Torch.Script as S

main = do

  -- load example data
  S.IVGenericDict example <- pickleLoad "traced_t5-small.example.pt"
  let inputIDs = snd $ example !! 0
  let inputsAttMask = snd $ example !! 1
  let decoderInputs = snd $ example !! 2
  let decoderAttMask = snd $ example !! 3

  -- load torchscript module
  tsModule <- S.loadScript WithoutRequiredGrad "traced_t5-small.pt"
  
  -- perform inference computation
  let IVTuple result = Torch.forward tsModule [inputIDs, inputsAttMask, decoderInputs, decoderAttMask] 
  let IVTensor res0 = result !! 0 
  let IVTuple res1 = result !! 1
  let res1' = (\(IVTuple x) -> x) <$> res1
  let IVTensor res2 = result !! 2
  print $ res0 ! 0 ! 0 ! 0
  -- print result

  putStrLn "Done"
