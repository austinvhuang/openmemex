module Main where

import Torch
import Torch.Script as S

main = do

  -- load example data
  S.IVGenericDict example <- pickleLoad "traced_t5-small.example.pt"
  let [inputIDs , inputsMask, decoderIDs, decoderMask] = snd <$> example

  -- load torchscript module
  tsModule <- S.loadScript WithoutRequiredGrad "traced_t5-small.pt"
  
  -- perform inference computation
  let IVTuple [IVTensor r0, IVTuple r1, IVTensor r2] =
        forward tsModule [inputIDs, inputsMask, decoderIDs, decoderMask] 
    
  print $ r0

  putStrLn "Done"

