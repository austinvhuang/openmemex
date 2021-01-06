module Main where

import Torch
import Torch.Script 

main = do
  model <- load WithoutRequiredGrad "traced.zip"
  let x               = asTensor ([[1.0, 2.0]] :: [[Float]])
      IVTensor result = Torch.Script.forward model [IVTensor x]
      expected        = Torch.matmul (asTensor ([[2.0, 3.0]] :: [[Float]])) (transpose2D x)
  putStrLn $ "\nInput\n" ++ show x
  putStrLn $ "\nTorscript Inference\n" ++ show result
  putStrLn $ "\nExpected\n" ++ show expected
