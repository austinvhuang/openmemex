module Main where

import Torch
import Torch.Script 

mlpTest = do
  model <- load WithoutRequiredGrad "traced.zip"
  let x               = asTensor ([[1.0, 2.0]] :: [[Float]])
      IVTensor result = Torch.Script.forward model [IVTensor x]
      expected        = Torch.matmul (asTensor ([[2.0, 3.0]] :: [[Float]])) (transpose2D x)
  putStrLn $ "\nInput\n" ++ show x
  putStrLn $ "\nTorscript Inference\n" ++ show result
  putStrLn $ "\nExpected\n" ++ show expected

robertaTest :: IO ()
robertaTest = do
  model <- load WithoutRequiredGrad "roberta_traced.zip"
  let ids = asTensor([[ 0, 3592, 141, 32, 47, 608, 4, 42, 16, 10, 1296, 4, 2]] :: [[Int]])
  let IVTensor result = Torch.Script.forward model [IVTensor ids] -- error this should be a tuple
  -- let IVTuple (IVTensor lastHiddenState) (IVTensor poolerOutput) = Torch.Script.forward model [IVTensor ids]
  putStrLn $ "\nInput\n" ++ show ids
  putStrLn $ "\nTorscript Inference\n" ++ show result
            
  pure ()

main = robertaTest
