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

robertaTest :: Tensor -> IO (Tensor, Tensor)
robertaTest ids = do
  model <- load WithoutRequiredGrad "roberta_traced.zip"
  let IVTuple [IVTensor lastHiddenState, IVTensor poolerOutput] = Torch.Script.forward model [IVTensor ids]
  putStrLn $ "\nInput\n" ++ show ids
  let values = asValue $ lastHiddenState ! (0 :: Int) ! (0 :: Int) :: [Float]
  putStrLn $ "\nFirst 10 values\n" ++ show ids
  putStrLn $ show $ Prelude.take 10 values
  pure (lastHiddenState, poolerOutput)

-- token ids for "hi how are you doing. this is a test."
ids = asTensor([[ 0, 3592, 141, 32, 47, 608, 4, 42, 16, 10, 1296, 4, 2]] :: [[Int]])

main = do
  robertaTest ids
  pure ()
