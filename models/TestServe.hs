import Torch

main = do
  let x = randn' [2, 3]
  print x
  putStrLn "Done"