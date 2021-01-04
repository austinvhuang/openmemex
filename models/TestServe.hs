import Torch

main = do
  x <- randIO' [2, 3]
  print x
  putStrLn "Done"