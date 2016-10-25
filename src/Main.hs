module Main where

import NN

main :: IO ()
main = do
  let net = createFullMeshNetwork 2 [2] 1
  putStrLn $ printNetwork net
