module Main where

import NN
import System.Random.Mersenne.Pure64

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing


-- For demonstrational purposes, not actually used
main' :: IO ()
main' = do
  putStrLn "Beginning simulation..."

  -- Create objects
  let net = createFullMeshNetwork 2 [2] 1
  rng <- newPureMT

  -- Train the network
  (trainedNet, _) <- trainNetWithGA net (\n -> -- Training data:
    outputDistance [0.0,0.0] [0.0] n +
    outputDistance [1.0,0.0] [1.0] n +
    outputDistance [0.0,1.0] [1.0] n +
    outputDistance [1.0,1.0] [0.0] n)
    0.2 -- multiplier
    50 -- chrildren
    1000 -- iterations
    1 -- threshold for early conversion detection
    rng

  -- Test result
  putStrLn ""
  putStrLn ("0 XOR 0: " ++ show (round $ head (thinkNet trainedNet [0,0])))
  putStrLn ("1 XOR 0: " ++ show (round $ head (thinkNet trainedNet [1,0])))
  putStrLn ("0 XOR 1: " ++ show (round $ head (thinkNet trainedNet [0,1])))
  putStrLn ("1 XOR 1: " ++ show (round $ head (thinkNet trainedNet [1,1])))
  putStrLn ""

  -- Print network
  putStrLn "Network visualization:"
  putStrLn ""
  putStrLn $ printNetwork trainedNet
