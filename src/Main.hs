module Main where

import NN
import System.Random.Mersenne.Pure64

import Graphics.Gloss

import System.TimeIt

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main' :: IO ()
main' = display window background drawing


main :: IO ()
main = do
    demo
    {-}
  -- Create objects
  let net = createFullMeshNetwork 2 [2] 1
  let rng = pureMT 483209

  avgs <- benchmark net rng [] :: IO [Double]

  putStrLn ""
  putStrLn $ "Runs: " ++ show (length avgs)
  putStrLn $ "Total Time: " ++ show (sum avgs)
  putStrLn $ "Avg. Time: " ++ show (sum avgs / fromIntegral (length avgs))
  putStrLn ""

benchmark :: Network -> PureMT -> [Double] -> IO [Double]
benchmark net rng l = do
  -- Train the network
  (t, (trainedNet, rng')) <- timeItT $ trainNetWithGA net (\n -> -- Training data:
    outputDistance [0.0,0.0] [0.0] n +
    outputDistance [1.0,0.0] [1.0] n +
    outputDistance [0.0,1.0] [1.0] n +
    outputDistance [1.0,1.0] [0.0] n)
    0.2 -- multiplier
    50 -- chrildren
    1000 -- iterations
    1 -- threshold for early conversion detection
    rng

  if length l < 4 then benchmark net rng' $ t : l else return $ t : l-}




-- For demonstrational purposes, not actually used
demo :: IO ()
demo = do
  putStrLn "Beginning demonstration..."

  -- Create objects
  let net = createFullMeshNetwork 2 [2] 1
  rng <- newPureMT

  -- Train the network
  (trainedNet, _) <- timeIt $ trainNetWithGA net (\n -> -- Training data:
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
