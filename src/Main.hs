module Main where

import NN
import System.Random.Mersenne.Pure64
import Control.Monad

main :: IO ()
main = do
  putStrLn "Commencing NN evaluation..."
  let net = createFullMeshNetwork 2 [2] 1
  rng <- newPureMT

  result <- test 500 net rng

  putStrLn result

test :: Int -> Network -> PureMT -> IO String
test 0 _ _ = return "Done!"
test i net rng = do
  -- Train the network
  (trainedNet, rng') <- trainNetWithGA net (\n -> -- Training data:
    outputDistance [0.0,0.0] [0.0] n +
    outputDistance [1.0,0.0] [1.0] n +
    outputDistance [0.0,1.0] [1.0] n +
    outputDistance [1.0,1.0] [0.0] n)
    0.2 -- multiplier
    80 -- chrildren
    1000 -- iterations
    1 -- threshold for early conversion detection
    rng

  let x1 = round $ head (thinkNet trainedNet [0,0])
  let x2 = round $ head (thinkNet trainedNet [1,0])
  let x3 = round $ head (thinkNet trainedNet [0,1])
  let x4 = round $ head (thinkNet trainedNet [1,1])

  if x1 /= 0 || x2 /= 1 || x3 /= 1 || x4 /= 0 then
    return $ "Validation failed at: " ++ show rng
  else do
    putStrLn ("==== Test complete, remaining: " ++ show i)
    test (i-1) net rng'





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
