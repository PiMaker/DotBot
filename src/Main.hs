{-# LANGUAGE OverloadedStrings #-}

module Main where

import NN
import System.Random.Mersenne.Pure64

import Data.IORef

import System.TimeIt

main :: IO ()
main = do
    benchMain

benchMain :: IO ()
benchMain = do
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

  if length l < 9 then benchmark net rng' $ t : l else return $ t : l
