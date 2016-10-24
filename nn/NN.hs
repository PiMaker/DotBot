module NN ( think
          , createFullMeshNetwork
          , printNetwork
          , thinkNet
          , Connection (..)
          , Neuron (..) ) where

import Data.List

data Connection = Connection { weight :: Float
                             , source :: Neuron }

data Neuron = Neuron { nid    :: String
                     , inputs :: [Connection] }
            | InputNeuron { nid   :: String
                          , value :: Float }

type Network = [Neuron]

-- | TODO
thinkNet :: Network -> [Float] -> [Float]
thinkNet net i = [0]

mapNet :: (Neuron -> a) -> Network -> [a]
mapNet f [] = []
mapNet f [x] = f x : mapNet f (map source $ case x of
  Neuron _ i -> i
  _ -> [])
mapNet f (x:xs) = f x : mapNet f xs ++ mapNet f (map source $ case x of
  Neuron _ i -> i
  _ -> [])

mapNetOnce :: (Neuron -> a) -> Network -> [a]
mapNetOnce f net = map f $ nubBy (\x y -> nid x == nid y) $ mapNet id net

think :: Neuron -> Float
think (InputNeuron _ value) = value
think (Neuron _ inputs)     = sum $ map valueFromConnection inputs

valueFromConnection :: Connection -> Float
valueFromConnection (Connection w s) = w * think s

-- | Input, Hidden, Output
createFullMeshNetwork :: Int -> [Int] -> Int -> Network
createFullMeshNetwork inp [] output =
    let inputNeurons = map ((\x -> InputNeuron {nid = x, value = 0}) . (\x -> show $ "i" ++ show x)) [1..inp]
    in  map (meshNeuron inputNeurons . (\x -> show $ "1" ++ show x)) [1..output]
createFullMeshNetwork inp [h] output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp [] h)
            . (\x -> show ("2" ++ show x))
createFullMeshNetwork inp (h:hs) output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp hs h)
            . (\x -> show $ show (length hs + 2) ++ show x)

meshNeuron :: [Neuron] -> String -> Neuron
meshNeuron sources name = Neuron {nid = name, inputs = map connectionFromNeuron sources}

connectionFromNeuron :: Neuron -> Connection
connectionFromNeuron n = Connection {weight = 1, source = n}

printNetwork :: Network -> String
printNetwork net = unlines $ mapNetOnce show net

instance Show Neuron where
  show (Neuron n is) = "Neuron: " ++ n ++ "\n" ++ unlines (map show is)
  show (InputNeuron n v) = "Input-Neuron: " ++ n ++ " = " ++ show v

instance Show Connection where
  show connection = "  <---" ++ show (weight connection) ++ "--- " ++ nid (source connection)
