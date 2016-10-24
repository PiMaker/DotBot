module NN ( think
          , createFullMeshNetwork
          , Connection (..)
          , Neuron (..) ) where

data Connection = Connection { weight :: Float
                             , source :: Neuron }

data Neuron = Neuron { nid :: String
                     , inputs :: [Connection] }
            | InputNeuron { nid :: String
                          , value :: Float }

type Network = [Neuron]

think :: Neuron -> Float
think (InputNeuron _ value) = value
think (Neuron _ inputs) = sum $ map valueFromConnection inputs

valueFromConnection :: Connection -> Float
valueFromConnection (Connection w s) = w * think s

-- Input, Hidden, Output
createFullMeshNetwork :: Int -> [Int] -> Int -> Network
createFullMeshNetwork inp [] output =
    let inputNeurons = map ((\x -> InputNeuron {nid = x, value = 0}) . (\x -> show $ "i" ++ show x)) [0..inp]
    in  map (meshNeuron inputNeurons . (\x -> show $ "1" ++ show x)) [0..output]
createFullMeshNetwork inp [h] output = map (meshNeuron (createFullMeshNetwork inp [] h) . (\x -> "2" ++ show x)) [0..output]
createFullMeshNetwork inp (h:hs) output = map (meshNeuron (createFullMeshNetwork inp hs h) . (\x -> show (length hs + 2) ++ show x)) [0..output]

meshNeuron :: [Neuron] -> String -> Neuron
meshNeuron sources name = Neuron {nid = name, inputs = map connectionFromNeuron sources}

connectionFromNeuron :: Neuron -> Connection
connectionFromNeuron n = Connection {weight = 1, source = n}

instance Show Network where
    show (neuron:net) = "Neuron: " ++ nid neuron ++ "\n" ++ show
