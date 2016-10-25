module NN ( think
          , createFullMeshNetwork
          , printNetwork
          , thinkNet
          , mapNet'
          , mapConnections
          , mapInputNeurons
          , inputId
          , sigmoid
          , valueFromConnection
          , Connection (..)
          , Neuron (..) ) where

import           Data.List
import qualified Data.Map      as M
import           System.Random

data Connection = Connection { weight :: Float
                             , source :: Neuron }

data Neuron = Neuron { nid    :: String
                     , inputs :: [Connection] }
            | InputNeuron { nid   :: String
                          , value :: Float }

type Network = [Neuron]

-- | Trains a network using a genetic algorithm
trainNetWithGA :: Network -> ([Float] -> Float) -> Float -> Float -> Int -> Int -> StdGen -> Network
trainNetWithGA net _ _ _ _ 0 _ = net
trainNetWithGA net cost c prob ch iters rng =
    --let nets = mapConnections (\c -> c) net
    trainNetWithGA net cost c prob ch (iters-1) rng

thinkNet :: Network -> [Float] -> [Float]
thinkNet net i =
    let i' = M.fromList $ zip [0..length i] i
    in map think $ mapInputNeurons (\(InputNeuron nid _) -> InputNeuron {nid = nid, value = M.findWithDefault 0 (inputId nid) i'}) net

inputId :: String -> Int
inputId = read . tail

mapInputNeurons :: (Neuron -> Neuron) -> Network -> Network
mapInputNeurons f [] = []
mapInputNeurons f [Neuron nid inputs] = [Neuron {nid = nid, inputs = map (\(n, o) -> Connection {weight = weight o, source = n}) $ zip (mapInputNeurons f $ map source inputs) inputs}]
mapInputNeurons f [n] = [f n]
mapInputNeurons f (n:ns) = mapInputNeurons f [n] ++ mapInputNeurons f ns

mapNet :: (Neuron -> a) -> Network -> [a]
mapNet f [] = []
mapNet f [x] = f x : mapNet f (map source $ case x of
  Neuron _ i -> i
  _          -> [])
mapNet f (x:xs) = f x : mapNet f xs ++ mapNet f (map source $ case x of
  Neuron _ i -> i
  _          -> [])

mapNet' :: b -> ((Neuron, b) -> (Neuron, b)) -> Network -> (Network, b)
mapNet' s f [] = ([], s)
mapNet' s f [x] =
    let (res, nxt) = f (x, s)
        (a, b) = mapNet' nxt f (map source $ case res of
            Neuron _ i -> i
            _          -> [])
        a' = map (\(ai, si) -> Connection {weight = weight si, source = ai}) $ zip a (case res of
            Neuron _ i -> i
            _          -> [])
    in ([case res of
        Neuron nid inputs -> Neuron {nid = nid, inputs = a'}
        _                 -> res], b)
mapNet' s f (x:xs) =
    let (res, nxt) = f (x, s)
        (a, b) = mapNet' nxt f (map source $ case res of
            Neuron _ i -> i
            _          -> [])
        a' = map (\(aim, sim) -> Connection {weight = weight sim, source = aim}) $ zip a (case res of
            Neuron _ i -> i
            _          -> [])
        (res', nxt') = mapNet' b f xs
    in ((case res of
        Neuron nid inputs -> Neuron {nid = nid, inputs = a'}
        _                 -> res) : res', nxt')

mapNetOnce :: (Neuron -> a) -> Network -> [a]
mapNetOnce f net = map f $ nubBy (\x y -> nid x == nid y) $ mapNet id net

mapConnections :: a -> ((Connection, a) -> (Connection, a)) -> Network -> (Network, a)
mapConnections s f =
    mapNet' s (\(x, prev) -> case x of
        Neuron nid inputs ->
            let (cons, nxt) = modifyCons prev f inputs
            in (Neuron{nid = nid, inputs = cons}, nxt)
        _ -> (x, prev))
    where modifyCons :: a -> ((Connection, a) -> (Connection, a)) -> [Connection] -> ([Connection], a)
          modifyCons s f [] = ([], s)
          modifyCons s f [c] =
              let (c', nxt) = f (c, s)
              in ([c'], nxt)
          modifyCons s f (c:cs) =
              let (c', nxt) = f (c, s)
                  (cs', nxt') = modifyCons nxt f cs
              in (c' : cs', nxt')

think :: Neuron -> Float
think (InputNeuron _ value) = value
think (Neuron _ inputs)     = product $ map (sigmoid . valueFromConnection) inputs

e = exp 1 :: Float

sigmoid :: Float -> Float
sigmoid t = 1/(1+(e**(-t)))

valueFromConnection :: Connection -> Float
valueFromConnection (Connection w s) = w * think s

-- | Input, Hidden, Output
createFullMeshNetwork :: Int -> [Int] -> Int -> Network
createFullMeshNetwork inp [] output =
    let inputNeurons = map ((\x -> InputNeuron {nid = x, value = 0}) . (\x -> "i" ++ show x)) [1..inp]
    in  map (meshNeuron inputNeurons . (\x -> "1" ++ show x)) [1..output]
createFullMeshNetwork inp [h] output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp [] h)
            . (\x -> "2" ++ show x)
createFullMeshNetwork inp (h:hs) output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp hs h)
            . (\x -> show (length hs + 2) ++ show x)

meshNeuron :: [Neuron] -> String -> Neuron
meshNeuron sources name = Neuron {nid = name, inputs = map connectionFromNeuron sources}

connectionFromNeuron :: Neuron -> Connection
connectionFromNeuron n = Connection {weight = 1, source = n}

printNetwork :: Network -> String
printNetwork net = unlines $ mapNet show net

instance Show Neuron where
  show (Neuron n is)     = "Neuron: " ++ n ++ "\n" ++ unlines (map show is)
  show (InputNeuron n v) = "Input-Neuron: " ++ n ++ " = " ++ show v

instance Show Connection where
  show connection = "  <---" ++ show (weight connection) ++ "--- " ++ nid (source connection)
