module NN ( think
          , createFullMeshNetwork
          , printNetwork
          , thinkNet
          , trainNetWithGA
          , trainNetWithGAFixedCostInput
          , outputDistance
          , outputDistanceFixed
          , Connection (..)
          , Neuron (..)
          , Network) where

import           Data.List
import qualified Data.Map      as M
import           System.Random.Mersenne.Pure64
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Float

data Connection = Connection { weight :: Float
                             , source :: Neuron }

data Neuron = Neuron { nid    :: B.ByteString
                     , inputs :: [Connection] }
            | InputNeuron { nid   :: B.ByteString
                          , value :: Float }

type Network = [Neuron]

outputDistance :: [Float] -> [Float] -> Network -> Float
outputDistance i oexp net =
  let oact = thinkNet net i
  in sum . map (abs . uncurry (-)) $ zip oexp oact

outputDistanceFixed :: [Float] -> Network -> Float
outputDistanceFixed oexp net =
  let oact = map think net
  in sum . map (abs . uncurry (-)) $ zip oexp oact

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

trainNetWithGA :: Network -> (Network -> Float) -> Float -> Int -> Int -> Float -> PureMT -> IO (Network, PureMT)
trainNetWithGA net = trainNetWithGAFixedCostInput net []

trainNetWithGAFixedCostInput :: Network -> [Float] -> (Network -> Float) -> Float -> Int -> Int -> Float -> PureMT -> IO (Network, PureMT)
trainNetWithGAFixedCostInput net ins cost prob ch iters th rng = do
  (done, net', rng') <- trainNetWithGA' [] net ins cost prob ch iters th rng
  if done
    then return (net', rng')
    else do
      putStrLn "Converged early, retrying..."
      trainNetWithGAFixedCostInput net ins cost prob ch iters th rng'

-- | Trains a network using a genetic algorithm
trainNetWithGA' :: [Float] -> Network -> [Float] -> (Network -> Float) -> Float -> Int -> Int -> Float -> PureMT -> IO (Bool, Network, PureMT)
trainNetWithGA' _ net _ _ _ _ 0 _ r = return (True, net, r)
trainNetWithGA' hist net ins cost prob ch iters th rng =
        let sameThreshold = iters `div` 4
        in if length hist > sameThreshold && head hist > th && allTheSame (take sameThreshold hist) then
              return (False, net, rng)
            else do -- Prevent early stagnation
              let prenet = case ins of
                            [] -> net
                            _ -> thinkNet' net ins
              let (net', rng') = genRands prenet prob ch ch rng -- Calculate
              let net'' = net : net' -- Add original network to prevent reverse progression
              let costlyNets = map (\x -> (x, cost x)) net''
              let bestNet = minimumBy (\(_, a) (_, b) -> compare a b) costlyNets
              when (iters `mod` 100 == 0) (putStrLn ("Iterations left: " ++ show iters ++ " (Current cost: " ++ show (snd bestNet) ++ ")")) -- Status output
              trainNetWithGA' (snd bestNet : hist) (fst bestNet) ins cost prob ch (iters-1) th rng' -- Recurse
    where genRands :: Network -> Float -> Int -> Int -> PureMT -> ([Network], PureMT)
          genRands net _ 0 _ r = ([net], r)
          genRands net p c cmax r =
            let (net', r') = mapConnections r (\(con, ri) ->
                  let (rval', ri') = randomDouble ri
                      rval = double2Float rval' * 2.0
                      calc = weight con + (rval-1.0)*p
                  in (Connection {weight = calc, source = source con}, ri')) net
                (mnet, r'') = genRands net p (c-1) cmax r'
            in (net' : mnet, r'')

thinkNet' :: Network -> [Float] -> Network
thinkNet' net i =
    let i' = M.fromList $ zip [1..] i
    in mapInputNeurons (\(InputNeuron nid _) -> InputNeuron {nid = nid, value = M.findWithDefault 0 (inputId nid) i'}) net

thinkNet :: Network -> [Float] -> [Float]
thinkNet net i =
    map think $ thinkNet' net i

inputId :: B.ByteString -> Int
inputId = readInt . B.tail

readInt :: B.ByteString -> Int
readInt bs = case B.readInt bs of
                Just (x, _) -> x
                Nothing -> error "This should never happen (input neuron number could not be parsed)"

mapInputNeurons :: (Neuron -> Neuron) -> Network -> Network
mapInputNeurons f [] = []
mapInputNeurons f [Neuron nid inputs] = [Neuron {nid = nid, inputs = map (\(n, o) -> Connection {weight = weight o, source = n}) $ zip (mapInputNeurons f $ map source inputs) inputs}]
mapInputNeurons f [n] = [f n]
mapInputNeurons f (n:ns) = head (mapInputNeurons f [n]) : mapInputNeurons f ns

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
think (Neuron _ inputs)     = sigmoid . sum $ map valueFromConnection inputs

e = exp 1 :: Float

sigmoid :: Float -> Float
sigmoid t = 1/(1+(e**(-t)))

valueFromConnection :: Connection -> Float
valueFromConnection (Connection w s) = w * think s

-- | Input, Hidden, Output
createFullMeshNetwork :: Int -> [Int] -> Int -> Network
createFullMeshNetwork inp [] output =
    let inputNeurons = map ((\x -> InputNeuron {nid = x, value = 0}) . (\x -> B.pack ("i" ++ show x))) [1..inp]
    in  map (meshNeuron inputNeurons . (\x -> "1" ++ show x)) [1..output]
createFullMeshNetwork inp [h] output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp [] h)
            . (\x -> "2" ++ show x)
createFullMeshNetwork inp (h:hs) output = map m [1..output]
  where m = meshNeuron (createFullMeshNetwork inp hs h)
            . (\x -> show (length hs + 2) ++ show x)

meshNeuron :: [Neuron] -> String -> Neuron
meshNeuron sources name = Neuron {nid = B.pack name, inputs = map connectionFromNeuron sources}

connectionFromNeuron :: Neuron -> Connection
connectionFromNeuron n = Connection {weight = 1, source = n}

printNetwork :: Network -> String
printNetwork net = unlines $ mapNetOnce show net

instance Show Neuron where
  show (Neuron n is)     = "Neuron: " ++ B.unpack n ++ "\n" ++ unlines (map show is)
  show (InputNeuron n v) = "Input-Neuron: " ++ B.unpack n ++ " = " ++ show v

instance Show Connection where
  show connection = "   <---" ++ show (weight connection) ++ "--- " ++ B.unpack (nid (source connection))
