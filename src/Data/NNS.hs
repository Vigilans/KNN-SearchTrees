{-# LANGUAGE ScopedTypeVariables #-}
module Data.NNS where

import Data.Point
import Data.Metric
import Data.Maybe
import qualified Data.Heap as Q

class Searcher s where
    kNearestNeighbors :: forall p v. (Metric p) => s p v -> Int -> p -> [(p, v)]
    -- nearNeighbors ::  (Metric p) => s p v -> Double -> p -> [(p, v)]

-- Brute force method with max k heap
newtype BruteForce p v = BruteForce [(p, v)]

instance Searcher BruteForce where
    kNearestNeighbors (BruteForce examples) k sample =
        let distExamples = map (\(p, v) -> (distance p sample, (p, v))) examples
        in snd <$> kMinsByHeap k distExamples

kMinsByHeap :: forall prio val. (Ord prio) => Int -> [(prio, val)] -> [(prio, val)]
kMinsByHeap k prioVals = Q.toAscList maxKHeap where
    (initList, restList) = splitAt k prioVals
    initHeap = Q.fromList initList :: Q.MaxPrioHeap prio val
    maxKHeap = foldl (\heap (prio, val) ->
            let (maxPrio, _) = fromJust $ Q.viewHead heap
            in if prio >= maxPrio then heap
                else Q.insert (prio, val) (Q.drop 1 heap)
        ) initHeap restList
