{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Data.NNS where

import Data.Metric
import Data.Maybe
import qualified Data.Heap as Q

class Searcher s where
    type Pt s :: *
    type Val s :: *
    kNearestNeighbors :: (Metric (Pt s)) => Int -> s -> Pt s -> [(Pt s, Val s)]
    nearestNeighbor   :: (Metric (Pt s)) => s -> Pt s -> (Pt s, Val s)
    nearestNeighbor = (head .) . kNearestNeighbors 1
    -- nearNeighbors ::  (Metric p) => s p v -> Double -> p -> [(p, v)]

-- Brute force method with max k heap
newtype BruteForce p v = BruteForce [(p, v)]
instance Searcher (BruteForce p v) where
    type Pt (BruteForce p v) = p
    type Val (BruteForce p v) = v
    kNearestNeighbors :: (Metric p) => Int -> BruteForce p v ->  p -> [(p, v)]
    kNearestNeighbors k (BruteForce examples) sample =
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

