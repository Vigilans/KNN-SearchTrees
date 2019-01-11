{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
module Data.NNS where

import Data.Point
import Data.Metric
import Data.Maybe
import Data.Heap (MaxPrioHeap, insert, viewHead)
import qualified Data.Heap as Heap

class Searcher s where
    kNearestNeighbors :: (Point p a c, Metric p) => s -> Int -> p -> [p]
    -- nearNeighbors ::  (Point p a c, Metric p) => s -> Double -> p -> [p]

-- Brute force method with max k heap
-- newtype BruteForce p = BruteForce [p]
-- instance Searcher (BruteForce p) where
--     kNearestNeighbors (BruteForce examples) k sample =
--         let distExamples = map (\e -> (distance e sample, e)) examples
--             (initList, restList) = splitAt k distExamples
--             initHeap = Heap.fromList initList :: MaxPrioHeap Double p
--             minKHeap = foldl (\heap (dist, example) ->
--                 let maxDist = fromJust $ viewHead heap
--                 in if dist >= maxDist then heap
--                    else insert (dist example) (drop 1 heap)
--                 ) initHeap restList
--         in [sample]
