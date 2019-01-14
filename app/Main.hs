{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.NNS
import Data.Metric
import Data.Point
import qualified Data.KdTree as KdTree
import qualified Data.CoverTree as CoverTree

knn :: (Searcher s, Metric (Pt s)) => Pt s -> s -> [(Pt s, Val s)]
knn = kNearestNeighbors 2

points = map Vector [[1, 2, 3], [2, 0, -1], [-3, 5, 0]] :: [Vector3d]
labels = [0, 1, 0]
entries = zip points labels
query = Vector [1, 0, 1] :: Vector3d

bruteForce = BruteForce entries
result1 = knn query bruteForce

kdTree = KdTree.fromList entries
result2 = knn query kdTree

coverTree = CoverTree.fromList 3 entries
isValid = CoverTree.checkInvariant coverTree
result3 = knn query coverTree

main :: IO ()
main = do
    let [a, b, c] = entries
    let t = CoverTree.singleton 7 a
    return ()
    
    
