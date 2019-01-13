module Main where

import Data.NNS
import Data.Point
import qualified Data.KdTree as KdTree
import qualified Data.CoverTree as CoverTree

points = Vector3d <$> [(1, 2, 3), (2, 0, -1), (-3, 5, 0)]
labels = [0, 1, 0]
entries = zip points labels
query = Vector3d (1, 0, 1)

bruteForce = BruteForce entries
result1 = kNearestNeighbors bruteForce 2 query

kdTree = KdTree.fromList entries
result2 = KdTree.kNearestNeighbors kdTree 2 query 

coverTree = CoverTree.fromList 10 entries
result3 = kNearestNeighbors coverTree 2 query 


main :: IO ()
main = do
    let [a, b, c] = entries
    let t = CoverTree.singleton 7 a
    return ()
    
    
