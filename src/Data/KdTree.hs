{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.KdTree where

import Data.Point
import Data.Metric
import Data.NNS
import Data.Ord
import Data.Maybe
import Data.List as L
import qualified Data.Heap as Q
import Debug.Trace

data KdNode a p v = Nil | KdNode {
    kdEntry :: (p, v),
    kdAxis  :: a,
    kdLeft  :: KdNode a p v,
    kdRight :: KdNode a p v
} deriving (Eq, Ord, Show, Read)

data KdTree a p v = KdTree {
    kdSize :: Int,
    kdRoot :: KdNode a p v
} deriving (Show, Read)

-- construct a balanced KdTree from a list
fromList :: (Point p a c, Metric p) => [(p, v)] -> KdTree a p v
fromList pvs = KdTree (length pvs) (buildNodes pvs)
    where buildNodes [] = Nil
          buildNodes pvs =
            let axis = selectAxis (fst <$> pvs)
                (left, midEntry, right) = medianPartition (comparing (coord axis . fst)) pvs
            in  KdNode midEntry axis (buildNodes left) (buildNodes right)

-- find the median element and partition using divide and conquer
medianPartition :: (b -> b -> Ordering) -> [b] -> ([b], b, [b])
medianPartition cmp xs = kthBiggest (length xs `div` 2) cmp xs
    where kthBiggest k cmp (x:xs)
            | k < n = let (l, m, r) = kthBiggest k cmp left in (l, m, r ++ x:right)
            | k > n = let (l, m, r) = kthBiggest (k - n - 1) cmp right in (left ++ x:l, m, r)
            | otherwise = (left, x, right)
            where (left, right) = partition ((== LT) . (`cmp` x)) xs
                  n = length left

-- arrange points in such a way that all coords in same axis in one list
coordsByAxis :: (Point p a c) => [p] -> [(a, [c])]
coordsByAxis = foldl zipPoint initial . map toList
    where zipPoint = zipWith (\(_, cs) (axis, c) -> (axis, c:cs))
          initial  = zip (axes (undefined :: a)) (repeat [])

-- median of the most spread dimension pivoting strategy
selectAxis :: (Point p a c, Metric p) => [p] -> a
selectAxis ps = fst $ maximumBy (comparing snd) vars
    where vars = map (\(axis, cs) -> (axis, variance cs)) $ coordsByAxis ps

-- search methods
-- instance Searcher (KdTree a) where
kNearestNeighbors :: (Point p a Double, Metric p, Show p, Show a, Show v) => KdTree a p v -> Int -> p -> [(p, v)]
kNearestNeighbors (KdTree _ root) k query =
    let serachByNode candidates Nil = candidates
        searchByNode candidates (KdNode (p, v) axis left right)
            | queryCoord <= pointCoord = searchBySubTrees newCands left right 
            | otherwise = searchBySubTrees newCands right left 
            where newCands = trace (show candidates) $ (distance p query, (p, v)) : candidates
                  queryCoord = coord axis query
                  pointCoord = coord axis p
                  searchBySubTrees candidates posSide negSide
                      | length newCands < k || inCircle = searchByNode newCands negSide
                      | otherwise = newCands
                      where newCands = searchByNode candidates posSide
                            inCircle = True--abs (queryCoord - pointCoord) < (fst $ head newCands) 
    in map snd $ kMinsByHeap k $ searchByNode ([] :: [(Double, (p, v))]) root
