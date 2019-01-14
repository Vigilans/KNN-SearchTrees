{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
fromList :: (Point p a c, Metric (p a c)) => [(p a c, v)] -> KdTree a (p a c) v
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
coordsByAxis :: (Point p a c) => [p a c] -> [(a, [c])]
coordsByAxis = foldl zipPoint initial . map toList
    where zipPoint = zipWith (\(_, cs) (axis, c) -> (axis, c:cs))
          initial  = zip (axes (undefined :: a)) (repeat [])

-- median of the most spread dimension pivoting strategy
selectAxis :: (Point p a c, Metric (p a c)) => [p a c] -> a
selectAxis ps = fst $ maximumBy (comparing snd) vars
    where vars = map (\(axis, cs) -> (axis, variance cs)) $ coordsByAxis ps

-- search methods
instance (Point p a c) => Searcher (KdTree a (p a c) v) where
    type Pt (KdTree a (p a c) v) = p a c
    type Val (KdTree a (p a c) v) = v

    kNearestNeighbors :: (Metric (p a c)) => Int -> p a c -> KdTree a (p a c) v -> [(p a c, v)]
    kNearestNeighbors k query (KdTree _ root) =
        let searchByNode candidates Nil = candidates
            searchByNode candidates (KdNode (p, v) axis left right)
                | queryCoord <= pointCoord = searchBySubTrees newCands left right 
                | otherwise = searchBySubTrees newCands right left
                where 
                    newCands = (distance p query, (p, v)):candidates
                    queryCoord = coord axis query
                    pointCoord = coord axis p
                    searchBySubTrees candidates' posSide negSide
                        | length newCands < k || inCircle = searchByNode newCands negSide
                        | otherwise = newCands
                        where 
                            newCands = searchByNode candidates' posSide
                            inCircle = absDiff queryCoord pointCoord < fst (head newCands) 
        in map snd $ kMinsByHeap k $ searchByNode [] root
