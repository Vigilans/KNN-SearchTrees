module Data.KdTree where

import Data.Point
import Data.Metric
import Data.Ord
import Data.List
import Debug.Trace

data KdNode p a v = Nil | KdNode {
    kdEntry :: (p, v),
    kdAxis  :: a,
    kdLeft  :: KdNode p a v,
    kdRight :: KdNode p a v
} deriving (Eq, Ord, Show, Read)

data KdTree p a v = KdTree {
    size :: Int,
    root :: KdNode p a v
} deriving (Show, Read)

-- construct a balanced KdTree from a list
fromList :: (Point p a c, Metric p) => [(p, v)] -> KdTree p a v
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
