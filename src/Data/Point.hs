{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Point where

import Data.List
import Data.Metric

-- A d-dimensional space with d axes.
class DimensionSpace a where
    -- number of coordinates of a point.
    dimension :: a -> Int
    -- get all available axes.
    axes :: a -> [a]
    -- get the base axis.
    baseAxis :: a -> a
    baseAxis = head . axes

-- A coordinate class with some statistical features
class (Ord c) => Coordinate c where
    average  :: [c] -> c
    variance :: [c] -> c

-- A point class mapping axies to coordinates.
class (DimensionSpace a, Coordinate c) => Point p a c | p -> a, p -> c where
    -- gets the point's k-th coordinate.
    coord :: a -> p -> c
    -- gets the Point's (axis, coordinate) pairs as lists
    toList :: p -> [(a, c)]

-- Vector space metrics
minkowskiMetric :: (Point p a Double) => Double -> p -> p -> Double
minkowskiMetric p a b =
    let diff = uncurry (-) <$> zip (snd <$> toList a) (snd <$> toList b)
        norm = (**p) . abs <$> diff
    in  (sum norm) ** (1/p)

manhattanMetric :: (Point p a Double) => p -> p -> Double
manhattanMetric = minkowskiMetric 1.0

euclideanMetric :: (Point p a Double) => p -> p -> Double
euclideanMetric = minkowskiMetric 2.0

-- Vector3d Definition

newtype Space3d = Space3d Int deriving (Eq, Ord, Show, Read)
instance DimensionSpace Space3d where
    dimension _ = 3
    axes _ = Space3d <$> [0, 1, 2]

instance Coordinate Double where
    average  xs = sum xs / genericLength xs
    variance xs = foldl (\v x -> v + (x - ax)^2) 0.0 xs / genericLength xs where ax = average xs

data Vector3d = Vector3d (Double, Double, Double) deriving (Eq, Ord, Show, Read)
instance Point Vector3d Space3d Double where
    coord (Space3d i) (Vector3d (x, y, z)) = [x, y, z] !! i
    toList (Vector3d (x, y, z)) = zip (Space3d <$> [0, 1, 2]) [x, y, z]

instance Metric Vector3d where
    distance = euclideanMetric
