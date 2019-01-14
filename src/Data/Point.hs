{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Point where

import Data.List
import Data.Metric

-- A d-dimensional space with d axes.
class (Enum a, Bounded a) => DimensionSpace a where
    -- number of coordinates of a point.
    dimension :: a -> Int
    -- get all available axes.
    axes :: a -> [a]
    -- get the base axis.
    baseAxis :: a -> a
    baseAxis = head . axes

-- A Coordinate class with some statistical features
class (Ord c) => Coordinate c where
    average  :: [c] -> c
    variance :: [c] -> c
    absDiff :: c -> c -> Double

-- A point class mapping axies to coordinates.
class (DimensionSpace a, Coordinate c) => Point p a c where
    -- gets the point's k-th Coordinate.
    coord :: a -> p a c -> c
    -- gets the Point's (axis, Coordinate) pairs as lists
    toList :: p a c -> [(a, c)]

-- Vector space metrics
minkowskiMetric :: (Point p a c) => Double -> p a c -> p a c -> Double
minkowskiMetric p a b =
    let coordPairs = zip (snd <$> toList a) (snd <$> toList b)
        normedDiff = (**p) . uncurry absDiff <$> coordPairs 
    in  (sum normedDiff) ** (1/p)

manhattanMetric :: (Point p a c) => p a c -> p a c -> Double
manhattanMetric = minkowskiMetric 1.0

euclideanMetric :: (Point p a c) => p a c -> p a c -> Double
euclideanMetric = minkowskiMetric 2.0

-- -- Vector3d Definition

data Space3d = X3d | Y3d | Z3d deriving (Eq, Ord, Enum, Bounded, Show, Read)
instance DimensionSpace Space3d where
    dimension _ = 3
    axes _ = [X3d, Y3d, Z3d]

instance Coordinate Double where
    average  xs = sum xs / genericLength xs
    variance xs = foldl (\v x -> v + (x - ax)^2) 0.0 xs / genericLength xs where ax = average xs
    absDiff a b = abs (a - b)

newtype Vector a c = Vector [c] deriving (Eq, Ord, Show, Read)
instance (DimensionSpace a, Coordinate c) => Point Vector a c where
    coord :: a -> Vector a c -> c
    coord a (Vector cs) = cs !! fromEnum a

    toList :: Vector a c -> [(a, c)]
    toList (Vector cs) = zip (enumFrom (minBound :: a)) cs

instance (DimensionSpace a) => Metric (Vector a Double) where
    distance = euclideanMetric

type Vector3d = Vector Space3d Double
