{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Point where

-- A d-dimensional space with d axes.
class DimensionSpace a where
    -- number of coordinates of a point.
    dimension :: a -> Int
    -- get all available axes.
    axes :: a -> [a]
    -- get next axis in rolling pattern.
    nextAxis :: a -> a
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

-- avg xs = sum xs / genericLength xs
-- var xs = foldl (\v x -> v + (x - ax)^2) 0.0 xs / genericLength xs where ax = avg xs

-- data KdSpace = KdSpace Int Int
-- instance DimensionSpace KdSpace where
--     dimension (KdSpace d _) = d
--     axes (KdSpace d _) = KdSpace d <$> [0..d-1]
--     axes undefined = KdSpace (-1) <$> [0..]


-- data Vector a c = Vector a c
