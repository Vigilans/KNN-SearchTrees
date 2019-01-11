module Data.Metric where

import Data.Point;
import Control.Applicative

type MetricFn p = p -> p -> Double

-- A metric space is a set together with a metric d on the set
-- it must satisfy following conditions:
--   1. d(x, y) >= 0
--   2. d(x, y) == 0 <=> x == y
--   3. d(x, y) == d(y, x)
--   4. d(x, z) <= d(x, y) + d(y, z)
class Metric p where
    distance :: MetricFn p

-- minkowskiMetric :: (Real c, Point p a c) => Double -> p -> p -> Double
-- minkowskiMetric p a b =
--     let diff = uncurry (-) <$> zip (toList a) (toList b)
--         norm = (**p) . abs <$> diff
--     in  (sum norm) ** (1/p)

-- manhattanMetric :: (Point p) => p a v -> p a v -> Double
-- manhattanMetric = minkowskiMetric 1.0

-- euclideanMetric :: (Point p) => p a v -> p a v -> Double
-- euclideanMetric = minkowskiMetric 2.0
