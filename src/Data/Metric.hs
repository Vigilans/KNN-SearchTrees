module Data.Metric where

type MetricFn p = p -> p -> Double

-- A metric space is a set together with a metric d on the set
-- it must satisfy following conditions:
--   1. d(x, y) >= 0
--   2. d(x, y) == 0 <=> x == y
--   3. d(x, y) == d(y, x)
--   4. d(x, z) <= d(x, y) + d(y, z)
class (Eq p, Ord p) => Metric p where
    distance :: MetricFn p
