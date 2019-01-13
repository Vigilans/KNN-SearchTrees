{-# LANGUAGE TypeSynonymInstances #-}
module Data.CoverTree where

import Data.Metric
import Data.NNS
import Data.Maybe
import Data.Either
import Data.Ord
import Data.List hiding (insert)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM

type CoverNode p v = (p, v) 

type CoverSet p v = [CoverNode p v]

type CoverMap p v = M.Map p (CoverSet p v) -- level -> parent -> children nodes

data CoverTree p v = CoverTree {
    levelRange :: (Int, Int),
    coverLevels :: IM.IntMap (CoverMap p v) 
} deriving (Eq, Show, Read)

-- Level ranges
minLevel :: CoverTree p v -> Int
minLevel (CoverTree (l, _) _) = l

maxLevel :: CoverTree p v -> Int
maxLevel (CoverTree (_, r) _) = r

-- Cover set and map acquisition
coverMap :: (Metric p) => Int -> CoverTree p v -> CoverMap p v
coverMap level (CoverTree levelRange levels) = (IM.!) levels $ clamp levelRange level

coverSet :: (Metric p) => Int -> CoverNode p v -> CoverTree p v -> CoverSet p v
coverSet level (parent, _) t = fromMaybe [] $ M.lookup parent (coverMap level t)

coverLevel :: (Metric p) => Int -> CoverTree p v -> CoverSet p v
coverLevel = (M.foldl (++) [] .) . coverMap

-- Children acquisition
children :: (Metric p) => CoverTree p v -> (Int, CoverNode p v) -> CoverSet p v
children = flip $ uncurry (coverSet . pred)

-- The whole dataset lies in the negative infinity level
dataSet :: (Metric p) => CoverTree p v -> CoverSet p v
dataSet = coverLevel (minBound :: Int)

-- Root set is the positive infinity level set with only one node
rootSet :: (Metric p) => CoverTree p v -> CoverSet p v
rootSet = coverLevel (maxBound :: Int)

-- Root node is the only node that lies in root set
root :: (Metric p) => CoverTree p v -> CoverNode p v
root = head . rootSet

-- Restrict number to interval [left, right]
clamp :: (Ord a) => (a, a) -> a -> a
clamp (left, right) = max left . min right

-- Cover Tree invariants


-- distance of a node and a point
nodeDist :: (Metric p) => p -> CoverNode p v -> Double
nodeDist p q = distance p (fst q)

-- distance of a node and a set: d(p,Q) = min[q∈Q]{d(p,q)}
setDist :: (Metric p) => p -> CoverSet p v -> Double
setDist p = minimum . map (distance p . fst)

nearestNeighbor :: (Metric p) => CoverTree p v -> p -> (p, v)
nearestNeighbor t p = 
    let qsInf  = rootSet t -- positive infinity
        qsInf' = foldl (\qsI i -> -- negative infinity
                let qs = concat [children t (i, q) | q <- qsI]
                in [q | q <- qs, distance p (fst q) <= setDist p qs + 2^i]
            ) qsInf [maxLevel t..minLevel t] 
    in minimumBy (comparing $ (distance p . fst)) qsInf'

instance Searcher CoverTree where
    kNearestNeighbors t k p = 
        let qsInf  = rootSet t -- positive infinity
            qsInf' = foldl (\qsI i -> -- negative infinity
                    let ds = [(distance p (fst q), ()) | q <- qsI]
                        dK = fst $ last $ kMinsByHeap k ds -- get the kth nearest distance
                        qs = concat [children t (i, q) | q <- qsI]
                    in [q | q <- qs, distance p (fst q) <= dK + 2^i]
                ) qsInf [maxLevel t..minLevel t] 
        in map snd $ kMinsByHeap k $ [(distance p (fst q), q) | q <- qsInf']
