{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import Debug.Trace

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

-- distance of a node and a point
nodeDist :: (Metric p) => p -> CoverNode p v -> Double
nodeDist p q = distance p (fst q)

-- distance of a node and a set: d(p,Q) = min[q∈Q]{d(p,q)}
setDist :: (Metric p) => p -> CoverSet p v -> Double
setDist p = minimum . map (distance p . fst)

-- Cover Tree invariants

-- Create a cover tree with a single value
singleton :: (Metric p) => Int -> (p, v) -> CoverTree p v
singleton maxLev (p, v) = uncoalesce (maxLev - 1) (p, v) CoverTree {
    levelRange  = (maxLev, maxLev),
    coverLevels = IM.singleton maxLev $ M.singleton p [(p, v)]
}

-- Extend the node of its self-child to i-th level
uncoalesce :: (Metric p) => Int -> CoverNode p v -> CoverTree p v -> CoverTree p v
uncoalesce i (p, v) (CoverTree (minLev, maxLev) levels) = CoverTree {
    levelRange  = (min minLev i, maxLev),
    coverLevels = 
        let foldrWhile cl j
                | j >= maxLev = cl
                | IM.notMember j cl = foldrWhile (IM.insert j (M.singleton p [(p, v)]) cl) (j + 1)
                | M.notMember  p cm = foldrWhile (IM.insert j (M.insert p [(p, v)] cm) cl) (j + 1)
                | otherwise = cl
                where cm = (IM.!) cl j
        in foldrWhile levels i
}

-- Insert a child to level-i node, possibly updating minLevel
-- It ensures that coverLevels ranging from (minLev, maxLev) exists.
insertChild :: (Metric p) => CoverTree p v -> (Int, CoverNode p v) -> CoverNode p v -> CoverTree p v
insertChild t (i, (p, v)) child = 
    let (CoverTree levelRange levels) = uncoalesce (i - 1) (p, v) t 
    in CoverTree {
        levelRange  = levelRange,
        coverLevels = IM.update (Just . M.update (\cs -> Just $ child:cs) p) (i - 1) levels
    }

traceT str stmt = trace (str ++ show stmt) stmt

-- Insert a node into the cover tree in recursive pattern
insert :: (Metric p, Eq v) => CoverTree p v -> (p, v) -> CoverTree p v
insert t (p, v) = snd $ insert' p (rootSet t) (maxLevel t)
    where insert' p qsI i
            | setDist p qs > 2^i = (False, t)
            | not parentFound && setDist p qsI <= 2^i = 
                let q = head [q | q <- qsI, nodeDist p q <= 2^i] 
                in (True, insertChild t (i, q) (p, v))
            | otherwise = (parentFound, newTree)
            where qs = concat [children t (i, q) | q <- qsI]
                  qsI' = [q | q <- qs, nodeDist p q <= 2^i]
                  (parentFound, newTree) = insert' p qsI' (i - 1)

fromList :: (Metric p, Eq v, Show p, Show v) => Int -> [(p, v)] -> CoverTree p v
fromList maxLev pvs = foldl insert (singleton maxLev $ head pvs) $ tail pvs 

-- -- Batch insert construction
-- fromList :: (Metric p) => [(p, v)] -> CoverTree p v
-- fromList pvs = 
--     let batchConstruct p (near, far) i
--             | S.null near = (p, S.empty)
--             | otherwise = 

instance Searcher (CoverTree p v) where
    type Pt (CoverTree p v) = p
    type Val (CoverTree p v) = v
    
    nearestNeighbor :: (Metric p) => p -> CoverTree p v -> (p, v)
    nearestNeighbor p t = 
        let qsInf  = rootSet t -- positive infinity
            qsInf' = foldl (\qsI i -> -- negative infinity
                    let qs = concat [children t (i, q) | q <- qsI]
                    in [q | q <- qs, distance p (fst q) <= setDist p qs + 2^i]
                ) qsInf [maxLevel t,maxLevel t - 1..minLevel t] 
        in minimumBy (comparing $ (distance p . fst)) qsInf'

    kNearestNeighbors :: (Metric p) => Int -> p -> CoverTree p v -> [(p, v)]
    kNearestNeighbors k p t = 
        let qsInf  = rootSet t -- positive infinity
            qsInf' = foldl (\qsI i -> -- negative infinity
                    let ds = [(distance p (fst q), ()) | q <- qsI]
                        dK = fst $ last $ kMinsByHeap k ds -- get the kth nearest distance
                        qs = concat [children t (i, q) | q <- qsI]
                    in [q | q <- qs, distance p (fst q) <= dK + 2^i]
                ) qsInf [maxLevel t,maxLevel t - 1..minLevel t] 
        in map snd $ kMinsByHeap k $ [(distance p (fst q), q) | q <- qsInf']
