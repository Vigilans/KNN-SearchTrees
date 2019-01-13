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
