{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Data.NNS where

import Data.Metric
import Data.Maybe
import Data.Time
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Heap as Q
import qualified Data.MultiSet as MS

class Searcher s where
    type Pt s :: *
    type Val s :: *
   
    -- nearNeighbors ::  (Metric p) => s p v -> Double -> p -> [(p, v)]
    kNearestNeighbors :: (Metric (Pt s)) => Int -> Pt s -> s -> [(Pt s, Val s)]
    
    nearestNeighbor   :: (Metric (Pt s)) => Pt s -> s -> (Pt s, Val s)
    nearestNeighbor = (head .) . kNearestNeighbors 1
    
    testOnDataset :: (Metric (Pt s), Eq (Val s), Ord (Val s)) => [(Pt s, Val s)] -> [(Pt s, Val s)] -> s -> IO Double
    testOnDataset trainSet testSet searcher = do
        let k = round $ sqrt $ genericLength trainSet -- theory value
        acc <- return $ foldl (\acc (p, v) ->
                    let knn = kNearestNeighbors k p searcher
                        counter = MS.fromList $ map snd knn
                        (pred, _) = maximumBy (comparing snd) $ MS.toOccurList counter
                    in if pred == v then acc + 1 else acc
                ) 0 testSet
        return (acc / genericLength testSet)

-- Brute force method with max k heap
newtype BruteForce p v = BruteForce [(p, v)]
instance Searcher (BruteForce p v) where
    type Pt (BruteForce p v) = p
    type Val (BruteForce p v) = v
    kNearestNeighbors :: (Metric p) => Int -> p -> BruteForce p v -> [(p, v)]
    kNearestNeighbors k sample (BruteForce examples) =
        let distExamples = map (\(p, v) -> (distance p sample, (p, v))) examples
        in snd <$> kMinsByHeap k distExamples

kMinsByHeap :: forall prio val. (Ord prio) => Int -> [(prio, val)] -> [(prio, val)]
kMinsByHeap k prioVals = Q.toAscList maxKHeap where
    (initList, restList) = splitAt k prioVals
    initHeap = Q.fromList initList :: Q.MaxPrioHeap prio val
    maxKHeap = foldl (\heap (prio, val) ->
        let (maxPrio, _) = fromJust $ Q.viewHead heap in 
            if prio >= maxPrio then heap
            else Q.insert (prio, val) (Q.drop 1 heap)
        ) initHeap restList

