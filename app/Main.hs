{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.NNS
import Data.Metric
import Data.Point
import Data.Time
import Data.List
import Data.List.Split
import qualified Data.KdTree as KdTree
import qualified Data.CoverTree as CoverTree
import System.Environment

-- points = map Vector [[1, 2, 3], [2, 0, -1], [-3, 5, 0]] :: [Vector3d]
-- labels = [0, 1, 0]
-- entries = zip points labels
-- query = Vector [1, 0, 1] :: Vector3d

testBruteForce :: IO Double
testBruteForce = do 
    content <- readFile $ "test/test_set.txt"
    let rawData = reverse . splitOn "\t" <$> lines content 
    let dataset = map (\(tag:point) -> (Vector $ map read point :: Vector3d, read tag :: Int)) rawData
    let (testSet, trainSet) = splitAt (length dataset `div` 3) dataset

    let bruteForce = BruteForce trainSet
    testOnDataset trainSet testSet bruteForce

testKdTree :: IO Double
testKdTree = do 
    content <- readFile $ "test/test_set.txt"
    let rawData = reverse . splitOn "\t" <$> lines content 
    let dataset = map (\(tag:point) -> (Vector $ map read point :: Vector3d, read tag :: Int)) rawData
    let (testSet, trainSet) = splitAt (length dataset `div` 3) dataset
    
    let kdTree = KdTree.fromList trainSet
    testOnDataset trainSet testSet kdTree

testCoverTree :: IO Double
testCoverTree = do 
    content <- readFile $ "test/test_set.txt"
    let rawData = reverse . splitOn "\t" <$> lines content 
    let dataset = map (\(tag:point) -> (Vector $ map read point :: Vector3d, read tag :: Int)) rawData
    let (testSet, trainSet) = splitAt (length dataset `div` 3) dataset

    let coverTree = CoverTree.fromList 3 trainSet
    print $ "isTreeValid: " ++ (show $ CoverTree.checkInvariant coverTree)
    testOnDataset trainSet testSet coverTree

main :: IO ()
main = return ()

    


    
    
