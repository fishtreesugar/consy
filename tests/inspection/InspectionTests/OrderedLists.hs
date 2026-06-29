{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.OrderedLists where

import Data.Function (id)
import Data.Int (Int)
import Data.Ord (Ordering, compare)
import Data.Vector (Vector)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.ByteString
import qualified Data.List
import qualified Data.Vector

import Consy


consSort :: [Int] -> [Int]
consSort = sort

consSortOn :: [Int] -> [Int]
consSortOn = sortOn id

consInsert :: Int -> [Int] -> [Int]
consInsert = insert

consSortBy :: [Int] -> [Int]
consSortBy = sortBy compare

consInsertBy :: Int -> [Int] -> [Int]
consInsertBy = insertBy compare

consMaximumBy :: [Int] -> Int
consMaximumBy = maximumBy compare

consMinimumBy :: [Int] -> Int
consMinimumBy = minimumBy compare

consSortBS, bsSort :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consSortBS = sort
bsSort = Data.ByteString.sort

consMaximumByVector, vectorMaximumBy :: Vector Int -> Int
consMaximumByVector = maximumBy compare
vectorMaximumBy = Data.Vector.maximumBy compare

consMinimumByVector, vectorMinimumBy :: Vector Int -> Int
consMinimumByVector = minimumBy compare
vectorMinimumBy = Data.Vector.minimumBy compare

listSort :: [Int] -> [Int]
listSort = Data.List.sort

listSortOn :: [Int] -> [Int]
listSortOn = Data.List.sortOn id

listInsert :: Int -> [Int] -> [Int]
listInsert = Data.List.insert

listSortBy :: [Int] -> [Int]
listSortBy = Data.List.sortBy compare

listInsertBy :: Int -> [Int] -> [Int]
listInsertBy = Data.List.insertBy compare

listMaximumBy :: [Int] -> Int
listMaximumBy = Data.List.maximumBy compare

listMinimumBy :: [Int] -> Int
listMinimumBy = Data.List.minimumBy compare


orderedListsInspectionTests :: TestTree
orderedListsInspectionTests =
  testGroup "Ordered Lists"
    [ $(inspectTest ('consSort === 'listSort))
    , $(inspectTest ('consSortOn === 'listSortOn))
    , $(inspectTest ('consInsert === 'listInsert))
    , $(inspectTest ('consSortBy === 'listSortBy))
    , $(inspectTest ('consInsertBy === 'listInsertBy))
    , $(inspectTest ('consMaximumBy === 'listMaximumBy))
    , $(inspectTest ('consMinimumBy === 'listMinimumBy))
    , $(inspectTest ('consSortBS === 'bsSort))
    , $(inspectTest ('consMaximumByVector === 'vectorMaximumBy))
    , $(inspectTest ('consMinimumByVector === 'vectorMinimumBy))
    ]
