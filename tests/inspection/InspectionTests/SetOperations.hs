{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.SetOperations where

import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Int (Int)
import Data.Ord (Ordering(..), compare)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.List

import Consy


sameByCompare :: Int -> Int -> Bool
sameByCompare x y = compare x y == EQ


consNub :: [Int] -> [Int]
consNub = nub

consNubOrd :: [Int] -> [Int]
consNubOrd = nubOrd

consDelete :: Int -> [Int] -> [Int]
consDelete = delete

consDifference :: [Int] -> [Int] -> [Int]
consDifference = (\\)

consUnion :: [Int] -> [Int] -> [Int]
consUnion = union

consIntersect :: [Int] -> [Int] -> [Int]
consIntersect = intersect

consNubBy :: [Int] -> [Int]
consNubBy = nubBy sameByCompare

consNubOrdBy :: [Int] -> [Int]
consNubOrdBy = nubOrdBy compare

consDeleteBy :: Int -> [Int] -> [Int]
consDeleteBy = deleteBy sameByCompare

consDeleteFirstsBy :: [Int] -> [Int] -> [Int]
consDeleteFirstsBy = deleteFirstsBy sameByCompare

consUnionBy :: [Int] -> [Int] -> [Int]
consUnionBy = unionBy sameByCompare

consIntersectBy :: [Int] -> [Int] -> [Int]
consIntersectBy = intersectBy sameByCompare

listNub :: [Int] -> [Int]
listNub = Data.List.nub

listNubOrd :: [Int] -> [Int]
listNubOrd = Data.List.nub

listDelete :: Int -> [Int] -> [Int]
listDelete = Data.List.delete

listDifference :: [Int] -> [Int] -> [Int]
listDifference = (Data.List.\\)

listUnion :: [Int] -> [Int] -> [Int]
listUnion = Data.List.union

listIntersect :: [Int] -> [Int] -> [Int]
listIntersect = Data.List.intersect

listNubBy :: [Int] -> [Int]
listNubBy = Data.List.nubBy sameByCompare

listNubOrdBy :: [Int] -> [Int]
listNubOrdBy = Data.List.nubBy sameByCompare

listDeleteBy :: Int -> [Int] -> [Int]
listDeleteBy = Data.List.deleteBy sameByCompare

listDeleteFirstsBy :: [Int] -> [Int] -> [Int]
listDeleteFirstsBy = Data.List.deleteFirstsBy sameByCompare

listUnionBy :: [Int] -> [Int] -> [Int]
listUnionBy = Data.List.unionBy sameByCompare

listIntersectBy :: [Int] -> [Int] -> [Int]
listIntersectBy = Data.List.intersectBy sameByCompare


setOperationsInspectionTests :: TestTree
setOperationsInspectionTests =
  testGroup "Set Operations"
    [ $(inspectTest ('consNub === 'listNub))
    , $(inspectTest ('consNubOrd === 'listNubOrd))
    , $(inspectTest ('consDelete === 'listDelete))
    , $(inspectTest ('consDifference === 'listDifference))
    , $(inspectTest ('consUnion === 'listUnion))
    , $(inspectTest ('consIntersect === 'listIntersect))
    , $(inspectTest ('consNubBy === 'listNubBy))
    , $(inspectTest ('consNubOrdBy === 'listNubOrdBy))
    , $(inspectTest ('consDeleteBy === 'listDeleteBy))
    , $(inspectTest ('consDeleteFirstsBy === 'listDeleteFirstsBy))
    , $(inspectTest ('consUnionBy === 'listUnionBy))
    , $(inspectTest ('consIntersectBy === 'listIntersectBy))
    ]
