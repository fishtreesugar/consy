{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.Generic where

import Data.Int (Int)
import GHC.Num (Integer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.List

import Consy


consGenericLength :: [Int] -> Integer
consGenericLength = genericLength

consGenericTake :: Integer -> [Int] -> [Int]
consGenericTake = genericTake

consGenericDrop :: Integer -> [Int] -> [Int]
consGenericDrop = genericDrop

consGenericSplitAt :: Integer -> [Int] -> ([Int], [Int])
consGenericSplitAt = genericSplitAt

consGenericIndex :: [Int] -> Integer -> Int
consGenericIndex = genericIndex

consGenericReplicate :: Integer -> Int -> [Int]
consGenericReplicate = genericReplicate

listGenericLength :: [Int] -> Integer
listGenericLength = Data.List.genericLength

listGenericTake :: Integer -> [Int] -> [Int]
listGenericTake = Data.List.genericTake

listGenericDrop :: Integer -> [Int] -> [Int]
listGenericDrop = Data.List.genericDrop

listGenericSplitAt :: Integer -> [Int] -> ([Int], [Int])
listGenericSplitAt = Data.List.genericSplitAt

listGenericIndex :: [Int] -> Integer -> Int
listGenericIndex = Data.List.genericIndex

listGenericReplicate :: Integer -> Int -> [Int]
listGenericReplicate = Data.List.genericReplicate


genericInspectionTests :: TestTree
genericInspectionTests =
  testGroup "Generic"
    [ $(inspectTest ('consGenericLength === 'listGenericLength))
    , $(inspectTest ('consGenericTake === 'listGenericTake))
    , $(inspectTest ('consGenericDrop === 'listGenericDrop))
    , $(inspectTest ('consGenericSplitAt === 'listGenericSplitAt))
    , $(inspectTest ('consGenericIndex === 'listGenericIndex))
    , $(inspectTest ('consGenericReplicate === 'listGenericReplicate))
    ]
