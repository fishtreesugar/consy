{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.SearchingByEquality where

import Data.Bool (Bool(..), (&&), (||), otherwise)
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Vector (Vector)
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Vector

import Consy


{- elem -}
consElem, listElem :: Eq a => a -> [a] -> Bool
consElem = elem
listElem x = go x
  where
    go x s =
      case s of
        [] -> False
        (a:as) -> x == a || go x as

consElemVector, vectorElem :: Eq a => a -> Vector a -> Bool
consElemVector = elem
vectorElem = Data.Vector.elem

consElemBS, bsElem :: Word8 -> Data.ByteString.ByteString -> Bool
consElemBS = elem
bsElem = Data.ByteString.elem

consElemLBS, lbsElem :: Word8 -> Data.ByteString.Lazy.ByteString -> Bool
consElemLBS = elem
lbsElem = Data.ByteString.Lazy.elem


{- notElem -}
consNotElem, listNotElem :: Eq a => a -> [a] -> Bool
consNotElem = notElem
listNotElem x = go x
  where
    go x s =
      case s of
        [] -> True
        (a:as) -> x /= a && go x as

consNotElemVector, vectorNotElem :: Eq a => a -> Vector a -> Bool
consNotElemVector = notElem
vectorNotElem = Data.Vector.notElem

consNotElemBS, bsNotElem :: Word8 -> Data.ByteString.ByteString -> Bool
consNotElemBS = notElem
bsNotElem = Data.ByteString.notElem

consNotElemLBS, lbsNotElem :: Word8 -> Data.ByteString.Lazy.ByteString -> Bool
consNotElemLBS = notElem
lbsNotElem = Data.ByteString.Lazy.notElem


{- lookup -}
consLookup, listLookup :: (Eq a) => a -> [(a,b)] -> Maybe b
consLookup = lookup
listLookup = go
  where
    go _ [] = Nothing
    go  key ((x,y):xys)
        | x == key  =  Just y
        | otherwise =  go key xys

searchingByEqualityInspectionTests :: TestTree
searchingByEqualityInspectionTests =
  testGroup "Searching By Equality"
    [ $(inspectTest ('consElem === 'listElem))
    , $(inspectTest ('consElemVector === 'vectorElem))
    , $(inspectTest ('consElemBS === 'bsElem))
    , $(inspectTest ('consElemLBS === 'lbsElem))
    , $(inspectTest ('consNotElem === 'listNotElem))
    , $(inspectTest ('consNotElemVector === 'vectorNotElem))
    , $(inspectTest ('consNotElemBS === 'bsNotElem))
    , $(inspectTest ('consNotElemLBS === 'lbsNotElem))
    , $(inspectTest ('consLookup === 'listLookup))
    ]
