{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.SearchingWithPredicate where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise, not)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList)
import GHC.Num (Num, Integer, (+), (-))
import GHC.Real (fromIntegral)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Internal.Fusion
import qualified Data.Vector
import qualified Data.Word

import Consy


{- find -}
consFind, listFind :: (a -> Bool) -> [a] -> Maybe a
consFind = find
listFind p = go
  where
    go [] = Nothing
    go (x : xs)
      | p x = Just x
      | otherwise = go xs

consFindText, textFind :: (Char -> Bool) -> Text -> Maybe Char
consFindText = find
textFind = Data.Text.find

consFindText', textFind' :: Text -> Maybe Char
consFindText' = find (== 'a')
textFind' = Data.Text.find (== 'a')

consFindText'', textFind'' :: Maybe Char
consFindText'' = find (== 'a') (pack "bbbb")
textFind'' = Data.Text.find (== 'a') (pack "bbbb")

consFindLazyText, lazyTextFind :: (Char -> Bool) -> Data.Text.Lazy.Text -> Maybe Char
consFindLazyText = find
lazyTextFind = Data.Text.Lazy.find

consFindVector, vectorFind :: (a -> Bool) -> Vector a -> Maybe a
consFindVector = find
vectorFind = Data.Vector.find

consFindBS, bsFind :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Maybe Word8
consFindBS = find
bsFind = Data.ByteString.find

consFindLBS, lbsFind :: (Word8 -> Bool)-> Data.ByteString.Lazy.ByteString -> Maybe Word8
consFindLBS = find
lbsFind = Data.ByteString.Lazy.find


{- filter -}
consFilter, listFilter :: (a -> Bool) -> [a] -> [a]
consFilter = filter
listFilter = Data.List.filter

consFilterText, textFilter :: (Char -> Bool) -> Text -> Text
consFilterText = filter
textFilter = Data.Text.filter

consFilterText', textFilter' :: Text -> Text
consFilterText' = filter (== 'a')
textFilter' = Data.Text.filter (== 'a')

consFilterText'', textFilter'' :: Text
consFilterText'' = filter (== 'a') (pack "bbbb")
textFilter'' = Data.Text.filter (== 'a') (pack "bbbb")

consFilterMapText, textFilterMap :: Text -> Text
consFilterMapText = map succ . filter (== 'a')
textFilterMap = Data.Text.map succ . Data.Text.filter (== 'a')

consMapFilter, listMapFilter :: Num a => (a -> Bool) -> [a] -> [a]
consMapFilter p = filter p . map (+1)
listMapFilter p = Data.List.filter p . Data.List.map (+1)

consFilterSeq, seqFilter :: (a -> Bool) -> Seq a -> Seq a
consFilterSeq = filter
seqFilter = Data.Sequence.filter


{- partititon -}
consPartitionList, listPartition :: (a -> Bool) -> [a] -> ([a], [a])
consPartitionList = partition
listPartition p = Data.List.foldr (select p) ([],[])
  where
    select p x ~(ts,fs)
      | p x       = (x : ts,fs)
      | otherwise = (ts, x : fs)
{-
The core for these two functions differ slightly, because the List-based version seems to
unpack the tuples, but the Cons-based version doesn't. This doesn't impact performance.

inspect ('consPartitionList === 'listPartition)
-}

consPartitionText, textPartition :: (Char -> Bool) -> Text -> (Text, Text)
consPartitionText = partition
textPartition = Data.Text.partition

consPartitionLazyText, lazyTextPartition :: (Char -> Bool) -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consPartitionLazyText = partition
lazyTextPartition = Data.Text.Lazy.partition

consPartitionVector, vectorPartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
consPartitionVector = partition
vectorPartition = Data.Vector.partition

consPartitionBS, bsPartition :: (Word8 -> Bool) -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consPartitionBS = partition
bsPartition = Data.ByteString.partition

consPartitionLBS, lbsPartition :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consPartitionLBS = partition
lbsPartition = Data.ByteString.Lazy.partition

consPartitionSeq, seqPartition :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
consPartitionSeq = partition
seqPartition = Data.Sequence.partition

searchingWithPredicateInspectionTests :: TestTree
searchingWithPredicateInspectionTests =
  testGroup "Searching With Predicate"
    [ $(inspectTest ('consFind === 'listFind))
    , $(inspectTest ('consFindText === 'textFind))
    , $(inspectTest ('consFindText' === 'textFind'))
    , $(inspectTest ('consFindText'' === 'textFind''))
    , $(inspectTest ('consFindLazyText === 'lazyTextFind))
    , $(inspectTest ('consFindVector === 'vectorFind))
    , $(inspectTest ('consFindBS === 'bsFind))
    , $(inspectTest ('consFindLBS === 'lbsFind))
    , $(inspectTest ('consFilter === 'listFilter))
    , $(inspectTest ('consFilterText === 'textFilter))
    , $(inspectTest ('consFilterText' === 'textFilter'))
    , $(inspectTest ('consFilterText'' === 'textFilter''))
    , $(inspectTest ('consFilterMapText === 'textFilterMap))
    , $(inspectTest (coreOf 'consMapFilter))
    , $(inspectTest ('consFilterSeq === 'seqFilter))
    , $(inspectTest ('consPartitionText === 'textPartition))
    , $(inspectTest ('consPartitionLazyText === 'lazyTextPartition))
    , $(inspectTest ('consPartitionVector === 'vectorPartition))
    , $(inspectTest ('consPartitionBS === 'bsPartition))
    , $(inspectTest ('consPartitionLBS === 'lbsPartition))
    , $(inspectTest ('consPartitionSeq === 'seqPartition))
    ]
