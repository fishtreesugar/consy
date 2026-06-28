{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.SpecialFolds where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..), (<), (>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList)
import GHC.Num (Num, (+), (-), (*))
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


{- concat -}
consConcat, listConcat :: [[a]] -> [a]
consConcat = concat
listConcat = foldr (append) []

consConcatText, textConcat :: [Text] -> Text
consConcatText = concat
textConcat = Data.Text.concat

consConcatLazyText, lazyTextConcat :: [Data.Text.Lazy.Text] -> Data.Text.Lazy.Text
consConcatLazyText = concat
lazyTextConcat = Data.Text.Lazy.concat

consConcatVector, vectorConcat :: [Vector a] -> Vector a
consConcatVector = concat
vectorConcat = Data.Vector.concat

consConcatBS, bsConcat :: [Data.ByteString.ByteString] -> Data.ByteString.ByteString
consConcatBS = concat
bsConcat = Data.ByteString.concat

consConcatLBS, lbsConcat :: [Data.ByteString.Lazy.ByteString] -> Data.ByteString.Lazy.ByteString
consConcatLBS = concat
lbsConcat = Data.ByteString.Lazy.concat


{- concatMap -}
consConcatMap, listConcatMap :: (a -> [b]) -> [a] -> [b]
consConcatMap = concatMap
listConcatMap f = \as -> foldr ((append) . f) [] as

consConcatMapText, textConcatMap :: (Char -> Text) -> Text -> Text
consConcatMapText = concatMap
textConcatMap = Data.Text.concatMap

consConcatMapLazyText, lazyTextConcatMap :: (Char -> Data.Text.Lazy.Text) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consConcatMapLazyText = concatMap
lazyTextConcatMap = Data.Text.Lazy.concatMap

consConcatMapVector, vectorConcatMap :: (a -> Vector b) -> Vector a -> Vector b
consConcatMapVector = concatMap
vectorConcatMap = Data.Vector.concatMap

consConcatMapBS, bsConcatMap :: (Word8 -> Data.ByteString.ByteString) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consConcatMapBS = concatMap
bsConcatMap = Data.ByteString.concatMap

consConcatMapLBS, lbsConcatMap :: (Word8 -> Data.ByteString.Lazy.ByteString) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consConcatMapLBS = concatMap
lbsConcatMap = Data.ByteString.Lazy.concatMap


{- and -}
consAnd, listAnd :: [Bool] -> Bool
consAnd = and
listAnd = foldr (&&) True

consAndVector, vectorAnd :: Vector Bool -> Bool
consAndVector = and
vectorAnd = Data.Vector.and


{- or -}
consOr, listOr :: [Bool] -> Bool
consOr = or
listOr = foldr (||) False

consOrVector, vectorOr :: Vector Bool -> Bool
consOrVector = or
vectorOr = Data.Vector.or


{- any -}
consAny, listAny :: (a -> Bool) -> [a] -> Bool
consAny = any
listAny p = go
  where
    go s =
      case s of
        [] -> False
        x:xs -> p x || go xs

consAnyText, textAny :: (Char -> Bool) -> Text -> Bool
consAnyText = any
textAny = Data.Text.any

consAnyLazyText, lazyTextAny :: (Char -> Bool) -> Data.Text.Lazy.Text -> Bool
consAnyLazyText = any
lazyTextAny = Data.Text.Lazy.any

consAnyVector, vectorAny :: (a -> Bool) -> Vector a -> Bool
consAnyVector = any
vectorAny = Data.Vector.any

consAnyBS, bsAny :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Bool
consAnyBS = any
bsAny = Data.ByteString.any

consAnyLBS, lbsAny :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Bool
consAnyLBS = any
lbsAny = Data.ByteString.Lazy.any


{- all -}
consAll, listAll :: (a -> Bool) -> [a] -> Bool
consAll = all
listAll p = go
  where
  go s =
    case s of
      [] -> True
      (x:xs) -> p x && go xs

consAllText, textAll :: (Char -> Bool) -> Text -> Bool
consAllText = all
textAll = Data.Text.all

consAllLazyText, lazyTextAll :: (Char -> Bool) -> Data.Text.Lazy.Text -> Bool
consAllLazyText = all
lazyTextAll = Data.Text.Lazy.all

consAllVector, vectorAll :: (a -> Bool) -> Vector a -> Bool
consAllVector = all
vectorAll = Data.Vector.all

consAllBS, bsAll :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Bool
consAllBS = all
bsAll = Data.ByteString.all

consAllLBS, lbsAll :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Bool
consAllLBS = all
lbsAll = Data.ByteString.Lazy.all


{- sum -}
consSum, listSum :: Num a => [a] -> a
consSum = sum
listSum xs = foldl (+) 0 xs

consSumVector, vectorSum :: Num a => Vector a -> a
consSumVector = sum
vectorSum = Data.Vector.sum

consSumSeq, seqSum :: Num a => Data.Sequence.Seq a -> a
consSumSeq = sum
seqSum = Data.Foldable.sum


{- product -}
consProduct, listProduct :: Num a => [a] -> a
consProduct = product
listProduct xs = foldl (*) 1 xs

consProductVector, vectorProduct :: Num a => Vector a -> a
consProductVector = product
vectorProduct = Data.Vector.product

consProductSeq, seqProduct :: Num a => Data.Sequence.Seq a -> a
consProductSeq = product
seqProduct = Data.Foldable.product


{- maximum -}
consMaximum, listMaximum :: (Ord a) => [a] -> a
consMaximum = maximum
listMaximum [] = errorEmptyList "maximum"
listMaximum xs = foldl1 max xs

consMaximumText, textMaximum :: Text -> Char
consMaximumText = maximum
textMaximum = Data.Text.maximum

consMaximumLazyText, lazyTextMaximum :: Data.Text.Lazy.Text -> Char
consMaximumLazyText = maximum
lazyTextMaximum = Data.Text.Lazy.maximum

consMaximumVector, vectorMaximum :: (Ord a) => Vector a -> a
consMaximumVector = maximum
vectorMaximum = Data.Vector.maximum

consMaximumBS, bsMaximum :: Data.ByteString.ByteString -> Word8
consMaximumBS = maximum
bsMaximum = Data.ByteString.maximum

consMaximumLBS, lbsMaximum :: Data.ByteString.Lazy.ByteString -> Word8
consMaximumLBS = maximum
lbsMaximum = Data.ByteString.Lazy.maximum

consMaximumSeq, seqMaximum :: (Ord a) => Data.Sequence.Seq a -> a
consMaximumSeq = maximum
seqMaximum = Data.Foldable.maximum


{- minimum -}
consMinimum, listMinimum :: (Ord a) => [a] -> a
consMinimum = minimum
listMinimum [] = errorEmptyList "minimum"
listMinimum s = foldl1 min s

consMinimumText, textMinimum :: Text -> Char
consMinimumText = minimum
textMinimum = Data.Text.minimum

consMinimumLazyText, lazyTextMinimum :: Data.Text.Lazy.Text -> Char
consMinimumLazyText = minimum
lazyTextMinimum = Data.Text.Lazy.minimum

consMinimumVector, vectorMinimum :: (Ord a) => Vector a -> a
consMinimumVector = minimum
vectorMinimum = Data.Vector.minimum

consMinimumBS, bsMinimum :: Data.ByteString.ByteString -> Word8
consMinimumBS = minimum
bsMinimum = Data.ByteString.minimum

consMinimumLBS, lbsMinimum :: Data.ByteString.Lazy.ByteString -> Word8
consMinimumLBS = minimum
lbsMinimum = Data.ByteString.Lazy.minimum

consMinimumSeq, seqMinimum :: (Ord a) => Data.Sequence.Seq a -> a
consMinimumSeq = minimum
seqMinimum = Data.Foldable.minimum

specialFoldsInspectionTests :: TestTree
specialFoldsInspectionTests =
  testGroup "Special Folds"
    [ $(inspectTest ('consConcat === 'listConcat))
    , $(inspectTest ('consConcatText === 'textConcat))
    , $(inspectTest ('consConcatLazyText === 'lazyTextConcat))
    , $(inspectTest ('consConcatVector === 'vectorConcat))
    , $(inspectTest ('consConcatBS === 'bsConcat))
    , $(inspectTest ('consConcatLBS === 'lbsConcat))
    , $(inspectTest ('consConcatMap === 'listConcatMap))
    , $(inspectTest ('consConcatMapText === 'textConcatMap))
    , $(inspectTest ('consConcatMapLazyText === 'lazyTextConcatMap))
    , $(inspectTest ('consConcatMapVector === 'vectorConcatMap))
    , $(inspectTest ('consConcatMapBS === 'bsConcatMap))
    , $(inspectTest ('consConcatMapLBS === 'lbsConcatMap))
    , $(inspectTest ('consAnd === 'listAnd))
    , $(inspectTest ('consAndVector === 'vectorAnd))
    , $(inspectTest ('consOr === 'listOr))
    , $(inspectTest ('consOrVector === 'vectorOr))
    , $(inspectTest ('consAny ==- 'listAny))
    , $(inspectTest ('consAnyText === 'textAny))
    , $(inspectTest ('consAnyLazyText === 'lazyTextAny))
    , $(inspectTest ('consAnyVector === 'vectorAny))
    , $(inspectTest ('consAnyBS === 'bsAny))
    , $(inspectTest ('consAnyLBS === 'lbsAny))
    , $(inspectTest ('consAll === 'listAll))
    , $(inspectTest ('consAllText === 'textAll))
    , $(inspectTest ('consAllLazyText === 'lazyTextAll))
    , $(inspectTest ('consAllVector === 'vectorAll))
    , $(inspectTest ('consAllBS === 'bsAll))
    , $(inspectTest ('consAllLBS === 'lbsAll))
    , $(inspectTest ('consSum === 'listSum))
    , $(inspectTest ('consSumVector === 'vectorSum))
    , $(inspectTest ('consSumSeq === 'seqSum))
    , $(inspectTest ('consProduct === 'listProduct))
    , $(inspectTest ('consProductVector === 'vectorProduct))
    , $(inspectTest ('consProductSeq === 'seqProduct))
    , $(inspectTest ('consMaximum === 'listMaximum))
    , $(inspectTest ('consMaximumText === 'textMaximum))
    , $(inspectTest ('consMaximumLazyText === 'lazyTextMaximum))
    , $(inspectTest ('consMaximumVector === 'vectorMaximum))
    , $(inspectTest ('consMaximumBS === 'bsMaximum))
    , $(inspectTest ('consMaximumLBS === 'lbsMaximum))
    , $(inspectTest ('consMaximumSeq === 'seqMaximum))
    , $(inspectTest ('consMinimum === 'listMinimum))
    , $(inspectTest ('consMinimumText === 'textMinimum))
    , $(inspectTest ('consMinimumLazyText === 'lazyTextMinimum))
    , $(inspectTest ('consMinimumVector === 'vectorMinimum))
    , $(inspectTest ('consMinimumBS === 'bsMinimum))
    , $(inspectTest ('consMinimumLBS === 'lbsMinimum))
    , $(inspectTest ('consMinimumSeq === 'seqMinimum))
    ]
