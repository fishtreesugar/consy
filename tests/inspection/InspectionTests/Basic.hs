{-# language BangPatterns #-}
{-# language CPP #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.Basic where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering, compare, (<), (>))
import Data.Sequence (Seq, (><))
import Data.Text (Text, pack)
import Data.Vector (Vector, (++))
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList)
import GHC.Num (Num, Integer, (+), (-))
import GHC.Real (fromIntegral)
import GHC.Stack (withFrozenCallStack)
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


{- append -}
consListAppend, listAppend :: [a] -> [a] -> [a]
consListAppend = append
listAppend = (Data.List.++)

consAppendText, textAppend :: Text -> Text -> Text
consAppendText = append
textAppend = Data.Text.append

consAppendLazyText, lazyTextAppend :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consAppendLazyText = append
lazyTextAppend = Data.Text.Lazy.append

consAppendVector, vectorAppend :: Vector a -> Vector a -> Vector a
consAppendVector = append
vectorAppend = (Data.Vector.++)

consAppendBS, bsAppend :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consAppendBS = append
bsAppend = Data.ByteString.append

consAppendLBS, lbsAppend :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consAppendLBS = append
lbsAppend = Data.ByteString.Lazy.append

consAppendSeq, seqAppend :: Data.Sequence.Seq a -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consAppendSeq = append
seqAppend = (><)


{- singleton -}
consSingletonList, listSingleton :: a -> [a]
consSingletonList = singleton
listSingleton = Data.List.singleton

consSingletonText, textSingleton :: Char -> Text
consSingletonText = singleton
textSingleton = Data.Text.singleton

consSingletonLazyText, lazyTextSingleton :: Char -> Data.Text.Lazy.Text
consSingletonLazyText = singleton
lazyTextSingleton = Data.Text.Lazy.singleton

consSingletonVector, vectorSingleton :: a -> Vector a
consSingletonVector = singleton
vectorSingleton = Data.Vector.singleton

consSingletonBS, bsSingleton :: Word8 -> Data.ByteString.ByteString
consSingletonBS = singleton
bsSingleton = Data.ByteString.singleton

consSingletonLBS, lbsSingleton :: Word8 -> Data.ByteString.Lazy.ByteString
consSingletonLBS = singleton
lbsSingleton = Data.ByteString.Lazy.singleton

consSingletonSeq, seqSingleton :: a -> Seq a
consSingletonSeq = singleton
seqSingleton = Data.Sequence.singleton


{- head -}
consListHead, listHead :: [a] -> a
consListHead = head
listHead [] = errorEmptyList "head"
listHead (x:_) = x
{-
This test fails for no good reason

inspect ('consListHead === 'listHead)
-}

consHeadText, textHead :: Text -> Char
consHeadText = head
textHead = withFrozenCallStack Data.Text.head

consHeadLazyText, lazyTextHead :: Data.Text.Lazy.Text -> Char
consHeadLazyText = head
lazyTextHead = withFrozenCallStack Data.Text.Lazy.head

consHeadVector, vectorHead :: Vector a -> a
consHeadVector = head
vectorHead = Data.Vector.head

consHeadBS, bsHead :: Data.ByteString.ByteString -> Data.Word.Word8
consHeadBS = head
bsHead = withFrozenCallStack Data.ByteString.head

consHeadLBS, lbsHead :: Data.ByteString.Lazy.ByteString -> Data.Word.Word8
consHeadLBS = head
lbsHead = withFrozenCallStack Data.ByteString.Lazy.head


{- last -}
consLastList, listLast :: [a] -> a
consLastList = last
listLast xs = Data.List.foldl (\_ x -> x) (withFrozenCallStack errorEmptyList "last") xs

consLastText, consFoldrLast, textFoldrLast, textLast :: Text -> Char
consLastText = last
textLast = withFrozenCallStack Data.Text.last
consFoldrLast = foldr (\_ x-> x) (withFrozenCallStack errorEmptyList "tail")
textFoldrLast = Data.Text.foldr (\_ x -> x) (withFrozenCallStack errorEmptyList "tail")

consLastLazyText, lazyTextLast :: Data.Text.Lazy.Text -> Char
consLastLazyText = last
lazyTextLast = withFrozenCallStack Data.Text.Lazy.last

consLastVector, vectorLast :: Vector a -> a
consLastVector = last
vectorLast = Data.Vector.last

consLastBS, bsLast :: Data.ByteString.ByteString -> Data.Word.Word8
consLastBS = last
bsLast = withFrozenCallStack Data.ByteString.last

consLastLBS, lbsLast :: Data.ByteString.Lazy.ByteString -> Data.Word.Word8
consLastLBS = last
lbsLast = withFrozenCallStack Data.ByteString.Lazy.last


{- tail -}
consTailList, listTail :: [a] -> [a]
consTailList = tail
listTail (_:xs) = xs
listTail [] = withFrozenCallStack errorEmptyList "tail"

consTailText, textTail :: Text -> Text
consTailText = tail
textTail = withFrozenCallStack Data.Text.tail

consTailLazyText, lazyTextTail :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTailLazyText = tail
lazyTextTail = withFrozenCallStack Data.Text.Lazy.tail

consTailVector, vectorTail :: Vector a -> Vector a
consTailVector = tail
vectorTail = Data.Vector.tail

consTailBS, bsTail :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consTailBS = tail
bsTail = withFrozenCallStack Data.ByteString.tail

consTailLBS, lbsTail :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTailLBS = tail
lbsTail = withFrozenCallStack Data.ByteString.Lazy.tail


{- init -}
consListInit, listInit :: [a] -> [a]
consListInit = init
listInit = withFrozenCallStack Data.List.init

consInitText, textInit :: Text -> Text
consInitText = init
textInit = withFrozenCallStack Data.Text.init

consInitLazyText, lazyTextInit :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consInitLazyText = init
lazyTextInit = withFrozenCallStack Data.Text.Lazy.init

consInitVector, vectorInit :: Vector a -> Vector a
consInitVector = init
vectorInit = Data.Vector.init

consInitBS, bsInit :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consInitBS = init
bsInit = withFrozenCallStack Data.ByteString.init

consInitLBS, lbsInit :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consInitLBS = init
lbsInit = withFrozenCallStack Data.ByteString.Lazy.init


{- null -}
consListNull, listNull :: [a] -> Bool
consListNull = null
listNull = go
  where
    go s =
      case s of
        [] -> True
        x : _ -> False

consNullText, textNull :: Text -> Bool
consNullText = null
textNull = Data.Text.null

consNullLazyText, lazyTextNull :: Data.Text.Lazy.Text -> Bool
consNullLazyText = null
lazyTextNull = Data.Text.Lazy.null

consNullVector, vectorNull :: Vector a -> Bool
consNullVector = null
vectorNull = Data.Vector.null

consNullBS, bsNull :: Data.ByteString.ByteString -> Bool
consNullBS = null
bsNull = Data.ByteString.null

consNullLBS, lbsNull :: Data.ByteString.Lazy.ByteString -> Bool
consNullLBS = null
lbsNull = Data.ByteString.Lazy.null

consNullSeq, seqNull :: Data.Sequence.Seq a -> Bool
consNullSeq = null
seqNull = Data.Sequence.null


{- length -}
consListLength, listLength :: [a] -> Int
consListLength = length
listLength = go 0
  where
    go !n s =
      case s of
        [] -> n
        _ : xs -> go (n + 1) xs

consLength, consFoldrLength, textFoldrLength, textLength :: Text -> Int
consLength = length
textLength = Data.Text.length
consFoldrLength = foldr (\_ -> (+1)) 0
textFoldrLength = Data.Text.foldr (\_ -> (+1)) 0

consFoldl'Length, textFoldl'Length :: Text -> Int
consFoldl'Length = foldl' (\b _ -> b + 1) 0
textFoldl'Length = Data.Text.foldl' (\b _ -> b + 1) 0

consFoldrLengthLText, ltextFoldrLength :: Data.Text.Lazy.Text -> Int
consFoldrLengthLText = foldr (\_ -> (+1)) 0
ltextFoldrLength = Data.Text.Lazy.foldr (\_ -> (+1)) 0

consFoldrLengthBS, bsFoldrLength :: Data.ByteString.ByteString -> Int
consFoldrLengthBS = foldr (\_ -> (+1)) 0
bsFoldrLength = Data.ByteString.foldr (\_ -> (+1)) 0

consFoldrLengthLBS, lbsFoldrLength :: Data.ByteString.Lazy.ByteString -> Int
consFoldrLengthLBS = foldr (\_ -> (+1)) 0
lbsFoldrLength = Data.ByteString.Lazy.foldr (\_ -> (+1)) 0

consFoldrLengthSeq, seqFoldrLength :: Data.Sequence.Seq a -> Int
consFoldrLengthSeq = foldr (\_ -> (+1)) 0
seqFoldrLength = Data.Foldable.foldr (\_ -> (+1)) 0

consLengthSeq, seqLength :: Data.Sequence.Seq a -> Int
consLengthSeq = length
seqLength = Data.Sequence.length


{- compareLength -}
#if MIN_VERSION_base(4,21,0)
consCompareLengthList, listCompareLength :: [a] -> Int -> Ordering
consCompareLengthList = compareLength
listCompareLength = Data.List.compareLength

#endif
consCompareLengthText, textCompareLength :: Text -> Int -> Ordering
consCompareLengthText = compareLength
textCompareLength = Data.Text.compareLength

consCompareLengthLazyText, lazyTextCompareLength :: Data.Text.Lazy.Text -> Int -> Ordering
consCompareLengthLazyText = compareLength
lazyTextCompareLength xs n = Data.Text.Lazy.compareLength xs (fromIntegral n)

consCompareLengthVector, vectorCompareLength :: Vector a -> Int -> Ordering
consCompareLengthVector = compareLength
vectorCompareLength xs n = compare (Data.Vector.length xs) n

consCompareLengthBS, bsCompareLength :: Data.ByteString.ByteString -> Int -> Ordering
consCompareLengthBS = compareLength
bsCompareLength xs n = compare (Data.ByteString.length xs) n

consCompareLengthLBS, lbsCompareLength :: Data.ByteString.Lazy.ByteString -> Int -> Ordering
consCompareLengthLBS = compareLength
lbsCompareLength xs n = compare (Data.ByteString.Lazy.length xs) (fromIntegral n)

consCompareLengthSeq, seqCompareLength :: Seq a -> Int -> Ordering
consCompareLengthSeq = compareLength
seqCompareLength xs n = compare (Data.Sequence.length xs) n

basicInspectionTests :: TestTree
basicInspectionTests =
  testGroup "Basic"
    [ $(inspectTest ('consListAppend === 'listAppend))
    , $(inspectTest ('consAppendText === 'textAppend))
    , $(inspectTest ('consAppendLazyText === 'lazyTextAppend))
    , $(inspectTest ('consAppendVector === 'vectorAppend))
    , $(inspectTest ('consAppendBS === 'bsAppend))
    , $(inspectTest ('consAppendLBS === 'lbsAppend))
    , $(inspectTest ('consAppendSeq === 'seqAppend))
    , $(inspectTest ('consSingletonList === 'listSingleton))
    , $(inspectTest ('consSingletonText === 'textSingleton))
    , $(inspectTest ('consSingletonLazyText === 'lazyTextSingleton))
    , $(inspectTest ('consSingletonVector === 'vectorSingleton))
    , $(inspectTest ('consSingletonBS === 'bsSingleton))
    , $(inspectTest ('consSingletonLBS === 'lbsSingleton))
    , $(inspectTest ('consSingletonSeq === 'seqSingleton))
    , $(inspectTest ('consHeadText === 'textHead))
    , $(inspectTest ('consHeadLazyText === 'lazyTextHead))
    , $(inspectTest ('consHeadVector === 'vectorHead))
    , $(inspectTest ('consHeadBS === 'bsHead))
    , $(inspectTest ('consHeadLBS === 'lbsHead))
    , $(inspectTest ('consLastList === 'listLast))
    , $(inspectTest ('consLastText === 'textLast))
    , $(inspectTest ('consFoldrLast === 'textFoldrLast))
    , $(inspectTest ('consLastLazyText === 'lazyTextLast))
    , $(inspectTest ('consLastVector === 'vectorLast))
    , $(inspectTest ('consLastBS === 'bsLast))
    , $(inspectTest ('consLastLBS === 'lbsLast))
    , $(inspectTest ('consTailList === 'listTail))
    , $(inspectTest ('consTailText === 'textTail))
    , $(inspectTest ('consTailLazyText === 'lazyTextTail))
    , $(inspectTest ('consTailVector === 'vectorTail))
    , $(inspectTest ('consTailBS === 'bsTail))
    , $(inspectTest ('consTailLBS === 'lbsTail))
    , $(inspectTest ('consListInit === 'listInit))
    , $(inspectTest ('consInitText === 'textInit))
    , $(inspectTest ('consInitLazyText === 'lazyTextInit))
    , $(inspectTest ('consInitVector === 'vectorInit))
    , $(inspectTest ('consInitBS === 'bsInit))
    , $(inspectTest ('consInitLBS === 'lbsInit))
    , $(inspectTest ('consListNull === 'listNull))
    , $(inspectTest ('consNullText === 'textNull))
    , $(inspectTest ('consNullLazyText === 'lazyTextNull))
    , $(inspectTest ('consNullVector === 'vectorNull))
    , $(inspectTest ('consNullBS === 'bsNull))
    , $(inspectTest ('consNullLBS === 'lbsNull))
    , $(inspectTest ('consNullSeq === 'seqNull))
    , $(inspectTest ('consListLength === 'listLength))
    , $(inspectTest ('consLength === 'textLength))
    , $(inspectTest ('consFoldl'Length === 'textFoldl'Length))
    , $(inspectTest ('consFoldrLengthLText === 'ltextFoldrLength))
    , $(inspectTest ('consFoldrLengthBS === 'bsFoldrLength))
    , $(inspectTest ('consFoldrLengthLBS === 'lbsFoldrLength))
    , $(inspectTest ('consFoldrLengthSeq === 'seqFoldrLength))
    , $(inspectTest ('consLengthSeq === 'seqLength))
#if MIN_VERSION_base(4,21,0)
    , $(inspectTest ('consCompareLengthList === 'listCompareLength))
#endif
    , $(inspectTest ('consCompareLengthText === 'textCompareLength))
    , $(inspectTest ('consCompareLengthLazyText === 'lazyTextCompareLength))
    , $(inspectTest ('consCompareLengthVector === 'vectorCompareLength))
    , $(inspectTest ('consCompareLengthBS === 'bsCompareLength))
    , $(inspectTest ('consCompareLengthLBS === 'lbsCompareLength))
    , $(inspectTest ('consCompareLengthSeq === 'seqCompareLength))
    ]
