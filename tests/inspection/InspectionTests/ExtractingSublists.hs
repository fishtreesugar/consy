{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.ExtractingSublists where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise, not)
import Data.Char (Char, isUpper)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (<=))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList, (++))
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


{- take -}
consTakeList, listTake :: Int -> [a] -> [a]
consTakeList = take
listTake = Data.List.take

consTakeText, textTake :: Int -> Text -> Text
consTakeText = take
textTake = Data.Text.take

consTakeText', textTake' :: Text -> Text
consTakeText' = take 10
textTake' = Data.Text.take 10

consTakeText'', textTake'' :: Text
consTakeText'' = take 10 (pack "bbbb")
textTake'' = Data.Text.take 10 (pack "bbbb")

consTakeLazyText, lazyTextTake :: Int -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTakeLazyText = take
lazyTextTake = \n -> Data.Text.Lazy.take (fromIntegral n)

consTakeVector, vectorTake :: Int -> Vector a -> Vector a
consTakeVector = take
vectorTake = Data.Vector.take

consTakeBS, bsTake :: Int -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consTakeBS = take
bsTake = Data.ByteString.take

consTakeLBS, lbsTake :: Int64 -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTakeLBS = take . fromIntegral
lbsTake = \n -> Data.ByteString.Lazy.take (fromIntegral n)

consTakeSeq, seqTake :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consTakeSeq = take
seqTake = Data.Sequence.take

consMapTakeList, listMapTake :: (a -> a) -> Int -> [a] -> [a]
consMapTakeList f n = map f . take n
listMapTake f n = Data.List.map f . Data.List.take n

consTakeZipList, zipListTake :: Int -> ZipList a -> ZipList a
consTakeZipList n = take n
zipListTake n = ZipList . listTake n . getZipList


{- drop -}
consDropList, listDrop :: Int -> [a] -> [a]
consDropList = drop
listDrop = Data.List.drop

consDropText, textDrop :: Int -> Text -> Text
consDropText = drop
textDrop = Data.Text.drop

consDropText', textDrop' :: Text -> Text
consDropText' = drop 10
textDrop' = Data.Text.drop 10

consDropText'', textDrop'' :: Text
consDropText'' = drop 10 (pack "bbbb")
textDrop'' = Data.Text.drop 10 (pack "bbbb")

consDropLazyText, lazyTextDrop :: Int -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropLazyText = drop
lazyTextDrop = Data.Text.Lazy.drop . fromIntegral

consDropVector, vectorDrop :: Int -> Vector a -> Vector a
consDropVector = drop
vectorDrop = Data.Vector.drop

consDropBS, bsDrop :: Int -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consDropBS = drop
bsDrop = Data.ByteString.drop

consDropLBS, lbsDrop :: Int -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consDropLBS = drop
lbsDrop = \n -> Data.ByteString.Lazy.drop (fromIntegral n)

consDropSeq, seqDrop :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consDropSeq = drop
seqDrop = Data.Sequence.drop


{- splitAt -}
consSplitAtList, listSplitAt :: Int -> [a] -> ([a], [a])
consSplitAtList = splitAt
listSplitAt n ls
  | n <= 0 = ([], ls)
  | otherwise = splitAt' n ls
    where
      splitAt' _ [] = ([], [])
      splitAt' 1 (x:xs) = ([x], xs)
      splitAt' m (x:xs) =
        let
          (xs', xs'') = splitAt' (m - 1) xs
        in
          (x:xs', xs'')
{-
the core from these two functions only differ by a single line- in the Cons-based version
'n' is unpacked and repacked after its comparison, but in the List-based version the 'n'
is preserved using 'case n of wild'. The performance difference is negligable

inspect ('consSplitAtList ==- 'listSplitAt)
-}

consSplitAtText, textSplitAt :: Int -> Text -> (Text, Text)
consSplitAtText = splitAt
textSplitAt = Data.Text.splitAt

consSplitAtText', textSplitAt' :: Text -> (Text, Text)
consSplitAtText' = splitAt 10
textSplitAt' = Data.Text.splitAt 10

consSplitAtText'', textSplitAt'' :: (Text, Text)
consSplitAtText'' = splitAt 10 (pack "bbbb")
textSplitAt'' = Data.Text.splitAt 10 (pack "bbbb")

consSplitAtLazyText, lazyTextSplitAt :: Int64 -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consSplitAtLazyText = splitAt . fromIntegral
lazyTextSplitAt = Data.Text.Lazy.splitAt

consSplitAtVector, vectorSplitAt :: Int -> Vector a -> (Vector a, Vector a)
consSplitAtVector = splitAt
vectorSplitAt = Data.Vector.splitAt

consSplitAtBS, bsSplitAt :: Int -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consSplitAtBS = splitAt
bsSplitAt = Data.ByteString.splitAt

consSplitAtLBS, lbsSplitAt :: Int -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consSplitAtLBS = splitAt
lbsSplitAt = \n -> Data.ByteString.Lazy.splitAt (fromIntegral n)

consSplitAtSeq, seqSplitAt :: Int -> Data.Sequence.Seq a -> (Data.Sequence.Seq a, Data.Sequence.Seq a)
consSplitAtSeq = splitAt
seqSplitAt = Data.Sequence.splitAt


{- takeWhile -}
consTakeWhileList, listTakeWhile :: (a -> Bool) -> [a] -> [a]
consTakeWhileList = takeWhile
listTakeWhile = Data.List.takeWhile

consTakeWhileText, textTakeWhile :: (Char -> Bool) -> Text -> Text
consTakeWhileText = takeWhile
textTakeWhile = Data.Text.takeWhile

consTakeWhileText', textTakeWhile' :: Text -> Text
consTakeWhileText' = takeWhile Data.Char.isUpper
textTakeWhile' = Data.Text.takeWhile Data.Char.isUpper

consTakeWhileLazyText, lazyTextTakeWhile :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTakeWhileLazyText = takeWhile
lazyTextTakeWhile = Data.Text.Lazy.takeWhile

consTakeWhileVector, vectorTakeWhile :: (a -> Bool) -> Vector a -> Vector a
consTakeWhileVector = takeWhile
vectorTakeWhile = Data.Vector.takeWhile

consTakeWhileBS, bsTakeWhile :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consTakeWhileBS = takeWhile
bsTakeWhile = Data.ByteString.takeWhile

consTakeWhileLBS, lbsTakeWhile :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTakeWhileLBS = takeWhile
lbsTakeWhile = Data.ByteString.Lazy.takeWhile


{- dropWhile -}
consDropWhileList, listDropWhile :: (a -> Bool) -> [a] -> [a]
consDropWhileList = dropWhile
listDropWhile p = go p
  where
    go p s =
      case s of
        [] -> []
        (x : xs) -> if p x
                    then go p xs
                    else xs

consDropWhileText, textDropWhile :: (Char -> Bool) -> Text -> Text
consDropWhileText = dropWhile
textDropWhile = Data.Text.dropWhile

consDropWhileText', textDropWhile' :: Text -> Text
consDropWhileText' = dropWhile Data.Char.isUpper
textDropWhile' = Data.Text.dropWhile Data.Char.isUpper

consDropWhileLazyText, lazyTextDropWhile :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropWhileLazyText = dropWhile
lazyTextDropWhile = Data.Text.Lazy.dropWhile

consDropWhileVector, vectorDropWhile :: (a -> Bool) -> Vector a -> Vector a
consDropWhileVector = dropWhile
vectorDropWhile = Data.Vector.dropWhile

consDropWhileBS, bsDropWhile :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consDropWhileBS = dropWhile
bsDropWhile = Data.ByteString.dropWhile

consDropWhileLBS, lbsDropWhile :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consDropWhileLBS = dropWhile
lbsDropWhile = Data.ByteString.Lazy.dropWhile


{- dropWhileEnd -}
consDropWhileEndList, listDropWhileEnd :: (a -> Bool) -> [a] -> [a]
consDropWhileEndList = dropWhileEnd
listDropWhileEnd = Data.List.dropWhileEnd

consDropWhileEndText, textDropWhileEnd :: (Char -> Bool) -> Text -> Text
consDropWhileEndText = dropWhileEnd
textDropWhileEnd = Data.Text.dropWhileEnd

consDropWhileEndText', textDropWhileEnd' :: Text -> Text
consDropWhileEndText' = dropWhileEnd Data.Char.isUpper
textDropWhileEnd' = Data.Text.dropWhileEnd Data.Char.isUpper

consDropWhileEndLazyText, lazyTextDropWhileEnd :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropWhileEndLazyText = dropWhileEnd
lazyTextDropWhileEnd = Data.Text.Lazy.dropWhileEnd


{- span -}
consSpanList, listSpan :: (a -> Bool) -> [a] -> ([a], [a])
consSpanList = span
listSpan = Data.List.span

consSpanText, textSpan :: (Char -> Bool) -> Text -> (Text, Text)
consSpanText = span
textSpan = Data.Text.span

consSpanText', textSpan' :: Text -> (Text, Text)
consSpanText' = span Data.Char.isUpper
textSpan' = Data.Text.span Data.Char.isUpper

consSpanLazyText, lazyTextSpan :: (Char -> Bool) -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consSpanLazyText = span
lazyTextSpan = Data.Text.Lazy.span

consSpanVector, vectorSpan :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
consSpanVector = span
vectorSpan = Data.Vector.span

consSpanBS, bsSpan :: (Word8 -> Bool) -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consSpanBS = span
bsSpan = Data.ByteString.span

consSpanLBS, lbsSpan :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consSpanLBS = span
lbsSpan = Data.ByteString.Lazy.span


{- break -}
consBreakList, listBreak :: (a -> Bool) -> [a] -> ([a], [a])
consBreakList = break
listBreak = Data.List.break

consBreakText, textBreak :: (Char -> Bool) -> Text -> (Text, Text)
consBreakText = break
textBreak = Data.Text.break

consBreakText', textBreak' :: Text -> (Text, Text)
consBreakText' = break Data.Char.isUpper
textBreak' = Data.Text.break Data.Char.isUpper

consBreakLazyText, lazyTextBreak :: (Char -> Bool) -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consBreakLazyText = break
lazyTextBreak = Data.Text.Lazy.break

consBreakVector, vectorBreak :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
consBreakVector = break
vectorBreak = Data.Vector.break

consBreakBS, bsBreak :: (Word8 -> Bool) -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consBreakBS = break
bsBreak = Data.ByteString.break

consBreakLBS, lbsBreak :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consBreakLBS = break
lbsBreak = Data.ByteString.Lazy.break


{- stripPrefix -}
consStripPrefixList, listStripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
consStripPrefixList = stripPrefix
listStripPrefix = Data.List.stripPrefix

consStripPrefixText, textStripPrefix :: Text -> Text -> Maybe Text
consStripPrefixText = stripPrefix
textStripPrefix = Data.Text.stripPrefix

consStripPrefixLazyText, lazyTextStripPrefix :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Maybe Data.Text.Lazy.Text
consStripPrefixLazyText = stripPrefix
lazyTextStripPrefix = Data.Text.Lazy.stripPrefix

{- groupBy -}
consGroupByList, listGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
consGroupByList = groupBy
listGroupBy = Data.List.groupBy

consGroupByText, textGroupBy :: (Char -> Char -> Bool) -> Text -> [Text]
consGroupByText = groupBy
textGroupBy = Data.Text.groupBy

consGroupByText', textGroupBy' :: Text -> [Text]
consGroupByText' = groupBy (==)
textGroupBy' = Data.Text.groupBy (==)

consGroupByLazyText, lazyTextGroupBy :: (Char -> Char -> Bool) -> Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consGroupByLazyText = groupBy
lazyTextGroupBy = Data.Text.Lazy.groupBy

consGroupByBS, bsGroupBy :: (Word8 -> Word8 -> Bool) -> Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consGroupByBS = groupBy
bsGroupBy = Data.ByteString.groupBy

consGroupByLBS, lbsGroupBy :: (Word8 -> Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consGroupByLBS = groupBy
lbsGroupBy = Data.ByteString.Lazy.groupBy

{- group -}
consGroupList, listGroup :: Eq a => [a] -> [[a]]
consGroupList = group
listGroup = Data.List.group

consGroupText, textGroup :: Text -> [Text]
consGroupText = group
textGroup = Data.Text.group

consGroupLazyText, lazyTextGroup :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consGroupLazyText = group
lazyTextGroup = Data.Text.Lazy.group

consGroupBS, bsGroup :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consGroupBS = group
bsGroup = Data.ByteString.group

consGroupLBS, lbsGroup :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consGroupLBS = group
lbsGroup = Data.ByteString.Lazy.group


{- inits -}
consInitsList, listInits :: [a] -> [[a]]
consInitsList = inits
listInits lst = build (initsGo [] lst)
  where
    initsGo hs xs c n =
      hs `c` case xs of
        [] -> n
        (x' : xs') -> initsGo (hs ++ [x']) xs' c n

{-
This test fails, but the code is morally the same and performs similarly

inspect ('consInitsList === 'listInits)
-}

consInitsText, textInits :: Text -> [Text]
consInitsText = inits
textInits = Data.Text.inits

consInitsLazyText, lazyTextInits :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consInitsLazyText = inits
lazyTextInits = Data.Text.Lazy.inits

consInitsBS, bsInits :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consInitsBS = inits
bsInits = Data.ByteString.inits

consInitsLBS, lbsInits :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consInitsLBS = inits
lbsInits = Data.ByteString.Lazy.inits

consInitsSeq, seqInits :: Seq a -> Seq (Seq a)
consInitsSeq = inits
seqInits = Data.Sequence.inits


{- tails -}
consTailsList, listTails :: [a] -> [[a]]
consTailsList = tails
listTails = Data.List.tails

consTailsText, textTails :: Text -> [Text]
consTailsText = tails
textTails = Data.Text.tails

consTailsLazyText, lazyTextTails :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consTailsLazyText = tails
lazyTextTails = Data.Text.Lazy.tails

consTailsBS, bsTails :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consTailsBS = tails
bsTails = Data.ByteString.tails

consTailsLBS, lbsTails :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consTailsLBS = tails
lbsTails = Data.ByteString.Lazy.tails

consTailsSeq, seqTails :: Seq a -> Seq (Seq a)
consTailsSeq = tails
seqTails = Data.Sequence.tails

extractingSublistsInspectionTests :: TestTree
extractingSublistsInspectionTests =
  testGroup "Extracting Sublists"
    [ $(inspectTest ('consTakeList === 'listTake))
    , $(inspectTest ('consTakeText === 'textTake))
    , $(inspectTest ('consTakeText' === 'textTake'))
    , $(inspectTest ('consTakeText'' === 'textTake''))
    , $(inspectTest ('consTakeLazyText === 'lazyTextTake))
    , $(inspectTest ('consTakeVector === 'vectorTake))
    , $(inspectTest ('consTakeBS === 'bsTake))
    , $(inspectTest ('consTakeLBS === 'lbsTake))
    , $(inspectTest ('consTakeSeq === 'seqTake))
    , $(inspectTest (coreOf 'consMapTakeList))
    , $(inspectTest (coreOf 'consTakeZipList))
    , $(inspectTest ('consDropList === 'listDrop))
    , $(inspectTest ('consDropText === 'textDrop))
    , $(inspectTest ('consDropText' === 'textDrop'))
    , $(inspectTest ('consDropText'' === 'textDrop''))
    , $(inspectTest ('consDropLazyText === 'lazyTextDrop))
    , $(inspectTest ('consDropVector === 'vectorDrop))
    , $(inspectTest ('consDropBS === 'bsDrop))
    , $(inspectTest ('consDropLBS === 'lbsDrop))
    , $(inspectTest ('consDropSeq === 'seqDrop))
    , $(inspectTest ('consSplitAtText === 'textSplitAt))
    , $(inspectTest ('consSplitAtText' === 'textSplitAt'))
    , $(inspectTest ('consSplitAtText'' === 'textSplitAt''))
    , $(inspectTest ('consSplitAtLazyText === 'lazyTextSplitAt))
    , $(inspectTest ('consSplitAtVector === 'vectorSplitAt))
    , $(inspectTest ('consSplitAtBS === 'bsSplitAt))
    , $(inspectTest ('consSplitAtLBS === 'lbsSplitAt))
    , $(inspectTest ('consSplitAtSeq === 'seqSplitAt))
    , $(inspectTest ('consTakeWhileList === 'listTakeWhile))
    , $(inspectTest ('consTakeWhileText === 'textTakeWhile))
    , $(inspectTest ('consTakeWhileText' === 'textTakeWhile'))
    , $(inspectTest ('consTakeWhileLazyText === 'lazyTextTakeWhile))
    , $(inspectTest ('consTakeWhileVector === 'vectorTakeWhile))
    , $(inspectTest ('consTakeWhileBS === 'bsTakeWhile))
    , $(inspectTest ('consTakeWhileLBS === 'lbsTakeWhile))
    , $(inspectTest ('consDropWhileList === 'listDropWhile))
    , $(inspectTest ('consDropWhileText === 'textDropWhile))
    , $(inspectTest ('consDropWhileText' === 'textDropWhile'))
    , $(inspectTest ('consDropWhileLazyText === 'lazyTextDropWhile))
    , $(inspectTest ('consDropWhileVector === 'vectorDropWhile))
    , $(inspectTest ('consDropWhileBS === 'bsDropWhile))
    , $(inspectTest ('consDropWhileLBS === 'lbsDropWhile))
    , $(inspectTest ('consDropWhileEndList === 'listDropWhileEnd))
    , $(inspectTest ('consDropWhileEndText === 'textDropWhileEnd))
    , $(inspectTest ('consDropWhileEndText' === 'textDropWhileEnd'))
    , $(inspectTest ('consDropWhileEndLazyText === 'lazyTextDropWhileEnd))
    , $(inspectTest ('consSpanList === 'listSpan))
    , $(inspectTest ('consSpanText === 'textSpan))
    , $(inspectTest ('consSpanText' === 'textSpan'))
    , $(inspectTest ('consSpanLazyText === 'lazyTextSpan))
    , $(inspectTest ('consSpanVector === 'vectorSpan))
    , $(inspectTest ('consSpanBS === 'bsSpan))
    , $(inspectTest ('consSpanLBS === 'lbsSpan))
    , $(inspectTest ('consBreakList === 'listBreak))
    , $(inspectTest ('consBreakText === 'textBreak))
    , $(inspectTest ('consBreakText' === 'textBreak'))
    , $(inspectTest ('consBreakLazyText === 'lazyTextBreak))
    , $(inspectTest ('consBreakVector === 'vectorBreak))
    , $(inspectTest ('consBreakBS === 'bsBreak))
    , $(inspectTest ('consBreakLBS === 'lbsBreak))
    , $(inspectTest ('consStripPrefixList === 'listStripPrefix))
    , $(inspectTest ('consStripPrefixText === 'textStripPrefix))
    , $(inspectTest ('consStripPrefixLazyText === 'lazyTextStripPrefix))
    , $(inspectTest ('consGroupByList === 'listGroupBy))
    , $(inspectTest ('consGroupByText === 'textGroupBy))
    , $(inspectTest ('consGroupByText' === 'textGroupBy'))
    , $(inspectTest ('consGroupByLazyText === 'lazyTextGroupBy))
    , $(inspectTest ('consGroupByBS === 'bsGroupBy))
    , $(inspectTest ('consGroupByLBS === 'lbsGroupBy))
    , $(inspectTest ('consGroupList === 'listGroup))
    , $(inspectTest ('consGroupText === 'textGroup))
    , $(inspectTest ('consGroupLazyText === 'lazyTextGroup))
    , $(inspectTest ('consGroupBS === 'bsGroup))
    , $(inspectTest ('consGroupLBS === 'lbsGroup))
    , $(inspectTest ('consInitsText === 'textInits))
    , $(inspectTest ('consInitsLazyText === 'lazyTextInits))
    , $(inspectTest ('consInitsBS === 'bsInits))
    , $(inspectTest ('consInitsLBS === 'lbsInits))
    , $(inspectTest ('consInitsSeq === 'seqInits))
    , $(inspectTest ('consTailsList === 'listTails))
    , $(inspectTest ('consTailsText === 'textTails))
    , $(inspectTest ('consTailsLazyText === 'lazyTextTails))
    , $(inspectTest ('consTailsBS === 'bsTails))
    , $(inspectTest ('consTailsLBS === 'lbsTails))
    , $(inspectTest ('consTailsSeq === 'seqTails))
    ]
