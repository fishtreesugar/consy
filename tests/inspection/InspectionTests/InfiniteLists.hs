{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.InfiniteLists where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char, isAlpha, isUpper, isLower, toLower, toUpper)
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
import GHC.Base (IO, pure, seq)
import GHC.Enum (succ)
import GHC.List (errorEmptyList, (++))
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


{- iterate -}
consIterate, listIterate :: (a -> a) -> a -> [a]
consIterate = iterate
listIterate f = go
  where
    go a = a : go (f a)

consIterateLazyText, lazyTextIterate :: (Char -> Char) -> Char -> Data.Text.Lazy.Text
consIterateLazyText = iterate
lazyTextIterate = Data.Text.Lazy.iterate

alterLowerUpper :: Char -> Char
alterLowerUpper a =
  case isAlpha a of
    True -> case isUpper a of
              True -> toLower a
              False -> toUpper a
    False -> a

consIterateLazyText1, lazyTextIterate1 :: Char -> Data.Text.Lazy.Text
consIterateLazyText1 = iterate alterLowerUpper
lazyTextIterate1 = Data.Text.Lazy.iterate alterLowerUpper

consIterateLazyText2, lazyTextIterate2 :: Data.Text.Lazy.Text
consIterateLazyText2 = iterate alterLowerUpper 'a'
lazyTextIterate2 = Data.Text.Lazy.iterate alterLowerUpper 'a'

consIterateLBS, lbsIterate :: (Word8 -> Word8) -> Word8 -> Data.ByteString.Lazy.ByteString
consIterateLBS = iterate
lbsIterate = Data.ByteString.Lazy.iterate

consIterateLBS1, lbsIterate1 :: Word8 -> Data.ByteString.Lazy.ByteString
consIterateLBS1 = iterate (\c -> c)
lbsIterate1 = Data.ByteString.Lazy.iterate (\c -> c)


{- iterate' -}
consIterate', listIterate' :: (a -> a) -> a -> [a]
consIterate' = iterate'
listIterate' f = go
  where
    go a =
      let
        a' = f a
      in
        a' `seq` (a : go a')


{- repeat -}
consRepeat, listRepeat :: a -> [a]
consRepeat a = repeat a
listRepeat a = as
  where
    as = a : as

consRepeatLazyText, lazyTextRepeat :: Char -> Data.Text.Lazy.Text
consRepeatLazyText = repeat
lazyTextRepeat = Data.Text.Lazy.repeat

consRepeatLBS, lbsRepeat :: Word8 -> Data.ByteString.Lazy.ByteString
consRepeatLBS = repeat
lbsRepeat = Data.ByteString.Lazy.repeat


{- replicate -}
consReplicate, listReplicate :: Int -> a -> [a]
consReplicate = replicate
listReplicate = Data.List.replicate

consReplicate1, listReplicate1 :: [Char]
consReplicate1 = replicate 100 'a'
listReplicate1 = Data.List.replicate 100 'a'

consReplicateMap, listReplicateMap :: [Int]
consReplicateMap = map (+10) (replicate 100 10 :: [Int])
listReplicateMap = Data.List.map (+10) (Data.List.replicate 100 10)

consReplicateMap', listReplicateMap' :: Int -> [Int]
consReplicateMap' n = map (+10) (replicate n 10 :: [Int])
listReplicateMap' n = Data.List.map (+10) (Data.List.replicate n 10)

consReplicateText, textReplicate :: Int -> Char -> Text
consReplicateText = replicate
textReplicate n = Data.Text.replicate n . Data.Text.singleton


{- cycle -}
consCycle, listCycle :: [a] -> [a]
consCycle = cycle
listCycle = \as ->
  withFrozenCallStack Data.List.cycle as

consCycleLazyText, lazyTextCycle :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consCycleLazyText = cycle
lazyTextCycle = withFrozenCallStack Data.Text.Lazy.cycle

consCycleLBS, lbsCycle :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consCycleLBS = cycle
lbsCycle = withFrozenCallStack Data.ByteString.Lazy.cycle

infiniteListsInspectionTests :: TestTree
infiniteListsInspectionTests =
  testGroup "Infinite Lists"
    [ $(inspectTest ('consIterate === 'listIterate))
    , $(inspectTest ('consIterateLazyText === 'lazyTextIterate))
    , $(inspectTest ('consIterateLazyText1 === 'lazyTextIterate1))
    , $(inspectTest ('consIterateLazyText2 === 'lazyTextIterate2))
    , $(inspectTest ('consIterateLBS === 'lbsIterate))
    , $(inspectTest ('consIterateLBS1 === 'lbsIterate1))
    , $(inspectTest ('consIterate' === 'listIterate'))
    , $(inspectTest ('consRepeat === 'listRepeat))
    , $(inspectTest ('consRepeatLazyText === 'lazyTextRepeat))
    , $(inspectTest ('consRepeatLBS === 'lbsRepeat))
    , $(inspectTest ('consReplicate === 'listReplicate))
    , $(inspectTest ('consReplicate1 === 'listReplicate1))
    , $(inspectTest (coreOf 'consReplicateMap'))
    , $(inspectTest (coreOf 'consReplicateMap'))
    , $(inspectTest ('consReplicateText === 'textReplicate))
    , $(inspectTest (coreOf 'consCycle))
    , $(inspectTest ('consCycleLazyText === 'lazyTextCycle))
    , $(inspectTest ('consCycleLBS === 'lbsCycle))
    ]
