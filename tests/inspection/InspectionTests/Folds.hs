{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.Folds where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
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


{- foldl -}
consFoldlText, textFoldl :: (a -> Char -> a) -> a -> Text -> a
consFoldlText = foldl
textFoldl = Data.Text.foldl

consFoldlText2, textFoldl2 :: Int -> Text -> Int
consFoldlText2 = foldl (\a _ -> a + 1)
textFoldl2 = Data.Text.foldl (\a _ -> a + 1)

consFoldlText4, textFoldl4 :: Int
consFoldlText4 = foldl (\a _ -> a + 1) 0 (pack "aaaa")
textFoldl4 = Data.Text.foldl (\a _ -> a + 1) 0 (pack "aaaa")

consFoldlSeq, seqFoldl :: (b -> a -> b) -> b -> Seq a -> b
consFoldlSeq = foldl
seqFoldl = Data.Foldable.foldl


{- foldl' -}
consFoldl'Text, textFoldl' :: (a -> Char -> a) -> a -> Text -> a
consFoldl'Text = foldl'
textFoldl' = Data.Text.foldl'

consFoldl'Text2, textFoldl'2 :: Int -> Text -> Int
consFoldl'Text2 = foldl' (\a _ -> a + 1)
textFoldl'2 = Data.Text.foldl' (\a _ -> a + 1)

consFoldl'Text4, textFoldl'4 :: Int
consFoldl'Text4 = foldl' (\a _ -> a + 1) 0 (pack "aaaa")
textFoldl'4 = Data.Text.foldl' (\a _ -> a + 1) 0 (pack "aaaa")

consFoldl'Seq, seqFoldl' :: (b -> a -> b) -> b -> Seq a -> b
consFoldl'Seq = foldl'
seqFoldl' = Data.Foldable.foldl'


{- foldl1 -}
consFoldl1Text, textFoldl1 :: (Char -> Char -> Char) -> Text -> Char
consFoldl1Text = foldl1
textFoldl1 = withFrozenCallStack Data.Text.foldl1

consFoldl1Text4, textFoldl14 :: Char
consFoldl1Text4 = foldl1 (\a _ -> '*')  (pack "aaaa")
textFoldl14 = withFrozenCallStack Data.Text.foldl1 (\a _ -> '*')  (pack "aaaa")

consFoldl1Seq, seqFoldl1 :: (a -> a -> a) -> Seq a -> a
consFoldl1Seq = foldl1
seqFoldl1 = Data.Foldable.foldl1


{- foldl1' -}
consFoldl1'Text, textFoldl1' :: (Char -> Char -> Char) -> Text -> Char
consFoldl1'Text = foldl1'
textFoldl1' = withFrozenCallStack Data.Text.foldl1'

consFoldl1'Text4, textFoldl1'4 :: Char
consFoldl1'Text4 = foldl1' (\a _ -> '*')  (pack "aaaa")
textFoldl1'4 = withFrozenCallStack Data.Text.foldl1' (\a _ -> '*')  (pack "aaaa")


{- foldr1 -}
consFoldr1Text, textFoldr1 :: (Char -> Char -> Char) -> Text -> Char
consFoldr1Text = foldr1
textFoldr1 = withFrozenCallStack Data.Text.foldr1

consFoldr1Text4, textFoldr14 :: Char
consFoldr1Text4 = foldr1 (\a _ -> '*')  (pack "aaaa")
textFoldr14 = withFrozenCallStack Data.Text.foldr1 (\a _ -> '*')  (pack "aaaa")

consFoldr1Seq, seqFoldr1 :: (a -> a -> a) -> Seq a -> a
consFoldr1Seq = foldr1
seqFoldr1 = Data.Foldable.foldr1

foldsInspectionTests :: TestTree
foldsInspectionTests =
  testGroup "Folds"
    [ $(inspectTest ('consFoldlText === 'textFoldl))
    , $(inspectTest ('consFoldlText2 === 'textFoldl2))
    , $(inspectTest ('consFoldlText4 === 'textFoldl4))
    , $(inspectTest ('consFoldlSeq === 'seqFoldl))
    , $(inspectTest ('consFoldl'Text === 'textFoldl'))
    , $(inspectTest ('consFoldl'Text2 === 'textFoldl'2))
    , $(inspectTest ('consFoldl'Text4 === 'textFoldl'4))
    , $(inspectTest ('consFoldl'Seq === 'seqFoldl'))
    , $(inspectTest ('consFoldl1Text === 'textFoldl1))
    , $(inspectTest ('consFoldl1Text4 === 'textFoldl14))
    , $(inspectTest ('consFoldl1Seq === 'seqFoldl1))
    , $(inspectTest ('consFoldl1'Text === 'textFoldl1'))
    , $(inspectTest ('consFoldl1'Text4 === 'textFoldl1'4))
    , $(inspectTest ('consFoldr1Text === 'textFoldr1))
    , $(inspectTest ('consFoldr1Text4 === 'textFoldr14))
    , $(inspectTest ('consFoldr1Seq === 'seqFoldr1))
    ]
