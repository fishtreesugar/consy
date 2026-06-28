{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.Scans where

import Control.Applicative (ZipList(..))
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy


{- scanl -}
consScanlSeq, seqScanl :: (b -> a -> b) -> b -> Data.Sequence.Seq a -> Data.Sequence.Seq b
consScanlSeq = scanl
seqScanl = Data.Sequence.scanl

consScanlList, listScanl :: (b -> a -> b) -> b -> [a] -> [b]
consScanlList = scanl
listScanl = scanlGo
  where
    scanlGo f q ls =
      q : (case ls of
             []   -> []
             x:xs -> scanlGo f (f q x) xs)
{-
The list-based passes some arguments in an unpacked tuple, but the cons-based version
uses lists and consing, but this doesn't impact performance

inspect ('consScanlList === 'listScanl)
-}

consScanlText, textScanl
  :: (Char -> Char -> Char) -> Char -> Data.Text.Text -> Data.Text.Text
consScanlText = scanl
textScanl = Data.Text.scanl

consScanlTextLazy, textLazyScanl
  :: (Char -> Char -> Char) -> Char -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consScanlTextLazy = scanl
textLazyScanl = Data.Text.Lazy.scanl

consScanlByteString, byteStringScanl
  :: (Word8 -> Word8 -> Word8) -> Word8 -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consScanlByteString = scanl
byteStringScanl = Data.ByteString.scanl

consScanlByteStringLazy, byteStringLazyScanl
  :: (Word8 -> Word8 -> Word8) -> Word8 -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consScanlByteStringLazy = scanl
byteStringLazyScanl = Data.ByteString.Lazy.scanl

consScanlVectorLazy, vectorLazyScanl
  :: (b -> a -> b) -> b -> Data.Vector.Vector a -> Data.Vector.Vector b
consScanlVectorLazy = scanl
vectorLazyScanl = Data.Vector.scanl


{- scanl' -}
consScanl'List, listScanl' :: (b -> a -> b) -> b -> [a] -> [b]
consScanl'List = scanl'
listScanl' = scanl'Go
  where
    scanl'Go f !q ls =
      q : (case ls of
             []   -> []
             x:xs -> scanl'Go f (f q x) xs)
{-
Same as above

inspect ('consScanl'List === 'listScanl')
-}

consScanl'VectorLazy, vectorLazyScanl'
  :: (a -> a -> a) -> a -> Data.Vector.Vector a -> Data.Vector.Vector a
consScanl'VectorLazy = scanl'
vectorLazyScanl' = Data.Vector.scanl'


{- scanl1 -}
consScanl1List, listScanl1 :: (a -> a -> a) -> [a] -> [a]
consScanl1List = scanl1
listScanl1 = Data.List.scanl1

consScanl1Text, textScanl1
  :: (Char -> Char -> Char) -> Data.Text.Text -> Data.Text.Text
consScanl1Text = scanl1
textScanl1 = Data.Text.scanl1

consScanl1TextLazy, textLazyScanl1
  :: (Char -> Char -> Char) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consScanl1TextLazy = scanl1
textLazyScanl1 = Data.Text.Lazy.scanl1

consScanl1ByteString, byteStringScanl1
  :: (Word8 -> Word8 -> Word8) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consScanl1ByteString = scanl1
byteStringScanl1 = Data.ByteString.scanl1

consScanl1VectorLazy, vectorLazyScanl1
  :: (a -> a -> a) -> Data.Vector.Vector a -> Data.Vector.Vector a
consScanl1VectorLazy = scanl1
vectorLazyScanl1 = Data.Vector.scanl1


{- scanr -}
consScanrList, listScanr :: (a -> b -> b) -> b -> [a] -> [b]
consScanrList = scanr
listScanr _ q0 [] =  [q0]
listScanr f q0 (x:xs) = f x q : qs
  where
    qs@(q:_) = listScanr f q0 xs

{-
this test fails because the Cons-based scanr is implemented using the
"static argument transformation". This results in a 3x speedup when operating on lists

inspect ('consScanrList === 'listScanr)
-}

consScanrText, textScanr
  :: (Char -> Char -> Char) -> Char -> Data.Text.Text -> Data.Text.Text
consScanrText = scanr
textScanr = Data.Text.scanr

consScanrTextLazy, textLazyScanr
  :: (Char -> Char -> Char) -> Char -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consScanrTextLazy = scanr
textLazyScanr = Data.Text.Lazy.scanr

consScanrByteString, byteStringScanr
  :: (Word8 -> Word8 -> Word8) -> Word8 -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consScanrByteString = scanr
byteStringScanr = Data.ByteString.scanr

consScanrVectorLazy, vectorLazyScanr
  :: (a -> b -> b) -> b -> Data.Vector.Vector a -> Data.Vector.Vector b
consScanrVectorLazy = scanr
vectorLazyScanr = Data.Vector.scanr


{- scanr1 -}
consScanr1List, listScanr1 :: (a -> a -> a) -> [a] -> [a]
consScanr1List = scanr1
listScanr1 _ [] = []
listScanr1 _ [x] = [x]
listScanr1 f (x:xs) = f x q : qs
  where qs@(q:_) = scanr1 f xs

{-
this test fails due to some kind of irrefutable pattern optimisation that happens in the
list case. The cons-based function is marginally slower.

inspect ('consScanr1List === 'listScanr1)
-}

consScanr1Text, textScanr1
  :: (Char -> Char -> Char) -> Data.Text.Text -> Data.Text.Text
consScanr1Text = scanr1
textScanr1 = Data.Text.scanr1

consScanr1TextLazy, textLazyScanr1
  :: (Char -> Char -> Char) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consScanr1TextLazy = scanr1
textLazyScanr1 = Data.Text.Lazy.scanr1

consScanr1ByteString, byteStringScanr1
  :: (Word8 -> Word8 -> Word8) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consScanr1ByteString = scanr1
byteStringScanr1 = Data.ByteString.scanr1

consScanr1VectorLazy, vectorLazyScanr1
  :: (a -> a -> a) -> Data.Vector.Vector a -> Data.Vector.Vector a
consScanr1VectorLazy = scanr1
vectorLazyScanr1 = Data.Vector.scanr1

scansInspectionTests :: TestTree
scansInspectionTests =
  testGroup "Scans"
    [ $(inspectTest ('consScanlText === 'textScanl))
    , $(inspectTest ('consScanlTextLazy === 'textLazyScanl))
    , $(inspectTest ('consScanlByteString === 'byteStringScanl))
    , $(inspectTest ('consScanlByteStringLazy === 'byteStringLazyScanl))
    , $(inspectTest ('consScanlVectorLazy === 'vectorLazyScanl))
    , $(inspectTest ('consScanl'VectorLazy === 'vectorLazyScanl'))
    , $(inspectTest ('consScanl1List === 'listScanl1))
    , $(inspectTest ('consScanl1Text === 'textScanl1))
    , $(inspectTest ('consScanl1TextLazy === 'textLazyScanl1))
    , $(inspectTest ('consScanl1ByteString === 'byteStringScanl1))
    , $(inspectTest ('consScanl1VectorLazy === 'vectorLazyScanl1))
    , $(inspectTest ('consScanrText === 'textScanr))
    , $(inspectTest ('consScanrTextLazy === 'textLazyScanr))
    , $(inspectTest ('consScanrByteString === 'byteStringScanr))
    , $(inspectTest ('consScanrVectorLazy === 'vectorLazyScanr))
    , $(inspectTest ('consScanr1Text === 'textScanr1))
    , $(inspectTest ('consScanr1TextLazy === 'textLazyScanr1))
    , $(inspectTest ('consScanr1ByteString === 'byteStringScanr1))
    , $(inspectTest ('consScanr1VectorLazy === 'vectorLazyScanr1))
    ]
