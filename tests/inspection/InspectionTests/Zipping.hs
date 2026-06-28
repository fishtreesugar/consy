{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Tasty.Inspection.Plugin #-}
module InspectionTests.Zipping where

import Data.Char (Char)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
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
import qualified Data.Word

import Consy


{- zip -}
consZip, listZip :: [a] -> [b] -> [(a,b)]
consZip = zip
listZip = Data.List.zip

consZipText, textZip :: Text -> Text -> [(Char, Char)]
consZipText = zip
textZip = Data.Text.zip

consZipLazyText, lazyTextZip :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> [(Char, Char)]
consZipLazyText = zip
lazyTextZip = Data.Text.Lazy.zip

consZipVector, vectorZip :: Vector a -> Vector b -> Vector (a,b)
consZipVector = zip
vectorZip = Data.Vector.zip

consZipBS, bsZip ::  Data.ByteString.ByteString -> Data.ByteString.ByteString -> [(Word8, Word8)]
consZipBS = zip
bsZip = Data.ByteString.zip

consZipLBS, lbsZip :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> [(Word8, Word8)]
consZipLBS = zip
lbsZip = Data.ByteString.Lazy.zip

consZipSeq, seqZip :: Seq a -> Seq b -> Seq (a,b)
consZipSeq = zip
seqZip = Data.Sequence.zip


{- zip3 -}
consZip3, listZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
consZip3 = zip3
listZip3 = Data.List.zip3

consZip3Vector, vectorZip3 :: Vector a -> Vector b -> Vector c -> Vector (a,b,c)
consZip3Vector = zip3
vectorZip3 = Data.Vector.zip3

consZip3Seq, seqZip3 :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
consZip3Seq = zip3
seqZip3 = Data.Sequence.zip3


{- zip4 -}
consZip4, listZip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
consZip4 = zip4
listZip4 = Data.List.zip4

consZip4Vector, vectorZip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a,b,c,d)
consZip4Vector = zip4
vectorZip4 = Data.Vector.zip4

consZip4Seq, seqZip4 :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a,b,c,d)
consZip4Seq = zip4
seqZip4 = Data.Sequence.zip4


{- zip5 -}
consZip5, listZip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
consZip5 = zip5
listZip5 = Data.List.zip5

consZip5Vector, vectorZip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a,b,c,d,e)
consZip5Vector = zip5
vectorZip5 = Data.Vector.zip5


{- zip6 -}
consZip6, listZip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
consZip6 = zip6
listZip6 = Data.List.zip6

consZip6Vector, vectorZip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a,b,c,d,e,f)
consZip6Vector = zip6
vectorZip6 = Data.Vector.zip6


{- zip7 -}
consZip7, listZip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
consZip7 = zip7
listZip7 = Data.List.zip7


{- zipWith -}
consZipWith, listZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
consZipWith f = zipWith f
listZipWith = Data.List.zipWith

consZipWithText, textZipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
consZipWithText = zipWith
textZipWith = Data.Text.zipWith

consZipWithLazyText, lazyTextZipWith :: (Char -> Char -> Char) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consZipWithLazyText = zipWith
lazyTextZipWith = Data.Text.Lazy.zipWith

consZipWithVector, vectorZipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
consZipWithVector = zipWith
vectorZipWith = Data.Vector.zipWith

consZipWithBS, bsZipWith :: (Word8 -> Word8 -> a) -> Data.ByteString.ByteString -> Data.ByteString.ByteString -> [a]
consZipWithBS = zipWith
bsZipWith = Data.ByteString.zipWith

consZipWithLBS, lbsZipWith :: (Word8 -> Word8 -> a) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> [a]
consZipWithLBS = zipWith
lbsZipWith = Data.ByteString.Lazy.zipWith

consZipWithSeq, seqZipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
consZipWithSeq = zipWith
seqZipWith = Data.Sequence.zipWith


{- zipWith3 -}
consZipWith3, listZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
consZipWith3 f = zipWith3 f
listZipWith3 = Data.List.zipWith3

consZipWith3Vector, vectorZipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
consZipWith3Vector = zipWith3
vectorZipWith3 = Data.Vector.zipWith3

consZipWith3Seq, seqZipWith3 :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
consZipWith3Seq = zipWith3
seqZipWith3 = Data.Sequence.zipWith3


{- zipWith4 -}
consZipWith4, listZipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
consZipWith4 f = zipWith4 f
listZipWith4 = Data.List.zipWith4

consZipWith4Vector, vectorZipWith4 :: (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
consZipWith4Vector = zipWith4
vectorZipWith4 = Data.Vector.zipWith4

consZipWith4Seq, seqZipWith4 :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
consZipWith4Seq = zipWith4
seqZipWith4 = Data.Sequence.zipWith4


{- zipWith5 -}
consZipWith5, listZipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
consZipWith5 f = zipWith5 f
listZipWith5 = Data.List.zipWith5

consZipWith5Vector, vectorZipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
consZipWith5Vector = zipWith5
vectorZipWith5 = Data.Vector.zipWith5


{- zipWith6 -}
consZipWith6, listZipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
consZipWith6 ff = zipWith6 ff
listZipWith6 = Data.List.zipWith6

consZipWith6Vector, vectorZipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
consZipWith6Vector = zipWith6
vectorZipWith6 = Data.Vector.zipWith6


{- zipWith7 -}
consZipWith7, listZipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
consZipWith7 f = zipWith7 f
listZipWith7 = Data.List.zipWith7


{- unzip -}
consUnzip, listUnzip :: [(a,b)] -> ([a],[b])
consUnzip = unzip
listUnzip =  Data.List.foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

consUnzipVector, vectorUnzip :: Vector (a, b) -> (Vector a, Vector b)
consUnzipVector = unzip
vectorUnzip = Data.Vector.unzip

consUnzipBS, bsUnzip :: [(Word8, Word8)] -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consUnzipBS = unzip
bsUnzip = Data.ByteString.unzip

consUnzipLBS, lbsUnzip :: [(Word8, Word8)] -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consUnzipLBS = unzip
lbsUnzip = Data.ByteString.Lazy.unzip


{- unzip3 -}
consUnzip3, listUnzip3 :: [(a,b,c)] -> ([a],[b],[c])
consUnzip3 = unzip3
listUnzip3 =  Data.List.foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])

consUnzip3Vector, vectorUnzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
consUnzip3Vector = unzip3
vectorUnzip3 = Data.Vector.unzip3


{- unzip4 -}
consUnzip4, listUnzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
consUnzip4 = unzip4
listUnzip4 =  Data.List.foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds)) ([],[],[],[])

consUnzip4Vector, vectorUnzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
consUnzip4Vector = unzip4
vectorUnzip4 = Data.Vector.unzip4


{- unzip5 -}
consUnzip5, listUnzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
consUnzip5 = unzip5
listUnzip5 =  Data.List.foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) -> (a:as,b:bs,c:cs,d:ds,e:es)) ([],[],[],[],[])

consUnzip5Vector, vectorUnzip5 :: Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
consUnzip5Vector = unzip5
vectorUnzip5 = Data.Vector.unzip5


{- unzip6 -}
consUnzip6, listUnzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
consUnzip6 = unzip6
listUnzip6 =  Data.List.foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs)) ([],[],[],[],[],[])

consUnzip6Vector, vectorUnzip6 :: Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
consUnzip6Vector = unzip6
vectorUnzip6 = Data.Vector.unzip6


{- unzip7 -}
consUnzip7, listUnzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
consUnzip7 = unzip7
listUnzip7 =  Data.List.foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs)) ([],[],[],[],[],[],[])

zippingInspectionTests :: TestTree
zippingInspectionTests =
  testGroup "Zipping"
    [ $(inspectTest ('consZip === 'listZip))
    , $(inspectTest ('consZipText === 'textZip))
    , $(inspectTest ('consZipLazyText === 'lazyTextZip))
    , $(inspectTest ('consZipVector === 'vectorZip))
    , $(inspectTest ('consZipBS === 'bsZip))
    , $(inspectTest ('consZipLBS === 'lbsZip))
    , $(inspectTest ('consZipSeq === 'seqZip))
    , $(inspectTest ('consZip3 === 'listZip3))
    , $(inspectTest ('consZip3Vector === 'vectorZip3))
    , $(inspectTest ('consZip3Seq === 'seqZip3))
    , $(inspectTest ('consZip4 === 'listZip4))
    , $(inspectTest ('consZip4Vector === 'vectorZip4))
    , $(inspectTest ('consZip4Seq === 'seqZip4))
    , $(inspectTest ('consZip5 === 'listZip5))
    , $(inspectTest ('consZip5Vector === 'vectorZip5))
    , $(inspectTest ('consZip6 === 'listZip6))
    , $(inspectTest ('consZip6Vector === 'vectorZip6))
    , $(inspectTest ('consZip7 === 'listZip7))
    , $(inspectTest ('consZipWith === 'listZipWith))
    , $(inspectTest ('consZipWithText === 'textZipWith))
    , $(inspectTest ('consZipWithLazyText === 'lazyTextZipWith))
    , $(inspectTest ('consZipWithVector === 'vectorZipWith))
    , $(inspectTest ('consZipWithBS === 'bsZipWith))
    , $(inspectTest ('consZipWithLBS === 'lbsZipWith))
    , $(inspectTest ('consZipWithSeq === 'seqZipWith))
    , $(inspectTest ('consZipWith3 === 'listZipWith3))
    , $(inspectTest ('consZipWith3Vector === 'vectorZipWith3))
    , $(inspectTest ('consZipWith3Seq === 'seqZipWith3))
    , $(inspectTest ('consZipWith4 === 'listZipWith4))
    , $(inspectTest ('consZipWith4Vector === 'vectorZipWith4))
    , $(inspectTest ('consZipWith4Seq === 'seqZipWith4))
    , $(inspectTest ('consZipWith5 === 'listZipWith5))
    , $(inspectTest ('consZipWith5Vector === 'vectorZipWith5))
    , $(inspectTest ('consZipWith6 === 'listZipWith6))
    , $(inspectTest ('consZipWith6Vector === 'vectorZipWith6))
    , $(inspectTest ('consZipWith7 === 'listZipWith7))
    , $(inspectTest ('consUnzip === 'listUnzip))
    , $(inspectTest ('consUnzipVector === 'vectorUnzip))
    , $(inspectTest ('consUnzipBS === 'bsUnzip))
    , $(inspectTest ('consUnzipLBS === 'lbsUnzip))
    , $(inspectTest ('consUnzip3 === 'listUnzip3))
    , $(inspectTest ('consUnzip3Vector === 'vectorUnzip3))
    , $(inspectTest ('consUnzip4 === 'listUnzip4))
    , $(inspectTest ('consUnzip4Vector === 'vectorUnzip4))
    , $(inspectTest ('consUnzip5 === 'listUnzip5))
    , $(inspectTest ('consUnzip5Vector === 'vectorUnzip5))
    , $(inspectTest ('consUnzip6 === 'listUnzip6))
    , $(inspectTest ('consUnzip6Vector === 'vectorUnzip6))
    , $(inspectTest ('consUnzip7 === 'listUnzip7))
    ]
