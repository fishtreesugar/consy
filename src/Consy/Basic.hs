{-# language BangPatterns #-}
{-# language CPP #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.Basic
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , append
  , head
  , last
  , tail
  , init
  , singleton
  , null
  , length
  , compareLength
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), otherwise)
import Data.Int (Int)
import Data.Function ((.), id)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering(..), compare, (<), (>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.List (errorEmptyList)
import GHC.Num ((+), (-))
import GHC.Real (Integral, fromIntegral)
import GHC.Stack (withFrozenCallStack)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (augment, build, foldr, foldl)


{-# inline [1] append #-}
-- (++) :: [a] -> [a] -> [a]
append :: Cons s s a a => s -> s -> s
append = go
  where
    go a b =
      case uncons a of
        Nothing -> b
        Just (x, xs) -> x `cons` go xs b

{-# rules
"cons ++" [~1]
    forall xs ys.
    append xs ys = augment (\c n -> foldr c n xs) ys
"cons foldr/app" [1]
    forall ys.
    foldr (:) ys = \xs -> append xs ys

"cons append text" [~2]
    append @Text = Data.Text.append
"cons append text eta" [~2]
    forall a b.
    append @Text a b = Data.Text.append a b

"cons append ltext" [~2]
    append @Data.Text.Lazy.Text = Data.Text.Lazy.append
"cons append ltext eta" [~2]
    forall a b.
    append @Data.Text.Lazy.Text a b = Data.Text.Lazy.append a b

"cons append vector" [~2]
    append @(Vector _) = (Data.Vector.++)
"cons append vector eta" [~2]
    forall a b.
    append @(Vector _) a b = (Data.Vector.++) a b

"cons append bs" [~2]
    append @BS.ByteString = BS.append
"cons append bs eta" [~2]
    forall a b.
    append @BS.ByteString a b = BS.append a b

"cons append lbs" [~2]
    append @LBS.ByteString = LBS.append
"cons append lbs eta" [~2]
    forall a b.
    append @LBS.ByteString a b = LBS.append a b

"cons append seq" [~2]
    append @(Seq _) = (Data.Sequence.><)
"cons append seq eta" [~2]
    forall a b.
    append @(Seq _) a b = (Data.Sequence.><) a b

"cons append list" [~2]
    append @[_] = (Data.List.++)
"cons append list eta" [~2]
    forall a b.
    append @[_] a b = (Data.List.++) a b
#-}


{-# inline [2] singleton #-}
-- singleton :: a -> [a]
singleton :: (AsEmpty s, Cons s s a a) => a -> s
singleton x = x `cons` Empty

{-# rules
"cons singleton text" [~2]
    singleton @Text = Data.Text.singleton
"cons singleton text eta" [~2]
    forall x.
    singleton @Text x = Data.Text.singleton x

"cons singleton ltext" [~2]
    singleton @Data.Text.Lazy.Text = Data.Text.Lazy.singleton
"cons singleton ltext eta" [~2]
    forall x.
    singleton @Data.Text.Lazy.Text x = Data.Text.Lazy.singleton x

"cons singleton vector" [~2]
    singleton @(Vector _) = Data.Vector.singleton
"cons singleton vector eta" [~2]
    forall x.
    singleton @(Vector _) x = Data.Vector.singleton x

"cons singleton bs" [~2]
    singleton @BS.ByteString = BS.singleton
"cons singleton bs eta" [~2]
    forall x.
    singleton @BS.ByteString x = BS.singleton x

"cons singleton lbs" [~2]
    singleton @LBS.ByteString = LBS.singleton
"cons singleton lbs eta" [~2]
    forall x.
    singleton @LBS.ByteString x = LBS.singleton x

"cons singleton seq" [~2]
    singleton @(Seq _) = Data.Sequence.singleton
"cons singleton seq eta" [~2]
    forall x.
    singleton @(Seq _) x = Data.Sequence.singleton x

"cons singleton list" [~2]
    singleton @[_] = Data.List.singleton
"cons singleton list eta" [~2]
    forall x.
    singleton @[_] x = Data.List.singleton x
#-}

{-# noinline [1] head #-}
-- head :: [a] -> a
head :: Cons s s a a => s -> a
head = \s ->
  case uncons s of
    Nothing -> errorEmptyList "head"
    Just (x, _) -> x

{-# rules
"cons head/build"
    forall (g::forall b.(a->b->b)->b->b).
    head (build g) = g (\x _ -> x) (errorEmptyList "head")
"cons head/augment"
    forall xs (g::forall b. (a->b->b) -> b -> b).
    head (augment g xs) = g (\x _ -> x) (head xs)

"cons head text" [~2]
    head @Text = withFrozenCallStack Data.Text.head
"cons head text eta" [~2]
    forall a.
    head @Text a = withFrozenCallStack Data.Text.head a

"cons head ltext" [~2]
    head @Data.Text.Lazy.Text = withFrozenCallStack Data.Text.Lazy.head
"cons head ltext eta" [~2]
    forall a.
    head @Data.Text.Lazy.Text a = withFrozenCallStack Data.Text.Lazy.head a

"cons head vector" [~2]
    head @(Vector _) = Data.Vector.head
"cons head vector eta" [~2]
    forall a.
    head @(Vector _) a = Data.Vector.head a

"cons head bs" [~2]
    head @BS.ByteString = withFrozenCallStack BS.head
"cons head bs eta" [~2]
    forall a.
    head @BS.ByteString a = withFrozenCallStack BS.head a

"cons head lbs" [~2]
    head @LBS.ByteString = withFrozenCallStack LBS.head
"cons head lbs eta" [~2]
    forall a.
    head @LBS.ByteString a = withFrozenCallStack LBS.head a
#-}


{-# inline [1] last #-}
-- last :: [a] -> a
last :: (AsEmpty s, Cons s s a a) => s -> a
last = \xs -> foldl (\_ x -> x) (withFrozenCallStack errorEmptyList "last") xs

{-# rules
"cons last text" [~2]
    last @Text = withFrozenCallStack Data.Text.last
"cons last text eta" [~2]
    forall a.
    -- last @Data.Text.Text a = Data.Text.last a
    last @Data.Text.Text a = Data.Text.foldl (\_ x -> x) (withFrozenCallStack errorEmptyList "tail") a

"cons last ltext" [~2]
    last @Data.Text.Lazy.Text = withFrozenCallStack Data.Text.Lazy.last
"cons last ltext eta" [~2]
    forall a.
    last @Data.Text.Lazy.Text a = withFrozenCallStack Data.Text.Lazy.last a

"cons last vector" [~2]
    last @(Vector _) = Data.Vector.last
"cons last vector eta" [~2]
    forall a.
    last @(Vector _) a = Data.Vector.last a

"cons last bs" [~2]
    last @BS.ByteString = withFrozenCallStack BS.last
"cons last bs eta" [~2]
    forall a.
    last @BS.ByteString a = withFrozenCallStack BS.last a

"cons last lbs" [~2]
    last @LBS.ByteString = withFrozenCallStack LBS.last
"cons last lbs eta" [~2]
    forall a.
    last @LBS.ByteString a = withFrozenCallStack LBS.last a
#-}


{-# inline [1] tail #-}
-- tail :: [a] -> [a]
tail :: (Cons s s a a) => s -> s
tail = \s ->
  case uncons s of
    Nothing -> withFrozenCallStack errorEmptyList "tail"
    Just (_, xs) -> xs

{-# rules
"cons tail text" [~2]
    tail @Text = withFrozenCallStack Data.Text.tail
"cons tail text eta" [~2]
    forall a.
    tail @Data.Text.Text a = withFrozenCallStack Data.Text.tail a

"cons tail ltext" [~2]
    tail @Data.Text.Lazy.Text = withFrozenCallStack Data.Text.Lazy.tail
"cons tail ltext eta" [~2]
    forall a.
    tail @Data.Text.Lazy.Text a = withFrozenCallStack Data.Text.Lazy.tail a

"cons tail vector" [~2]
    tail @(Vector _) = Data.Vector.tail
"cons tail vector eta" [~2]
    forall a.
    tail @(Vector _) a = Data.Vector.tail a

"cons tail bs" [~2]
    tail @BS.ByteString = withFrozenCallStack BS.tail
"cons tail bs eta" [~2]
    forall a.
    tail @BS.ByteString a = withFrozenCallStack BS.tail a

"cons tail lbs" [~2]
    tail @LBS.ByteString = withFrozenCallStack LBS.tail
"cons tail lbs eta" [~2]
    forall a.
    tail @LBS.ByteString a = withFrozenCallStack LBS.tail a
#-}


{-# inline [2] init #-}
-- init :: [a] -> [a]
init :: (AsEmpty s, Cons s s a a) => s -> s
init = go
  where
    go s =
      case uncons s of
        Nothing -> withFrozenCallStack errorEmptyList "init"
        Just (x, xs) -> init' x xs
          where
            init' _ Empty = Empty
            init' y ys = case uncons ys of
                          Nothing -> y `cons` Empty
                          Just (z, zs) -> y `cons` (init' z zs)

{-# rules
"cons init text" [~2]
    init @Text = withFrozenCallStack Data.Text.init
"cons init text eta" [~2]
    forall a.
    init @Data.Text.Text a = withFrozenCallStack Data.Text.init a

"cons init ltext" [~2]
    init @Data.Text.Lazy.Text = withFrozenCallStack Data.Text.Lazy.init
"cons init ltext eta" [~2]
    forall a.
    init @Data.Text.Lazy.Text a = withFrozenCallStack Data.Text.Lazy.init a

"cons init vector" [~2]
    init @(Vector _) = Data.Vector.init
"cons init vector eta" [~2]
    forall a.
    init @(Vector _) a = Data.Vector.init a

"cons init bs" [~2]
    init @BS.ByteString = withFrozenCallStack BS.init
"cons init bs eta" [~2]
    forall a.
    init @BS.ByteString a = withFrozenCallStack BS.init a

"cons init lbs" [~2]
    init @LBS.ByteString = withFrozenCallStack LBS.init
"cons init lbs eta" [~2]
    forall a.
    init @LBS.ByteString a = withFrozenCallStack LBS.init a

"cons init list" [~2]
    init @[_] = withFrozenCallStack Data.List.init
"cons init list eta" [~2]
    forall a.
    init @[_] a = withFrozenCallStack Data.List.init a
#-}

{-# inline [2] null #-}
-- null :: [a] -> Bool
null :: (AsEmpty s, Cons s s a a) => s -> Bool
null = \s ->
      case uncons s of
        Nothing -> True
        _ -> False

{-# rules
"cons null text" [~2]
    null @Text = Data.Text.null
"cons null text eta" [~2]
    forall a.
    null @Data.Text.Text a = Data.Text.null a

"cons null ltext" [~2]
    null @Data.Text.Lazy.Text = Data.Text.Lazy.null
"cons null ltext eta" [~2]
    forall a.
    null @Data.Text.Lazy.Text a = Data.Text.Lazy.null a

"cons null vector" [~2]
    null @(Vector _) = Data.Vector.null
"cons null vector eta" [~2]
    forall a.
    null @(Vector _) a = Data.Vector.null a

"cons null bs" [~2]
    null @BS.ByteString = BS.null
"cons null bs eta" [~2]
    forall a.
    null @BS.ByteString a = BS.null a

"cons null lbs" [~2]
    null @LBS.ByteString = LBS.null
"cons null lbs eta" [~2]
    forall a.
    null @LBS.ByteString a = LBS.null a

"cons null seq" [~2]
    null @(Seq _) = Data.Sequence.null
"cons null seq eta" [~2]
    forall a.
    null @(Seq _) a = Data.Sequence.null a
#-}


{-# inline [0] length #-}
-- length :: [a] -> Int
length :: (Cons s s a a, Integral n) => s -> n
length = go 0
  where
    go !n s =
      case uncons s of
        Nothing -> n
        Just (_, xs) -> go (n+1) xs

-- lenACC :: [a] -> Int -> Int
lenAcc :: (Cons s s a a, Integral n) => n -> s -> n
lenAcc !n s =
  case uncons s of
    Nothing -> n
    Just (_, xs) -> lenAcc (n+1) xs

{-# rules
"cons length" [~1]
    forall xs.
    length xs = foldr lengthFB idLength xs 0
"cons lengthList" [1]
    foldr lengthFB idLength = lenAcc

"cons length text"
    length @Text = fromIntegral . Data.Text.length
"cons length text eta"
    forall xs.
    length @Text xs = fromIntegral (Data.Text.length xs)

"cons length ltext"
    length @Data.Text.Lazy.Text = fromIntegral . Data.Text.Lazy.length
"cons length ltext eta"
    forall xs.
    length @Data.Text.Lazy.Text xs = fromIntegral (Data.Text.Lazy.length xs)

"cons length vector"
    length @(Vector _) = fromIntegral . Data.Vector.length
"cons length vector eta"
    forall xs.
    length @(Vector _) xs = fromIntegral (Data.Vector.length xs)

"cons length bs"
    length @BS.ByteString = BS.length
"cons length bs eta"
    forall xs.
    length @BS.ByteString xs = fromIntegral (BS.length xs)

"cons length bslazy"
    length @LBS.ByteString = LBS.length
"cons length bslazy eta"
    forall xs.
    length @LBS.ByteString xs = fromIntegral (LBS.length xs)

"cons length seq"
    length @(Seq _) = Data.Sequence.length
"cons length seq eta"
    forall xs.
    length @(Seq _) xs = fromIntegral (Data.Sequence.length xs)
 #-}

{-# inline [0] lengthFB #-}
-- lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# inline [0] idLength #-}
-- idLength :: Int -> Int
idLength :: Int -> Int
idLength = id


{-# inline [2] compareLength #-}
-- compareLength :: [a] -> Int -> Ordering
compareLength :: Cons s s a a => s -> Int -> Ordering
compareLength xs n
  | n < 0 = GT
  | otherwise =
      foldr
        (\_ f m -> if m > 0 then f (m - 1) else GT)
        (\m -> if m > 0 then LT else EQ)
        xs
        n

{-# rules
"cons compareLength text" [~2]
    compareLength @Text = Data.Text.compareLength
"cons compareLength text eta" [~2]
    forall xs n.
    compareLength @Text xs n = Data.Text.compareLength xs n

"cons compareLength ltext" [~2]
    compareLength @Data.Text.Lazy.Text = \xs n -> Data.Text.Lazy.compareLength xs (fromIntegral n)
"cons compareLength ltext eta" [~2]
    forall xs n.
    compareLength @Data.Text.Lazy.Text xs n = Data.Text.Lazy.compareLength xs (fromIntegral n)

"cons compareLength vector" [~2]
    compareLength @(Vector _) = \xs n -> compare (Data.Vector.length xs) n
"cons compareLength vector eta" [~2]
    forall xs n.
    compareLength @(Vector _) xs n = compare (Data.Vector.length xs) n

"cons compareLength bs" [~2]
    compareLength @BS.ByteString = \xs n -> compare (BS.length xs) n
"cons compareLength bs eta" [~2]
    forall xs n.
    compareLength @BS.ByteString xs n = compare (BS.length xs) n

"cons compareLength lbs" [~2]
    compareLength @LBS.ByteString = \xs n -> compare (LBS.length xs) (fromIntegral n)
"cons compareLength lbs eta" [~2]
    forall xs n.
    compareLength @LBS.ByteString xs n = compare (LBS.length xs) (fromIntegral n)

"cons compareLength seq" [~2]
    compareLength @(Seq _) = \xs n -> compare (Data.Sequence.length xs) n
"cons compareLength seq eta" [~2]
    forall xs n.
    compareLength @(Seq _) xs n = compare (Data.Sequence.length xs) n
#-}

#if MIN_VERSION_base(4,21,0)
{-# rules
"cons compareLength list" [~2]
    compareLength @[_] = Data.List.compareLength
"cons compareLength list eta" [~2]
    forall xs n.
    compareLength @[_] xs n = Data.List.compareLength xs n
#-}
#endif
