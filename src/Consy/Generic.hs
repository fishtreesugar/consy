{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.Generic
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , genericLength
  , genericTake
  , genericDrop
  , genericSplitAt
  , genericIndex
  , genericReplicate
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=), (<))
import GHC.Base (errorWithoutStackTrace)
import GHC.Num (Num(..))
import GHC.Real (Integral)

import qualified Data.List


{-# inline [2] genericLength #-}
-- genericLength :: Num i => [a] -> i
genericLength :: (Cons s s a a, Num i) => s -> i
genericLength = go 0
  where
    go n xs =
      case uncons xs of
        Nothing -> n
        Just (_, xs') -> go (n + 1) xs'

{-# rules
"cons genericLength list" [~2]
    genericLength @[_] = Data.List.genericLength
"cons genericLength list eta" [~2]
    forall xs.
    genericLength @[_] xs = Data.List.genericLength xs
#-}


{-# inline [2] genericTake #-}
-- genericTake :: Integral i => i -> [a] -> [a]
genericTake :: (AsEmpty s, Cons s s a a, Integral i) => i -> s -> s
genericTake n xs
  | n <= 0 = Empty
  | otherwise =
      case uncons xs of
        Nothing -> Empty
        Just (x, xs') -> x `cons` genericTake (n - 1) xs'

{-# rules
"cons genericTake list" [~2]
    genericTake @[_] = Data.List.genericTake
"cons genericTake list eta" [~2]
    forall n xs.
    genericTake @[_] n xs = Data.List.genericTake n xs
#-}


{-# inline [2] genericDrop #-}
-- genericDrop :: Integral i => i -> [a] -> [a]
genericDrop :: (AsEmpty s, Cons s s a a, Integral i) => i -> s -> s
genericDrop n xs
  | n <= 0 = xs
  | otherwise =
      case uncons xs of
        Nothing -> Empty
        Just (_, xs') -> genericDrop (n - 1) xs'

{-# rules
"cons genericDrop list" [~2]
    genericDrop @[_] = Data.List.genericDrop
"cons genericDrop list eta" [~2]
    forall n xs.
    genericDrop @[_] n xs = Data.List.genericDrop n xs
#-}


{-# inline [2] genericSplitAt #-}
-- genericSplitAt :: Integral i => i -> [a] -> ([a], [a])
genericSplitAt :: (AsEmpty s, Cons s s a a, Integral i) => i -> s -> (s, s)
genericSplitAt n xs
  | n <= 0 = (Empty, xs)
  | otherwise =
      case uncons xs of
        Nothing -> (Empty, Empty)
        Just (x, xs') ->
          case genericSplitAt (n - 1) xs' of
            (ys, zs) -> (x `cons` ys, zs)

{-# rules
"cons genericSplitAt list" [~2]
    genericSplitAt @[_] = Data.List.genericSplitAt
"cons genericSplitAt list eta" [~2]
    forall n xs.
    genericSplitAt @[_] n xs = Data.List.genericSplitAt n xs
#-}


{-# inline [2] genericIndex #-}
-- genericIndex :: Integral i => [a] -> i -> a
genericIndex :: (Cons s s a a, Integral i) => s -> i -> a
genericIndex xs n
  | n < 0 = errorWithoutStackTrace "List.genericIndex: negative argument."
  | otherwise = go xs n
  where
    go ys i =
      case uncons ys of
        Nothing -> errorWithoutStackTrace "List.genericIndex: index too large."
        Just (y, ys')
          | i == 0 -> y
          | otherwise -> go ys' (i - 1)

{-# rules
"cons genericIndex list" [~2]
    genericIndex @[_] = Data.List.genericIndex
"cons genericIndex list eta" [~2]
    forall xs n.
    genericIndex @[_] xs n = Data.List.genericIndex xs n
#-}


{-# inline [2] genericReplicate #-}
-- genericReplicate :: Integral i => i -> a -> [a]
genericReplicate :: (AsEmpty s, Cons s s a a, Integral i) => i -> a -> s
genericReplicate n x
  | n <= 0 = Empty
  | otherwise = x `cons` genericReplicate (n - 1) x

{-# rules
"cons genericReplicate list" [~2]
    genericReplicate @[_] = Data.List.genericReplicate
"cons genericReplicate list eta" [~2]
    forall n x.
    genericReplicate @[_] n x = Data.List.genericReplicate n x
#-}
