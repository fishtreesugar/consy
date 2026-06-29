{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.OrderedLists
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , sort
  , sortOn
  , insert
  , sortBy
  , insertBy
  , maximumBy
  , minimumBy
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, Ordering(..), compare)
import Data.Vector (Vector)
import GHC.List (errorEmptyList)

import qualified Data.ByteString as BS
import qualified Data.List
import qualified Data.Vector


{-# inline [2] sort #-}
-- sort :: Ord a => [a] -> [a]
sort :: (AsEmpty s, Cons s s a a, Ord a) => s -> s
sort = sortBy compare

{-# rules
"cons sort list" [~2]
    sort @[_] = Data.List.sort
"cons sort list eta" [~2]
    forall xs.
    sort @[_] xs = Data.List.sort xs
#-}


{-# inline [2] sortOn #-}
-- sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn :: (AsEmpty s, Cons s s a a, Ord b) => (a -> b) -> s -> s
sortOn f = fromList . Data.List.sortOn f . toList

{-# rules
"cons sortOn list" [~2]
    sortOn @[_] = Data.List.sortOn
"cons sortOn list eta" [~2]
    forall f xs.
    sortOn @[_] f xs = Data.List.sortOn f xs
#-}


{-# inline [2] insert #-}
-- insert :: Ord a => a -> [a] -> [a]
insert :: (AsEmpty s, Cons s s a a, Ord a) => a -> s -> s
insert = insertBy compare

{-# rules
"cons insert list" [~2]
    insert @[_] = Data.List.insert
"cons insert list eta" [~2]
    forall x xs.
    insert @[_] x xs = Data.List.insert x xs
#-}


{-# inline [2] sortBy #-}
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Ordering) -> s -> s
sortBy cmp = fromList . Data.List.sortBy cmp . toList

{-# rules
"cons sortBy list" [~2]
    sortBy @[_] = Data.List.sortBy
"cons sortBy list eta" [~2]
    forall cmp xs.
    sortBy @[_] cmp xs = Data.List.sortBy cmp xs

"cons sort bs" [~2]
    sort @BS.ByteString = BS.sort
"cons sort bs eta" [~2]
    forall xs.
    sort @BS.ByteString xs = BS.sort xs
#-}


{-# inline [2] insertBy #-}
-- insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Ordering) -> a -> s -> s
insertBy cmp x xs =
  case uncons xs of
    Nothing -> x `cons` Empty
    Just (y, ys) ->
      case cmp x y of
        GT -> y `cons` insertBy cmp x ys
        _ -> x `cons` xs

{-# rules
"cons insertBy list" [~2]
    insertBy @[_] = Data.List.insertBy
"cons insertBy list eta" [~2]
    forall cmp x xs.
    insertBy @[_] cmp x xs = Data.List.insertBy cmp x xs
#-}


{-# inline [2] maximumBy #-}
-- maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy :: Cons s s a a => (a -> a -> Ordering) -> s -> a
maximumBy cmp xs =
  case uncons xs of
    Nothing -> errorEmptyList "maximumBy"
    Just (x, xs') -> go x xs'
  where
    go best rest =
      case uncons rest of
        Nothing -> best
        Just (x, xs') ->
          case cmp x best of
            GT -> go x xs'
            _ -> go best xs'

{-# rules
"cons maximumBy list" [~2]
    maximumBy @[_] = Data.List.maximumBy
"cons maximumBy list eta" [~2]
    forall cmp xs.
    maximumBy @[_] cmp xs = Data.List.maximumBy cmp xs

"cons maximumBy vector" [~2]
    maximumBy @(Vector _) = Data.Vector.maximumBy
"cons maximumBy vector eta" [~2]
    forall cmp xs.
    maximumBy @(Vector _) cmp xs = Data.Vector.maximumBy cmp xs
#-}


{-# inline [2] minimumBy #-}
-- minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy :: Cons s s a a => (a -> a -> Ordering) -> s -> a
minimumBy cmp xs =
  case uncons xs of
    Nothing -> errorEmptyList "minimumBy"
    Just (x, xs') -> go x xs'
  where
    go best rest =
      case uncons rest of
        Nothing -> best
        Just (x, xs') ->
          case cmp x best of
            LT -> go x xs'
            _ -> go best xs'

{-# rules
"cons minimumBy list" [~2]
    minimumBy @[_] = Data.List.minimumBy
"cons minimumBy list eta" [~2]
    forall cmp xs.
    minimumBy @[_] cmp xs = Data.List.minimumBy cmp xs

"cons minimumBy vector" [~2]
    minimumBy @(Vector _) = Data.Vector.minimumBy
"cons minimumBy vector eta" [~2]
    forall cmp xs.
    minimumBy @(Vector _) cmp xs = Data.Vector.minimumBy cmp xs
#-}


{-# inline toList #-}
toList :: Cons s s a a => s -> [a]
toList xs =
  case uncons xs of
    Nothing -> []
    Just (x, xs') -> x : toList xs'


{-# inline fromList #-}
fromList :: (AsEmpty s, Cons s s a a) => [a] -> s
fromList [] = Empty
fromList (x : xs) = x `cons` fromList xs
