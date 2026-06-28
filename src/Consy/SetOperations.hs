{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.SetOperations
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , nub
  , nubOrd
  , delete
  , (\\)
  , union
  , intersect
  , nubBy
  , nubOrdBy
  , deleteBy
  , deleteFirstsBy
  , unionBy
  , intersectBy
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (||), otherwise)
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, Ordering(..), compare)

import qualified Data.List


data SetBy a
  = Tip
  | Bin (SetBy a) a (SetBy a)


{-# inline [2] nub #-}
-- nub :: Eq a => [a] -> [a]
nub :: (AsEmpty s, Cons s s a a, Eq a) => s -> s
nub = nubBy (==)

{-# rules
"cons nub list" [~2]
    nub @[_] = Data.List.nub
"cons nub list eta" [~2]
    forall xs.
    nub @[_] xs = Data.List.nub xs
#-}


{-# inline [2] nubOrd #-}
-- nubOrd :: Ord a => [a] -> [a]
nubOrd :: (AsEmpty s, Cons s s a a, Ord a) => s -> s
nubOrd = nubOrdBy compare

{-# rules
"cons nubOrd list" [~2]
    nubOrd @[_] = Data.List.nub
"cons nubOrd list eta" [~2]
    forall xs.
    nubOrd @[_] xs = Data.List.nub xs
#-}


{-# inline [2] delete #-}
-- delete :: Eq a => a -> [a] -> [a]
delete :: (AsEmpty s, Cons s s a a, Eq a) => a -> s -> s
delete = deleteBy (==)

{-# rules
"cons delete list" [~2]
    delete @[_] = Data.List.delete
"cons delete list eta" [~2]
    forall x xs.
    delete @[_] x xs = Data.List.delete x xs
#-}


{-# inline [2] (\\) #-}
-- (\\) :: Eq a => [a] -> [a] -> [a]
(\\) :: (AsEmpty s, Cons s s a a, Eq a) => s -> s -> s
(\\) = deleteFirstsBy (==)
infix 5 \\

{-# rules
"cons difference list" [~2]
    (\\) @[_] = (Data.List.\\)
"cons difference list eta" [~2]
    forall xs ys.
    (\\) @[_] xs ys = (Data.List.\\) xs ys
#-}


{-# inline [2] union #-}
-- union :: Eq a => [a] -> [a] -> [a]
union :: (AsEmpty s, Cons s s a a, Eq a) => s -> s -> s
union = unionBy (==)

{-# rules
"cons union list" [~2]
    union @[_] = Data.List.union
"cons union list eta" [~2]
    forall xs ys.
    union @[_] xs ys = Data.List.union xs ys
#-}


{-# inline [2] intersect #-}
-- intersect :: Eq a => [a] -> [a] -> [a]
intersect :: (AsEmpty s, Cons s s a a, Eq a) => s -> s -> s
intersect = intersectBy (==)

{-# rules
"cons intersect list" [~2]
    intersect @[_] = Data.List.intersect
"cons intersect list eta" [~2]
    forall xs ys.
    intersect @[_] xs ys = Data.List.intersect xs ys
#-}


{-# inline [2] nubBy #-}
-- nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Bool) -> s -> s
nubBy eq = go []
  where
    go seen xs =
      case uncons xs of
        Nothing -> Empty
        Just (x, xs')
          | elemBy eq x seen -> go seen xs'
          | otherwise -> x `cons` go (x : seen) xs'

{-# rules
"cons nubBy list" [~2]
    nubBy @[_] = Data.List.nubBy
"cons nubBy list eta" [~2]
    forall eq xs.
    nubBy @[_] eq xs = Data.List.nubBy eq xs
#-}


{-# inline [2] nubOrdBy #-}
-- nubOrdBy :: (a -> a -> Ordering) -> [a] -> [a]
nubOrdBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Ordering) -> s -> s
nubOrdBy cmp = go Tip
  where
    go seen xs =
      case uncons xs of
        Nothing -> Empty
        Just (x, xs') ->
          case insertBy cmp x seen of
            (False, seen') -> go seen' xs'
            (True, seen') -> x `cons` go seen' xs'

{-# rules
"cons nubOrdBy list" [~2]
    nubOrdBy @[_] = \cmp -> Data.List.nubBy (\x y -> cmp x y == EQ)
"cons nubOrdBy list eta" [~2]
    forall cmp xs.
    nubOrdBy @[_] cmp xs = Data.List.nubBy (\x y -> cmp x y == EQ) xs
#-}


{-# inline [2] deleteBy #-}
-- deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Bool) -> a -> s -> s
deleteBy eq x = go
  where
    go xs =
      case uncons xs of
        Nothing -> Empty
        Just (y, ys)
          | eq x y -> ys
          | otherwise -> y `cons` go ys

{-# rules
"cons deleteBy list" [~2]
    deleteBy @[_] = Data.List.deleteBy
"cons deleteBy list eta" [~2]
    forall eq x xs.
    deleteBy @[_] eq x xs = Data.List.deleteBy eq x xs
#-}


{-# inline [2] deleteFirstsBy #-}
-- deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Bool) -> s -> s -> s
deleteFirstsBy eq = go
  where
    go xs ys =
      case uncons ys of
        Nothing -> xs
        Just (y, ys') -> go (deleteBy eq y xs) ys'

{-# rules
"cons deleteFirstsBy list" [~2]
    deleteFirstsBy @[_] = Data.List.deleteFirstsBy
"cons deleteFirstsBy list eta" [~2]
    forall eq xs ys.
    deleteFirstsBy @[_] eq xs ys = Data.List.deleteFirstsBy eq xs ys
#-}


{-# inline [2] unionBy #-}
-- unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Bool) -> s -> s -> s
unionBy eq xs ys = append xs (go (toList xs) ys)
  where
    go seen rest =
      case uncons rest of
        Nothing -> Empty
        Just (y, ys')
          | elemBy eq y seen -> go seen ys'
          | otherwise -> y `cons` go (y : seen) ys'

{-# rules
"cons unionBy list" [~2]
    unionBy @[_] = Data.List.unionBy
"cons unionBy list eta" [~2]
    forall eq xs ys.
    unionBy @[_] eq xs ys = Data.List.unionBy eq xs ys
#-}


{-# inline [2] intersectBy #-}
-- intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy :: (AsEmpty s, Cons s s a a) => (a -> a -> Bool) -> s -> s -> s
intersectBy eq xs ys = go xs
  where
    go rest =
      case uncons rest of
        Nothing -> Empty
        Just (x, xs')
          | elemIn eq x ys -> x `cons` go xs'
          | otherwise -> go xs'

{-# rules
"cons intersectBy list" [~2]
    intersectBy @[_] = Data.List.intersectBy
"cons intersectBy list eta" [~2]
    forall eq xs ys.
    intersectBy @[_] eq xs ys = Data.List.intersectBy eq xs ys
#-}


{-# inline [2] insertBy #-}
insertBy :: (a -> a -> Ordering) -> a -> SetBy a -> (Bool, SetBy a)
insertBy _ x Tip = (True, Bin Tip x Tip)
insertBy cmp x (Bin l y r) =
  case cmp x y of
    LT ->
      case insertBy cmp x l of
        (isNew, l') -> (isNew, Bin l' y r)
    EQ -> (False, Bin l y r)
    GT ->
      case insertBy cmp x r of
        (isNew, r') -> (isNew, Bin l y r')


{-# inline elemBy #-}
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy eq x (y : ys) = eq x y || elemBy eq x ys


{-# inline elemIn #-}
elemIn :: Cons s s a a => (a -> a -> Bool) -> a -> s -> Bool
elemIn eq x xs =
  case uncons xs of
    Nothing -> False
    Just (y, ys) -> eq x y || elemIn eq x ys


{-# inline toList #-}
toList :: Cons s s a a => s -> [a]
toList xs =
  case uncons xs of
    Nothing -> []
    Just (x, xs') -> x : toList xs'


{-# inline append #-}
append :: Cons s s a a => s -> s -> s
append xs ys =
  case uncons xs of
    Nothing -> ys
    Just (x, xs') -> x `cons` append xs' ys
