{-# language NoImplicitPrelude #-}
module Consy
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
    -- * Basic functions
    -- , List -- not implemented: Data.List's concrete list type is not Cons-generic.
    -- Data.List.(++).
  , append
  , head
  , last
  , tail
  , init
  , uncons
  , singleton
  , null
  , length
    -- Newer Data.List addition.
  , compareLength
    -- * List transformations
  , map
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  , permutations
    -- * Reducing lists (folds)
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
    -- ** Special folds
  , concat
  , concatMap
  , and
  , or
  , any
  , all
  , sum
  , product
  , maximum
  , minimum
    -- * Building lists
    -- ** Scans
  , scanl
  , scanl'
  , scanl1
  , scanr
  , scanr1
    -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
    -- ** Infinite structures
  , iterate
  , iterate'
  , repeat
  , replicate
  , cycle
    -- ** Unfolding
  , unfoldr
    -- * Sublists
    -- ** Extracting substructures
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , dropWhileEnd
  , span
  , break
  , stripPrefix
  , group
  , inits
    -- , inits1 -- not implemented: returns NonEmpty, so it is not Cons-generic.
  , tails
    -- , tails1 -- not implemented: returns NonEmpty, so it is not Cons-generic.
    -- ** Predicates
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , isSubsequenceOf
    -- * Searching lists
    -- ** Searching by equality
  , elem
  , notElem
  , lookup
    -- ** Searching with a predicate
  , find
  , filter
  , partition
    -- * Indexing lists
    -- Newer Data.List addition.
  , (!?)
  , (!!)
  , elemIndex
  , elemIndices
  , findIndex
  , findIndices
    -- * Zipping and unzipping
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zip7
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , zipWith7
  , unzip
  , unzip3
  , unzip4
  , unzip5
  , unzip6
  , unzip7
    -- * Special lists
    -- ** Functions on strings
    -- , lines -- not implemented: string-specific rather than element-polymorphic.
    -- , words -- not implemented: string-specific rather than element-polymorphic.
    -- , unlines -- not implemented: string-specific rather than element-polymorphic.
    -- , unwords -- not implemented: string-specific rather than element-polymorphic.
    -- ** "Set" operations
  , nub
    -- Newer Data.List addition.
  , nubOrd
  , delete
  , (\\)
  , union
  , intersect
    -- ** Ordered lists
  , sort
  , sortOn
  , insert
    -- * Generalized functions
    -- ** The "By" operations
  , nubBy
    -- Newer Data.List addition.
  , nubOrdBy
  , deleteBy
  , deleteFirstsBy
  , unionBy
  , intersectBy
  , groupBy
  , sortBy
  , insertBy
  , maximumBy
  , minimumBy
    -- ** The "generic" operations
  , genericLength
  , genericTake
  , genericDrop
  , genericSplitAt
  , genericIndex
  , genericReplicate
    -- * Consy extras
  , traverse
  , foldMap
  , augment
  , build
  )
where

import Control.Lens.Cons hiding (uncons)
import qualified Control.Lens.Cons as LensCons
import Control.Lens.Empty
import Data.Maybe (Maybe)

import Consy.Basic hiding (uncons)
import Consy.TransformationsMap hiding (uncons)
import Consy.Transformations hiding (uncons)
import Consy.Folds hiding (uncons)
import Consy.SpecialFolds hiding (uncons)
import Consy.AccumulatingMaps hiding (uncons)
import Consy.InfiniteLists hiding (uncons)
import Consy.Unfolding hiding (uncons)
import Consy.ExtractingSublists hiding (uncons)
import Consy.Scans hiding (uncons)
import Consy.SublistsWithPredicates hiding (uncons)
import Consy.SearchingByEquality hiding (uncons)
import Consy.SetOperations hiding (uncons)
import Consy.SearchingWithPredicate hiding (uncons)
import Consy.Traversable
import Consy.Indexing hiding (uncons)
import Consy.Zipping hiding (uncons)
import Consy.OrderedLists hiding (uncons)
import Consy.Generic hiding (uncons)


uncons :: Cons s s a a => s -> Maybe (a, s)
uncons = LensCons.uncons
