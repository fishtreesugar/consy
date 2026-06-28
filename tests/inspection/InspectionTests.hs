module InspectionTests
  ( inspectionTests
  , module InspectionTests.AccumulatingMaps
  , module InspectionTests.Basic
  , module InspectionTests.ExtractingSublists
  , module InspectionTests.Folds
  , module InspectionTests.Indexing
  , module InspectionTests.InfiniteLists
  , module InspectionTests.Scans
  , module InspectionTests.SearchingByEquality
  , module InspectionTests.SearchingWithPredicate
  , module InspectionTests.SpecialFolds
  , module InspectionTests.SublistsWithPredicates
  , module InspectionTests.Transformations
  , module InspectionTests.TransformationsMap
  , module InspectionTests.Unfolding
  , module InspectionTests.Zipping
  )
where

import Test.Tasty (TestTree, testGroup)

import InspectionTests.AccumulatingMaps
import InspectionTests.Basic
import InspectionTests.ExtractingSublists
import InspectionTests.Folds
import InspectionTests.Indexing
import InspectionTests.InfiniteLists
import InspectionTests.Scans
import InspectionTests.SearchingByEquality
import InspectionTests.SearchingWithPredicate
import InspectionTests.SpecialFolds
import InspectionTests.SublistsWithPredicates
import InspectionTests.Transformations
import InspectionTests.TransformationsMap
import InspectionTests.Unfolding
import InspectionTests.Zipping

inspectionTests :: TestTree
inspectionTests =
  testGroup "inspection"
    [ accumulatingMapsInspectionTests
    , basicInspectionTests
    , extractingSublistsInspectionTests
    , foldsInspectionTests
    , indexingInspectionTests
    , infiniteListsInspectionTests
    , scansInspectionTests
    , searchingByEqualityInspectionTests
    , searchingWithPredicateInspectionTests
    , specialFoldsInspectionTests
    , sublistsWithPredicatesInspectionTests
    , transformationsInspectionTests
    , transformationsMapInspectionTests
    , unfoldingInspectionTests
    , zippingInspectionTests
    ]
