module Main where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Consy
import qualified Data.List
import qualified InspectionTests

prop_transpose_correct :: Property
prop_transpose_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 10) $
      Gen.string (Range.constant 0 10) Gen.unicode
    Data.List.transpose str === Consy.transpose str

prop_permutations_correct :: Property
prop_permutations_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 5) $
      Gen.string (Range.constant 0 5) Gen.unicode
    Data.List.permutations str === Consy.permutations str

prop_inits_correct :: Property
prop_inits_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 10) $
      Gen.string (Range.constant 0 10) Gen.unicode
    Data.List.inits str === Consy.inits str
    Data.List.inits str === InspectionTests.listInits str

main :: IO ()
main =
  defaultMain $
    testGroup "consy"
      [ InspectionTests.inspectionTests
      , testGroup "properties"
        [ testProperty "transpose" prop_transpose_correct
        , testProperty "permutations" prop_permutations_correct
        , testProperty "inits" prop_inits_correct
        ]
      ]
