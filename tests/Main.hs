module Main where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Consy
import qualified Data.List
import qualified Data.Text as Text
import qualified InspectionTests

expectedUnsnoc :: [a] -> Maybe ([a], a)
expectedUnsnoc [] = Nothing
expectedUnsnoc xs = Just (Data.List.init xs, Data.List.last xs)

expectedIndexMaybe :: [a] -> Int -> Maybe a
expectedIndexMaybe xs n
  | n < 0 = Nothing
  | otherwise =
      case Data.List.drop n xs of
        [] -> Nothing
        x : _ -> Just x

expectedCompareLength :: [a] -> Int -> Ordering
expectedCompareLength xs n = compare (Data.List.length xs) n

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

prop_basic_additions_correct :: Property
prop_basic_additions_correct =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 0 50) (Gen.int (Range.constant (-1000) 1000))
    x <- forAll $ Gen.int (Range.constant (-1000) 1000)
    n <- forAll $ Gen.int (Range.constant (-10) 60)
    (Consy.singleton x :: [Int]) === [x]
    Consy.unsnoc xs === expectedUnsnoc xs
    Consy.compareLength xs n === expectedCompareLength xs n

prop_safe_index_correct :: Property
prop_safe_index_correct =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 0 50) (Gen.int (Range.constant (-1000) 1000))
    n <- forAll $ Gen.int (Range.constant (-10) 60)
    (Consy.!?) xs n === expectedIndexMaybe xs n

prop_nub_ord_correct :: Property
prop_nub_ord_correct =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 0 50) (Gen.int (Range.constant (-1000) 1000))
    Consy.nubOrd xs === Data.List.nub xs
    Consy.nubOrdBy compare xs === Data.List.nub xs

prop_set_operations_correct :: Property
prop_set_operations_correct =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-1000) 1000)
    xs <- forAll intList
    ys <- forAll intList
    Consy.nub xs === Data.List.nub xs
    Consy.delete x xs === Data.List.delete x xs
    (Consy.\\) xs ys === (Data.List.\\) xs ys
    Consy.union xs ys === Data.List.union xs ys
    Consy.intersect xs ys === Data.List.intersect xs ys

prop_by_operations_correct :: Property
prop_by_operations_correct =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-1000) 1000)
    xs <- forAll intList
    ys <- forAll intList
    zs <- forAll intList1
    Consy.nubBy sameParity xs === Data.List.nubBy sameParity xs
    Consy.deleteBy sameParity x xs === Data.List.deleteBy sameParity x xs
    Consy.deleteFirstsBy sameParity xs ys === Data.List.deleteFirstsBy sameParity xs ys
    Consy.unionBy sameParity xs ys === Data.List.unionBy sameParity xs ys
    Consy.intersectBy sameParity xs ys === Data.List.intersectBy sameParity xs ys
    Consy.sortBy compare xs === Data.List.sortBy compare xs
    Consy.insertBy compare x xs === Data.List.insertBy compare x xs
    Consy.maximumBy compare zs === Data.List.maximumBy compare zs
    Consy.minimumBy compare zs === Data.List.minimumBy compare zs
  where
    sameParity a b = even a == even b

prop_ordered_lists_correct :: Property
prop_ordered_lists_correct =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-1000) 1000)
    xs <- forAll intList
    let txt = Text.pack (Data.List.map toChar xs)
    Consy.sort xs === Data.List.sort xs
    Consy.sortOn abs xs === Data.List.sortOn abs xs
    Consy.insert x xs === Data.List.insert x xs
    Consy.sort txt === Text.pack (Data.List.sort (Text.unpack txt))

prop_generic_operations_correct :: Property
prop_generic_operations_correct =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-1000) 1000)
    xs <- forAll intList1
    n <- forAll $ Gen.integral (Range.constant (-10 :: Integer) 60)
    ix <- forAll $ Gen.integral (Range.constant 0 (toInteger (Data.List.length xs - 1)))
    (Consy.genericLength xs :: Integer) === Data.List.genericLength xs
    Consy.genericTake n xs === Data.List.genericTake n xs
    Consy.genericDrop n xs === Data.List.genericDrop n xs
    Consy.genericSplitAt n xs === Data.List.genericSplitAt n xs
    Consy.genericIndex xs ix === Data.List.genericIndex xs ix
    Consy.genericReplicate n x === Data.List.genericReplicate n x
  where
    toInteger = fromIntegral

intList :: Gen [Int]
intList =
  Gen.list (Range.constant 0 50) (Gen.int (Range.constant (-1000) 1000))

intList1 :: Gen [Int]
intList1 =
  Gen.list (Range.constant 1 50) (Gen.int (Range.constant (-1000) 1000))

toChar :: Int -> Char
toChar n = toEnum (97 + abs n `mod` 26)

main :: IO ()
main =
  defaultMain $
    testGroup "consy"
      [ InspectionTests.inspectionTests
      , testGroup "properties"
        [ testProperty "transpose" prop_transpose_correct
        , testProperty "permutations" prop_permutations_correct
        , testProperty "inits" prop_inits_correct
        , testProperty "basic additions" prop_basic_additions_correct
        , testProperty "safe index" prop_safe_index_correct
        , testProperty "nubOrd" prop_nub_ord_correct
        , testProperty "set operations" prop_set_operations_correct
        , testProperty "by operations" prop_by_operations_correct
        , testProperty "ordered lists" prop_ordered_lists_correct
        , testProperty "generic operations" prop_generic_operations_correct
        ]
      ]
