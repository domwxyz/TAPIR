{-# LANGUAGE OverloadedStrings #-}

module Tapir.Core.SelectionSpec (spec) where

import Prelude hiding (length, filter)
import Test.Hspec
import Tapir.Core.Selection

spec :: Spec
spec = describe "Tapir.Core.Selection" $ do

  describe "fromList" $ do
    it "returns Left SelectionEmpty for empty list" $ do
      fromList ([] :: [Int]) `shouldBe` Left SelectionEmpty

    it "returns Right Selection for non-empty list" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          selected sel `shouldBe` 1
          selectedIndex sel `shouldBe` 0

    it "selects first element by default" $ do
      case fromList ["a", "b", "c"] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> selected sel `shouldBe` "a"

  describe "singleton" $ do
    it "creates selection with single item" $ do
      let sel = singleton "only"
      selected sel `shouldBe` "only"
      selectedIndex sel `shouldBe` 0
      length sel `shouldBe` 1

  describe "toList" $ do
    it "returns all items" $ do
      case fromList [1,2,3,4,5] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> toList sel `shouldBe` [1,2,3,4,5]

  describe "length" $ do
    it "returns correct length" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> length sel `shouldBe` 3

    it "returns 1 for singleton" $ do
      length (singleton "x") `shouldBe` 1

  describe "moveNext" $ do
    it "moves to next item" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveNext sel
          selected sel' `shouldBe` 2
          selectedIndex sel' `shouldBe` 1

    it "stays at end when already at last" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveNext $ moveNext $ moveNext sel
          selected sel' `shouldBe` 3
          selectedIndex sel' `shouldBe` 2

  describe "movePrev" $ do
    it "moves to previous item" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = movePrev $ moveNext $ moveNext sel
          selected sel' `shouldBe` 2
          selectedIndex sel' `shouldBe` 1

    it "stays at start when already at first" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = movePrev sel
          selected sel' `shouldBe` 1
          selectedIndex sel' `shouldBe` 0

  describe "moveToIndex" $ do
    it "moves to valid index" $ do
      case fromList [1,2,3,4,5] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToIndex 3 sel
          selected sel' `shouldBe` 4
          selectedIndex sel' `shouldBe` 3

    it "clamps negative index to 0" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToIndex (-5) sel
          selected sel' `shouldBe` 1
          selectedIndex sel' `shouldBe` 0

    it "clamps too-large index to last" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToIndex 100 sel
          selected sel' `shouldBe` 3
          selectedIndex sel' `shouldBe` 2

  describe "moveToStart" $ do
    it "moves to first item" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToStart $ moveToIndex 2 sel
          selected sel' `shouldBe` 1
          selectedIndex sel' `shouldBe` 0

  describe "moveToEnd" $ do
    it "moves to last item" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToEnd sel
          selected sel' `shouldBe` 3
          selectedIndex sel' `shouldBe` 2

  describe "updateItems" $ do
    it "updates items and preserves valid index" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToIndex 1 sel
          case updateItems [10,20,30,40] sel' of
            Left _ -> expectationFailure "expected Right"
            Right sel'' -> do
              selected sel'' `shouldBe` 20
              selectedIndex sel'' `shouldBe` 1

    it "clamps index when new list is shorter" $ do
      case fromList [1,2,3,4,5] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = moveToIndex 4 sel  -- index 4, last item
          case updateItems [10,20] sel' of
            Left _ -> expectationFailure "expected Right"
            Right sel'' -> do
              selected sel'' `shouldBe` 20
              selectedIndex sel'' `shouldBe` 1

    it "returns Left SelectionEmpty for empty list" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          updateItems [] sel `shouldBe` Left SelectionEmpty

  describe "filter" $ do
    it "filters items and preserves selection when possible" $ do
      case fromList [1,2,3,4,5] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          case filter even sel of
            Left _ -> expectationFailure "expected Right"
            Right sel' -> do
              toList sel' `shouldBe` [2,4]

    it "returns Left SelectionEmpty when all items filtered" $ do
      case fromList [1,3,5] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          filter even sel `shouldBe` Left SelectionEmpty

  describe "mapItems" $ do
    it "transforms all items" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = mapItems (*10) sel
          toList sel' `shouldBe` [10, 20, 30]
          selectedIndex sel' `shouldBe` 0

    it "preserves selection index" $ do
      case fromList [1,2,3] of
        Left _ -> expectationFailure "expected Right"
        Right sel -> do
          let sel' = mapItems show $ moveToIndex 2 sel
          selected sel' `shouldBe` "3"
          selectedIndex sel' `shouldBe` 2
