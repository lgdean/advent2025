{-# LANGUAGE OverloadedStrings #-}
module Day07Spec (spec) where

import Test.Hspec

import Day07

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day07-example"
      doPart1 input `shouldBe` 21

    it "can solve Part 1" $ do
      input <- readFile "inputs/day07"
      doPart1 input `shouldBe` 1594

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day07-example"
      doPart2 input `shouldBe` 40

    it "can solve Part 2" $ do
      input <- readFile "inputs/day07"
      doPart2 input `shouldBe` 15650261281478
