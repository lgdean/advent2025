{-# LANGUAGE OverloadedStrings #-}
module Day04Spec (spec) where

import Test.Hspec

import Day04

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day04-example"
      doPart1 input `shouldBe` 13

    it "can solve Part 1" $ do
      input <- readFile "inputs/day04"
      doPart1 input `shouldBe` 1320

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day04-example"
      doPart2 input `shouldBe` 43

    it "can solve Part 2" $ do
      input <- readFile "inputs/day04"
      doPart2 input `shouldBe` 8354
