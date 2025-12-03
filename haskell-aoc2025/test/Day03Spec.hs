{-# LANGUAGE OverloadedStrings #-}
module Day03Spec (spec) where

import Test.Hspec

import Day03

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day03-example"
      doPart1 input `shouldBe` 357

    it "can solve Part 1" $ do
      input <- readFile "inputs/day03"
      doPart1 input `shouldBe` 17087

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day03-example"
      doPart2 input `shouldBe` 3121910778619

    it "can solve Part 2" $ do
      input <- readFile "inputs/day03"
      doPart2 input `shouldBe` 169019504359949
