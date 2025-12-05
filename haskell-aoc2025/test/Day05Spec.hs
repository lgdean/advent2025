{-# LANGUAGE OverloadedStrings #-}
module Day05Spec (spec) where

import Test.Hspec

import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      ranges <- readFile "inputs/day05-ranges-example"
      input <- readFile "inputs/day05-available-example"
      doPart1 ranges input `shouldBe` 3

    it "can solve Part 1" $ do
      ranges <- readFile "inputs/day05-ranges"
      input <- readFile "inputs/day05-available"
      doPart1 ranges input `shouldBe` 567

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day05-example"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day05"
--      doPart2 input `shouldBe` 0
