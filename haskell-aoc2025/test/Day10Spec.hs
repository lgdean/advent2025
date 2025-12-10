{-# LANGUAGE OverloadedStrings #-}
module Day10Spec (spec) where

import Test.Hspec

import Day10

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day10-example"
      doPart1 input `shouldBe` 7

--    it "can solve Part 1" $ do
--      input <- readFile "inputs/day10"
--      doPart1 input `shouldBe` 0

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day10-example"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day10"
--      doPart2 input `shouldBe` 0
