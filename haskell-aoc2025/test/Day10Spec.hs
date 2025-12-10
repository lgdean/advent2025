{-# LANGUAGE OverloadedStrings #-}
module Day10Spec (spec) where

import Test.Hspec

import Day10

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle first given machine" $ do
      fewestButtonPresses ".##." [[3], [1,3], [2], [2,3], [0,2], [0,1]] `shouldBe` 2

    it "can handle given example" $ do
      input <- readFile "inputs/day10-example"
      doPart1 input `shouldBe` 7

    it "can solve Part 1" $ do
      input <- readFile "inputs/day10"
      doPart1 input `shouldBe` 498

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day10-example"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day10"
--      doPart2 input `shouldBe` 0
