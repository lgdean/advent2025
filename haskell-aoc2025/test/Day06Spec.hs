{-# LANGUAGE OverloadedStrings #-}
module Day06Spec (spec) where

import Test.Hspec

import Day06

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day06-example"
      doPart1 input `shouldBe` 4277556

    it "can solve Part 1" $ do
      input <- readFile "inputs/day06"
      doPart1 input `shouldBe` 6503327062445

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day06-example"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day06"
--      doPart2 input `shouldBe` 0
