{-# LANGUAGE OverloadedStrings #-}
module Day08Spec (spec) where

import Test.Hspec

import Day08

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day08-example"
      doPart1 10 input `shouldBe` 40

    it "can solve Part 1" $ do
      input <- readFile "inputs/day08"
      doPart1 1000 input `shouldBe` 50760

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day08-example"
--      doPart2 input `shouldBe` 25272
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day08"
--      doPart2 input `shouldBe` 0
