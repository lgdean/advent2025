{-# LANGUAGE OverloadedStrings #-}
module Day02Spec (spec) where

import Test.Hspec

import Day02

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day02-example"
      doPart1 input `shouldBe` 1227775554

    it "can solve Part 1" $ do
      input <- readFile "inputs/day02"
      doPart1 input `shouldBe` 24747430309

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day02-example"
      doPart2 input `shouldBe` 4174379265

    it "can solve Part 2" $ do
      input <- readFile "inputs/day02"
      -- doPart2 input `shouldBe` 30962646867 -- too high
      doPart2 input `shouldBe` 30962646823 -- needed to handle 2-17 range properly
