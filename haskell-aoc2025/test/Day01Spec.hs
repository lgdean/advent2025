{-# LANGUAGE OverloadedStrings #-}
module Day01Spec (spec) where

import Test.Hspec

import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example"
      doPart1 input `shouldBe` 3

    it "can solve Part 1" $ do
      input <- readFile "inputs/day01"
      doPart1 input `shouldBe` 1172

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example"
      doPart2 input `shouldBe`  6

    it "can handle my example" $ do
      input <- readFile "inputs/day01-my-example"
      doPart2 input `shouldBe` 3

    it "can handle my example" $ do
      input <- readFile "inputs/day01-my-example-2"
      doPart2 input `shouldBe` 4

    it "can handle my example" $ do
      input <- readFile "inputs/day01-my-example-3"
      doPart2 input `shouldBe` 5

    it "can handle my example 4" $ do
      input <- readFile "inputs/day01-my-example-4"
      doPart2 input `shouldBe` 3

    it "can handle my example 5" $ do
      input <- readFile "inputs/day01-my-example-5"
      doPart2 input `shouldBe` 4

    it "can handle my example 6 turning left at end" $ do
      input <- readFile "inputs/day01-my-example-6"
      doPart2 input `shouldBe` 5

    it "can handle my example 6 turning right at end" $ do
      input <- readFile "inputs/day01-my-example-6-other-dir"
      doPart2 input `shouldBe` 5

    it "can solve Part 2" $ do
      input <- readFile "inputs/day01"
      -- doPart2 input `shouldBe` 6967 -- not correct
      -- doPart2 input `shouldBe` 7409 -- too high
      -- doPart2 input `shouldBe` 6688 -- too low
      -- doPart2 input `shouldBe` 6822 -- not correct
      doPart2 input `shouldBe` 6932
