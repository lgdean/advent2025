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

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day02-example"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day02"
--      doPart2 input `shouldBe` 0
