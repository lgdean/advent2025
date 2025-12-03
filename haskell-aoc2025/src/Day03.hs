module Day03
    (
      doPart1,
--      doPart2
    ) where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let batteryBanks = map (map digitToInt) $ lines input
      joltages = map (digits2Int . largestJoltageBatteries 2) batteryBanks
  in sum joltages

largestJoltageBatteries :: Int -> [Int] -> [Int]
largestJoltageBatteries 0 _ = []
largestJoltageBatteries _ [] = error "sorry, no batteries in empty list"
largestJoltageBatteries 1 xs = [maximum xs]
largestJoltageBatteries n batteries =
  let largestFirstBattery = maximum $ drop (n-1) $ reverse batteries
      firstOfThatOne = fromJust $ elemIndex largestFirstBattery batteries
      remainingBank = drop (firstOfThatOne+1) batteries
  in largestFirstBattery : largestJoltageBatteries (n-1) remainingBank

digits2Int :: [Int] -> Int
digits2Int ds = foldl (\acc n -> acc*10+n) 0 ds
