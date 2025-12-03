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
      joltages = map largestJoltage batteryBanks
  in sum joltages

largestJoltage :: [Int] -> Int
largestJoltage batteries =
  let largestFirstDigit = maximum $ tail $ reverse batteries
      firstOfThatOne =fromJust $ elemIndex largestFirstDigit batteries
      largestSecondDigit = maximum $ drop (firstOfThatOne+1) batteries
  in 10 * largestFirstDigit + largestSecondDigit
