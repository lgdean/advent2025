module Day06
    (
      doPart1,
      doPart2
    ) where

import Data.List (transpose)
import Data.List.Split (splitWhen)
import Data.String.Strip (strip)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      operators = words $ head $ reverse allLines
      numbers = map (map read . words) $ tail $ reverse allLines :: [[Int]]
      numberCols = transpose numbers
      results = zipWith applyOperation operators numberCols
  in sum results

applyOperation :: String -> [Int] -> Int
applyOperation "+" nums = sum nums
applyOperation "*" nums = product nums
applyOperation o _ = error ("cannot apply operation " ++ o)

doPart2 :: [Char] -> Int
doPart2 input =
  let allLines = lines input
      operators = words $ head $ reverse allLines
      columns = transpose $ tail $ reverse allLines
      chunks = splitWhen (all (== ' ')) columns
      nums = map (map (strip . reverse)) chunks :: [[String]]
      numbers = map (map read) nums :: [[Int]]
      results = zipWith applyOperation operators numbers
  in sum results
