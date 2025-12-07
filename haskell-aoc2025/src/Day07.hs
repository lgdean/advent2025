module Day07
    (
      doPart1,
--      doPart2
    ) where

import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      startX = fromJust $ elemIndex 'S' $ head allLines
      results = scanl whatHappens (0, [startX]) (tail allLines)
  in sum (map fst results)

-- given current beam x positions and a row, returns how many split, and next x-positions
whatHappens :: (Int, [Int]) -> [Char] -> (Int, [Int])
whatHappens (_, beamX) rowContent =
  let results = map (flip oneResult rowContent)  beamX
      nSplits = length $ filter ((>1) . length) results
  in (nSplits, nub $ concat results)

oneResult :: Int -> [Char] -> [Int]
oneResult oneX rowContent =
  case rowContent !! oneX of
    '^' -> [oneX-1, oneX+1]
    '.' -> [oneX]
    _ -> error ("unexpected content")
