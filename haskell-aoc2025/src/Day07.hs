module Day07
    (
      doPart1,
      doPart2
    ) where

import Data.List (elemIndex, nub)
import qualified Data.Map.Strict as Map
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

doPart2 :: [Char] -> Int
doPart2 input =
  let allLines = lines input
      firstState = zip [0..] $ map (\c -> if c=='S' then 1 else 0) $ head allLines
      results = scanl whatHappens2 (0, firstState) (tail allLines)
      finalResult = snd $ head $ reverse results
  in sum $ map snd finalResult

-- given current beam x positions and a row with counts, returns some number, and next x-positions
-- I'm sure that was a very clear x-position :joy:
whatHappens2 :: (Int, [(Int, Int)]) -> [Char] -> (Int, [(Int, Int)])
whatHappens2 (_, beamX) rowContent =
  let nextRowComponents = zipWith expand beamX rowContent
      nextRow = contract nextRowComponents
  in (0, nextRow)

expand :: (Int, Int) -> Char -> [(Int, Int)]
expand (xpos, count) '^' = [(xpos-1, count), (xpos, 0), (xpos+1, count)]
expand (xpos, count)  _  = [(xpos, count)]

contract :: [[(Int, Int)]] -> [(Int, Int)]
contract lists =
  let theMaps = map Map.fromList lists
  in Map.toList $ Map.unionsWith (+) theMaps
