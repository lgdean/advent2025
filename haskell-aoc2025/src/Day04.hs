module Day04
    (
      doPart1,
--      doPart2
    ) where

import Data.Map (Map, findWithDefault)
import qualified Data.Map.Strict as Map

doPart1 :: [Char] -> Int
doPart1 input =
  let grid = parseGrid input
      papers = Map.filter (== '@') grid
      accessiblePapers = filter (`fewerThanFourPaperNeighbors` grid) $ Map.keys papers
  in length accessiblePapers

hasPaper :: (Int, Int) -> Map (Int, Int) Char -> Bool
hasPaper pos grid =
  let content = Map.findWithDefault '.' pos grid
  in content == '@'

fewerThanFourPaperNeighbors :: (Int, Int) -> Map (Int, Int) Char -> Bool
fewerThanFourPaperNeighbors pos grid =
  let neighbors = neighborCoords pos
      howManyPaper = filter id $ map (`hasPaper` grid) neighbors
  in length howManyPaper < 4

-- I just keep copying variants of this code:
parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

-- ok but I do keep copying this one
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (x,y) = (a+x, b+y)

-- also this one, but there are 2 versions of it
neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) =
  [add (x, y) (a, b) |
   a <- [- 1, 0, 1], b <- [- 1, 0, 1], (a, b) /= (0, 0)]
