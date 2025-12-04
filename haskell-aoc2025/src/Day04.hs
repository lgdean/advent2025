module Day04
    (
      doPart1,
      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let grid = parseGrid input
      papers = Map.filter (== '@') grid
      accessiblePapers = filter (`fewerThanFourPaperNeighbors` grid) $ Map.keys papers
  in length accessiblePapers

doPart2 :: [Char] -> Int
doPart2 input =
  let grid = parseGrid input
      papers = Map.filter (== '@') grid
      accessiblePapers = filter (`fewerThanFourPaperNeighbors` grid) $ Map.keys papers
      resultGrid = keepRemovingPaper grid
  in Map.size papers - Map.size (Map.filter (== '@') resultGrid)

keepRemovingPaper :: Map (Int, Int) Char -> Map (Int, Int) Char
keepRemovingPaper grid =
  let papers = Map.filter (== '@') grid
      accessiblePapers = filter (`fewerThanFourPaperNeighbors` grid) $ Map.keys papers
      nextGrid = Map.filterWithKey (\k _ -> k `notElem` accessiblePapers) grid
  in if null accessiblePapers
     then trace "returning" $ grid
     else trace ("keep going with grid of size " ++ show (Map.size nextGrid)) $ keepRemovingPaper nextGrid

--keepRemovingPaper :: Map (Int, Int) Char -> Map (Int, Int) Char
--keepRemovingPaper grid
--  | null accessiblePapers = grid
--  | otherwise = keepRemovingPaper $ Map.filterWithKey (\k _ -> k `notElem` accessiblePapers) grid
--  where papers = Map.filter (== '@') grid
--        accessiblePapers = filter (`fewerThanFourPaperNeighbors` grid) $ Map.keys papers

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
