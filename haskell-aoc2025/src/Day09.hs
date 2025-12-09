module Day09
    (
      doPart1,
      doPart2
    ) where

import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

type Location = (Int, Int)

doPart2 :: [Char] -> Int
doPart2 input =
  let allLines = lines input
      redTileLocations = map parse2dCoord allLines
      redsShifted = tail redTileLocations ++ [head redTileLocations]
      outline = Set.fromList $ concat $ zipWith drawLine redTileLocations redsShifted
      everyCornerY = nub $ sort $ map snd redTileLocations
      everyCornerX = nub $ sort $ map fst redTileLocations
      outlineSubset = Set.filter (\(x,y) -> x `elem` everyCornerX && y `elem` everyCornerY) outline
      redPairs = [(a,b) | a <- redTileLocations, b <- redTileLocations, a < b,
                          fst a < 1000 -- in the given example, not the real data
                            || fst a >= 94862 -- or beyond the long horizontal gap
                            || (snd a <= 48448) == (snd b <= 48448), -- or on same side of it (above/below)
                          allIn outlineSubset everyCornerY (a,b)]
      areas = map (uncurry area) redPairs
  in trace (show redPairs) $ maximum areas

-- I believe this works because I graphed the data and looked at it
allIn :: Set Location -> [Int] -> (Location, Location) -> Bool
allIn outline relevantYs ((a, b), (c, d)) =
  let smallerY = min b d
      biggerY = max b d
      yRange = takeWhile (<= biggerY) $ dropWhile (< smallerY) relevantYs
      smallerX = min a c -- probably unnecessary due to sorting; keeping it for clarity
      biggerX = max a c
      isOK y = (\((x1,_),(x2,_)) -> x1 <= smallerX && biggerX <= x2) $ horizontalLineEnds outline y
  in all isOK yRange

horizontalLineEnds :: Set Location -> Int -> (Location, Location)
horizontalLineEnds outline y =
  let ends = Set.toAscList $ Set.filter ((==y) . snd) outline
  in case ends of
    [a, b] -> (a, b)
    (a : _ : rest) -> (a, head (reverse rest)) -- should be ok per visual inspection
    _      -> error ("did not consider this case: " ++ show ends ++ " for " ++ show y ++ " of " ++ show outline)

drawLine :: Location -> Location -> [Location]
drawLine (a, b) (c, d)
  | a == c = [(a, y) | y <- [min b d .. max b d]]
  | b == d = [(x, b) | x <- [min a c .. max a c]]
  | otherwise = error ("cannot handle line: " ++ show (a,b) ++ ", " ++ show (c,d))

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      redTileLocations = map parse2dCoord allLines
      redPairs = [(a,b) | a <- redTileLocations, b <- redTileLocations, a < b]
      areas = map (uncurry area) redPairs
  in maximum areas

area :: (Int, Int) -> (Int, Int) -> Int
area (x, y) (a, b) = (1+abs(x-a)) * (1+abs(y-b))

parse2dCoord :: String -> (Int, Int)
parse2dCoord cube =
  let parts = splitOn "," cube
  in case parts of
    [x,y] -> (read x, read y)
    _       -> error ("cannot parse coordinates: " ++ cube)
