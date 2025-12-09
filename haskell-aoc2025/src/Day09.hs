module Day09
    (
      doPart1,
--      doPart2
    ) where

import Data.List.Split (splitOn)

type Location = (Int, Int)

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
