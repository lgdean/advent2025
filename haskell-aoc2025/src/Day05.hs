module Day05
    (
      doPart1,
--      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

import Debug.Trace (trace)

doPart1 :: [Char] -> [Char] -> Int
doPart1 ranges input =
  let freshRanges = map parseRange $ lines ranges
      isFresh n = not $ null $ filter (\r -> inRange r n) freshRanges
      ingredients = map read $ lines input
  in length $ filter isFresh ingredients


-- Data.Range exists, and perhaps I will want to use it, hm

inRange :: (Int, Int) -> Int -> Bool
inRange (lower, upper) x =
  if (upper < lower)
  then trace ("oh!!") lower <= x && x <= upper
  else lower <= x && x <= upper


parseRange :: String -> (Int, Int)
parseRange line =
  case splitOn ['-'] line of
    [x, y] -> (read x, read y)
    otherwise -> error ("cannot parse range: " ++ line)
