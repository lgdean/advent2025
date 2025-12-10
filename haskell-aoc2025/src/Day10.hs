module Day10
    (
      doPart1,
--      doPart2
    ) where

import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

type Location = (Int, Int)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
  in length allLines

parse2dCoord :: String -> (Int, Int)
parse2dCoord cube =
  let parts = splitOn "," cube
  in case parts of
    [x,y] -> (read x, read y)
    _       -> error ("cannot parse coordinates: " ++ cube)
