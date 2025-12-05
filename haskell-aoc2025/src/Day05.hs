module Day05
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Range ((+=+), joinRanges, Range(SpanRange))
import qualified Data.Range as Range

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

doPart2 :: [Char] -> Int
doPart2 ranges =
  let freshRanges = map parseRange $ lines ranges
      (lowers, uppers) = unzip freshRanges
      (minFresh, maxFresh) = (minimum lowers, maximum uppers)
      fancyRanges = map (uncurry (+=+)) freshRanges
      betterRanges = joinRanges fancyRanges
      rangeSizes = map rangeSize betterRanges
  in sum rangeSizes

rangeSize :: Range Int -> Int
rangeSize (SpanRange (Range.Bound x _) (Range.Bound y _)) = 1+y-x
rangeSize _ = error "sorry, cannot handle"

parseRange :: String -> (Int, Int)
parseRange line =
  case splitOn ['-'] line of
    [x, y] -> (read x, read y)
    _ -> error ("cannot parse range: " ++ line)
