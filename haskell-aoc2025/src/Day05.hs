module Day05
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Range ((+=+), inRange, joinRanges, Range(SpanRange))
import qualified Data.Range as Range

doPart1 :: [Char] -> [Char] -> Int
doPart1 ranges input =
  let freshRanges = map parseRange $ lines ranges
      isFresh n = not $ null $ filter (\r -> inRange r n) freshRanges
      ingredients = map read $ lines input
  in length $ filter isFresh ingredients

doPart2 :: [Char] -> Int
doPart2 ranges =
  let freshRanges = map parseRange $ lines ranges
      betterRanges = joinRanges freshRanges
      rangeSizes = map rangeSize betterRanges
  in sum rangeSizes

rangeSize :: Range Int -> Int
rangeSize (SpanRange (Range.Bound x _) (Range.Bound y _)) = 1+y-x
rangeSize _ = error "sorry, cannot handle"

parseRange :: String -> Range Int
parseRange line =
  case splitOn ['-'] line of
    [x, y] -> (read x) +=+ (read y)
    _ -> error ("cannot parse range: " ++ line)
