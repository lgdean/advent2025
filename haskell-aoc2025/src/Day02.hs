module Day02
    (
      doPart1,
--      doPart2
    ) where

import Lib (digits, strip)

import Data.List.Split (splitOn)

import Debug.Trace (trace)

doPart1 :: [Char] -> Integer
doPart1 input =
  let ranges = map parseRange $ splitOn [','] input
      results = map invalidIdsWithinRange ranges :: [[Integer]]
  in sum $ concat results

-- no idea why I made it so complicated: premature optimization
invalidIdsWithinRange :: (String, String) -> [Integer]
invalidIdsWithinRange (lower, upper)
  | (read lower :: Int) > read upper = []
  | length upper > length lower + 1 = error "please write code for this case"
  | isOdd (length lower) && isOdd (length upper) = []
  | isOdd (length lower) = invalidIdsWithinRange (('1' : take (length upper - 1) (repeat '0')), upper)
  | otherwise =
      let lowerInt = read lower :: Integer
          upperInt = read upper :: Integer
          highUpperChar = if isOdd (length upper) then '9' else head upper
          candidateHalves = generateAll (head lower) highUpperChar (length lower `div` 2)
          candidates = map (\s -> s ++ s) candidateHalves
          isInRange n = lowerInt <= n && n <= upperInt
          in filter isInRange $ map read candidates

generateAll :: Char -> Char -> Int -> [String]
generateAll _ _ 0 = [""]
generateAll low high 1 = map (\c -> [c]) [low .. high]
generateAll low high len =
  [ (x : rest) | x <- [low..high], rest <- generateAll ('0'::Char) ('9'::Char) (len-1)]


parseRange :: String -> (String, String)
parseRange input =
  let parts = splitOn ['-'] input
  in case parts of
    one : other : [] -> (one, other)
    _                -> error ("cannot parse range: " ++ input)

isOdd :: Integral a => a -> Bool
isOdd n = n `mod` 2 == 1
