module Day02
    (
      doPart1,
      doPart2
    ) where

import Lib (digits, divisibleBy, strip)

import Data.List.Split (splitOn)
import qualified Data.Set as Set

import Debug.Trace (trace)

doPart1 :: [Char] -> Integer
doPart1 input =
  let ranges = map parseRange $ splitOn [','] input
      results = map part1UsingPart2Code ranges :: [[Integer]]
  in sum $ concat results

doPart2 :: [Char] -> Integer
doPart2 input =
  let ranges = map parseRange $ splitOn [','] input
      results = map part2invalidIdsWithinRange ranges :: [[Integer]]
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

theEvenOneIfAny x y =
  if isOdd x
  then if isOdd y then [] else [y]
  else [x]

part1UsingPart2Code :: (String, String) -> [Integer]
part1UsingPart2Code (lower, upper)
  | (read lower :: Int) > read upper = []
  | length upper > length lower + 1 = error "please write code for this case"
  | otherwise =
      let lowerInt = read lower :: Integer
          upperInt = read upper :: Integer
          isInRange n = lowerInt <= n && n <= upperInt
          partLengths = map (flip div 2) (theEvenOneIfAny (length lower) (length upper))
          lengthsToTry = filter (\n -> length lower `divisibleBy` n || length upper `divisibleBy` n) partLengths
          allResults = concatMap (flip generateAssembled (head partLengths * 2, head partLengths * 2)) lengthsToTry
          uniqueResults = Set.filter isInRange $ Set.fromList $ map read allResults
      in Set.toList uniqueResults

part2invalidIdsWithinRange :: (String, String) -> [Integer]
part2invalidIdsWithinRange (lower, upper)
  | (read lower :: Int) > read upper = []
  | length upper > length lower + 1 = error "please write code for this case"
  | otherwise =
      let lowerInt = read lower :: Integer
          upperInt = read upper :: Integer
          isInRange n = lowerInt <= n && n <= upperInt
          partLengths = [1 .. length upper `div` 2]
          lengthsToTry = filter (\n -> length lower `divisibleBy` n || length upper `divisibleBy` n) partLengths
          allResults = concatMap (flip generateAssembled (length lower, length upper)) lengthsToTry
          uniqueResults = Set.filter isInRange $ Set.fromList $ map read allResults
      in Set.toList uniqueResults

generateAssembled :: Int -> (Int, Int) -> [String]
generateAssembled 0 _ = error "do not call with zero"
generateAssembled n (l1, l2) =
  let parts = generateAll '1' '9' n
      assembled1 = if l1 `divisibleBy` n && l1 > n then map (\s -> concat $ take (l1 `div` n) $ repeat s) parts else []
      assembled2 = if l2 `divisibleBy` n then map (\s -> concat $ take (l2 `div` n) $ repeat s) parts else []
  in assembled1 ++ assembled2

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
