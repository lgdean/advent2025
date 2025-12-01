module Day01
    (
      doPart1,
      doPart2
    ) where

import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let rotations = map parseLine $ lines input
      initPosition = 50
      positions = applyRotations initPosition (map fst rotations)
  in trace (show positions) $ length $ filter (==0) positions

applyRotations :: Int -> [Int -> Int] -> [Int]
applyRotations pos [] = [pos]
applyRotations pos (r:rs) =
  pos : applyRotations ((r pos) `mod` 100) rs

parseLine :: String -> (Int -> Int, Int)
parseLine ('L' : rest) = (((flip (-)) (read rest)), read rest)
parseLine ('R' : rest) = (((flip (+)) (read rest)), read rest)
parseLine anythingElse = error ("cannot parse line: " ++ anythingElse)

doPart2 :: [Char] -> Int
doPart2 input = 0
