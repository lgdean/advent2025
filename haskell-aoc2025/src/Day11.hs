module Day11
    (
      doPart1,
      doPart2
    ) where

import Lib (strip)

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let deviceMap = Map.fromList $ map parseLine $ lines input
      paths = pathsFrom deviceMap Set.empty "you"
  in length paths

pathsFrom :: Map String [String] -> Set String -> String -> [[String]]
pathsFrom _ _ "out" = [["out"]]
pathsFrom outputMap seenSoFar src
  | src `Set.member` seenSoFar = [] -- cycle detected
  | otherwise = map (src:) $ concatMap (pathsFrom outputMap (Set.insert src seenSoFar)) $ Map.findWithDefault [] src outputMap

parseLine :: String -> (String, [String])
parseLine line =
  let parts = splitOn ":" line
  in case parts of
    [] -> error "empty line?"
    [_] -> error "just one item in line"
    (fromDevice : rest) -> (fromDevice, splitOn " " $ strip $ head rest)

doPart2 :: [Char] -> Int
doPart2 input =
  let deviceMap = Map.fromList $ map parseLine $ lines input
      paths = pathsFrom deviceMap Set.empty "svr"
      relevantPaths = filter (\p -> "dac" `elem` p && "fft" `elem` p) paths
  in length relevantPaths
