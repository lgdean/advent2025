module Day11
    (
      doPart1,
      doPart2
    ) where

import Lib (strip)

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let deviceMap = Map.fromList $ map parseLine $ lines input
      paths = pathsFrom deviceMap Set.empty Map.empty "you"
  in length paths

-- TODO returned Map is incomplete, which may be fine, or may be hack leading to bugs
--pathsFrom :: Map String [String] -> Set String -> Map String [[String]] -> String -> (Map String [[String]], [[String]])
pathsFrom :: Map String [String] -> Set String -> Map String [[String]] -> String -> [[String]]
pathsFrom _ _ _ "out" = [["out"]]
pathsFrom outputMap seenSoFar knownPathsOut src
  | src `Set.member` seenSoFar = [] -- cycle detected
  | src `Map.member` knownPathsOut = knownPathsOut Map.! src
  | otherwise =
     let nextSteps = Map.findWithDefault [] src outputMap
         nextSeenSoFar = Set.insert src seenSoFar
         pathInfoFrom currMap dev = pathsFrom outputMap nextSeenSoFar currMap dev
         pathAndMapFrom currMap dev = Map.insert dev (pathInfoFrom currMap dev) currMap
         newPathMap = foldl' pathAndMapFrom knownPathsOut nextSteps :: Map String [[String]]
--     in map (src:) $ concatMap (pathInfoFrom knownPathsOut) nextSteps
     in map (src:) $ concat $ mapMaybe (\d -> Map.lookup d newPathMap) nextSteps

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
      paths = pathsFrom deviceMap Set.empty Map.empty "svr"
      relevantPaths = filter (\p -> "dac" `elem` p && "fft" `elem` p) paths
  in length relevantPaths
