module Day08
    (
      doPart1,
--      doPart2
    ) where

import Data.List (partition, sort)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

type Location = (Int, Int, Int)

doPart1 :: Int -> [Char] -> Int
doPart1 howMany input =
  let allLines = lines input
      boxLocations = map parse3dCoord allLines
      distances = buildDistances $ sort boxLocations
      pairsToConnect = sort $ map snd $ take howMany $ sort distances
      -- these are just the circuits of size greater than 1
      circuits = buildUpCircuits [] pairsToConnect
      sortedCircuits = reverse $ sort $ map Set.size circuits
  in trace (show $ take 3 sortedCircuits) $ product $ take 3 sortedCircuits

buildUpCircuits :: [Set Location] -> [(Location, Location)] -> [Set Location]
buildUpCircuits circuits [] = circuits
buildUpCircuits circuits ((a,b) : rest) =
  let nextState = circuitBuildingStep circuits (a,b)
  in buildUpCircuits nextState rest

circuitBuildingStep :: [Set Location] -> (Location, Location) -> [Set Location]
circuitBuildingStep circuits (a,b) =
  let (aCircuitList, noA) = partition (Set.member a) circuits
      (bCircuitList, noB) = partition (Set.member b) circuits
  in case (aCircuitList, bCircuitList) of
    ([], []) -> (Set.fromList [a,b] : circuits)
    ([], [bCircuit]) -> (Set.insert a bCircuit : noB)
    ([aCircuit], []) -> (Set.insert b aCircuit : noA)
    ([aCircuit], [bCircuit]) -> ((Set.union aCircuit bCircuit) : (filter (/= aCircuit) noB))
    _ -> error "programmer surprise"

-- takes in a sorted list of locations
-- maybe can avoid calculating them all, but for now, no need to worry
buildDistances :: [Location] -> [(Int, (Location, Location))]
buildDistances [] = []
buildDistances [_loc] = []
buildDistances (loc : rest) =
  let distances = map (\other -> (distanceSquared loc other, (loc, other))) rest
  in distances ++ buildDistances rest

distanceSquared :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distanceSquared (x, y, z) (a, b, c) = (x-a)^(2::Int) + (y-b)^(2::Int) + (z-c)^(2::Int)

parse3dCoord :: String -> (Int, Int, Int)
parse3dCoord cube =
  let parts = splitOn "," cube
  in case parts of
    [x,y,z] -> (read x, read y, read z)
    _       -> error ("cannot parse coordinates: " ++ cube)
