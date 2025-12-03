module Day01
    (
      doPart1,
      doPart2
    ) where

doPart1 :: [Char] -> Int
doPart1 input =
  let rotations = map parseLine $ lines input
      initPosition = 50
      positions = applyRotations initPosition (map fst rotations)
  in length $ filter (==0) (map fst positions)

applyRotations :: Int -> [Int -> Int] -> [(Int, Int)]
applyRotations pos [] = [(pos,0)]
applyRotations pos (r:rs) =
  let (zeroVisits, nextPos) = (r pos) `divMod` 100
      howMuchMoved = ((r pos) - pos) -- yes, silly to recalculate direction here
      actualZeroVisits =
        if nextPos == 0 && (r pos) <= 0
          then (abs zeroVisits) + 1
          else if pos == 0 && howMuchMoved < 0
               then (abs zeroVisits) - 1
               else abs zeroVisits
  in (pos, actualZeroVisits) : applyRotations nextPos rs

parseLine :: String -> (Int -> Int, Int)
parseLine ('L' : rest) = (((flip (-)) (read rest)), read rest)
parseLine ('R' : rest) = (((flip (+)) (read rest)), read rest)
parseLine anythingElse = error ("cannot parse line: " ++ anythingElse)

doPart2 :: [Char] -> Int
doPart2 input =
  let rotations = map parseLine $ lines input
      initPosition = 50
      positions = applyRotations initPosition (map fst rotations)
  in (sum $ map snd positions)
