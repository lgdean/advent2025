module Day10
    (
      fewestButtonPresses,
      doPart1,
      fewestButtonPresses2,
      doPart2
    ) where

import Data.List.Split (splitOn)

--import Debug.Trace (trace)

fewestButtonPresses :: [Char] -> [[Int]] -> Int
fewestButtonPresses pattern buttons =
  let comboTimes = createCombos (length buttons) [0,1] :: [[Int]]
      combos = map (\c -> zipWith replicate c buttons) comboTimes :: [[[[Int]]]]
      buttonPressResult combo = [if odd (length $ filter (==x) (concat $ concat combo)) then '#' else '.' | x <- [0..length pattern-1]]
      goodCombos = filter ((==pattern) . buttonPressResult) combos
      comboLengths = map (length . concat) goodCombos
  in minimum comboLengths

fewestButtonPresses2 :: [Int] -> [[Int]] -> Int
fewestButtonPresses2 pattern buttons =
  let comboTimes = createCombos (length buttons) [0 .. maximum pattern] :: [[Int]] -- combinatorial explosion! oh no
      combos = map (\c -> zipWith replicate c buttons) comboTimes :: [[[[Int]]]]
      buttonPressResult combo = [(length $ filter (==x) (concat $ concat combo)) | x <- [0..length pattern-1]]
      goodCombos = filter ((==pattern) . buttonPressResult) combos
      comboLengths = map (length . concat) goodCombos
  in minimum comboLengths

-- surely doable with replicateM or the like
createCombos :: Int -> [a] -> [[a]]
createCombos 0 _ = [] -- or [[]] ? but unused at the moment anyway
createCombos 1 xs = [[x] | x <- xs]
createCombos n xs = [x:rest | x <- xs, rest <- createCombos (n-1) xs]

doPart1 :: [Char] -> Int
doPart1 input =
  let machines = map (\(a,b,_) -> (a,b)) $ map parseLine $ lines input
  in sum $ map (uncurry fewestButtonPresses) machines

doPart2 :: [Char] -> Int
doPart2 input =
  let machines = map (\(_,b,c) -> (c,b)) $ map parseLine $ lines input
  in sum $ map (uncurry fewestButtonPresses2) machines

parseLine :: String -> ([Char], [[Int]], [Int])
parseLine line =
  let parts = splitOn " " line
  in case parts of
    [] -> error "empty line?"
    [_] -> error "just one item in line"
    [_, _] -> error "still not enough"
    (pattern : rest) -> (reverse $ tail $ reverse $ tail pattern,
                         map parseButton $ tail $ reverse rest,
                         parseButton $ head $ reverse rest)

parseButton :: String -> [Int]
parseButton input =
  let parts = splitOn "," $ reverse $ tail $ reverse $ tail input
  in map read parts
