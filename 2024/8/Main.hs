module Main where
import Data.List (foldl')
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parseInput :: [String] -> [(Char, (Int, Int))]
parseInput grid =
  [ (freq, (x, y))
  | (y, row) <- zip [0..] grid
  , (x, freq) <- zip [0..] row
  , freq /= '.' ]  -- ignore '.' placeholders

findPairs :: [(Char, (Int, Int))] -> [[((Int, Int), (Int, Int))]]
findPairs antennas =
  let grouped = foldr (\(f, pos) acc -> addToGroup f pos acc) [] antennas
  in map (\(_, positions) ->
         [ (a, b)
         | a <- positions
         , b <- positions
         , a < b  -- pick each pair once
         ])
       grouped
  where
    addToGroup f pos [] = [(f, [pos])]
    addToGroup f pos ((g, ps):rest)
      | f == g    = (g, pos : ps) : rest
      | otherwise = (g, ps) : addToGroup f pos rest

part1 :: String -> Int
part1 contents =
  let grid      = lines contents
      height    = length grid
      width     = length (head grid)
      antennas  = parseInput grid
      antinodes = collectAntinodesPart1 antennas width height
  in Set.size antinodes

collectAntinodesPart1 :: [(Char, (Int, Int))] -> Int -> Int -> Set.Set (Int, Int)
collectAntinodesPart1 antennas width height =
  let pairs      = concat (findPairs antennas)
      -- For each pair, compute the 2 'outside' antinodes (the reflection points):
      candidatePositions = concatMap calculateAntinodes pairs
      inBounds (x, y)    = (x >= 0 && x < width && y >= 0 && y < height)
      validCandidates    = filter inBounds candidatePositions

      -- Also, some antennas themselves can be antinodes if they fulfill the 1:2 ratio
      antennaCoords      = map snd antennas
      validAntennas      = filter (\coord -> any (isValidAntinodePart1 coord) pairs)
                                    antennaCoords

  in Set.fromList (validCandidates ++ validAntennas)

calculateAntinodes :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
calculateAntinodes ((x1, y1), (x2, y2)) =
  let dx = x2 - x1
      dy = y2 - y1
      a1 = (x1 - dx, y1 - dy)
      a2 = (x2 + dx, y2 + dy)
  in [a1, a2]

isValidAntinodePart1 :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
isValidAntinodePart1 (ax, ay) ((x1, y1), (x2, y2)) =
  let dx1 = x1 - ax
      dy1 = y1 - ay
      dx2 = x2 - ax
      dy2 = y2 - ay

      -- Cross product == 0 => collinear
      cross = dx1 * dy2 - dy1 * dx2
      collinear = (cross == 0)

      dist1 = dx1 * dx1 + dy1 * dy1
      dist2 = dx2 * dx2 + dy2 * dy2

      ratio12 = (dist1 == 2 * dist2) || (dist2 == 2 * dist1)
  in collinear && ratio12

part2 :: String -> Int
part2 contents =
  let grid      = lines contents
      height    = length grid
      width     = length (head grid)
      antennas  = parseInput grid
      antinodes = collectAntinodesPart2 antennas width height
  in Set.size antinodes

collectAntinodesPart2 :: [(Char, (Int, Int))] -> Int -> Int -> Set.Set (Int, Int)
collectAntinodesPart2 antennas width height =
  let pairs = concat (findPairs antennas)
      -- For each pair, find all integer grid points collinear with that pair (including the pair itself)
      allPoints = concatMap (\(p1, p2) -> allCollinearPoints p1 p2 width height) pairs
  in Set.fromList allPoints

allCollinearPoints :: (Int, Int) -> (Int, Int) -> Int -> Int -> [(Int, Int)]
allCollinearPoints (x1, y1) (x2, y2) width height =
  let dx = x2 - x1
      dy = y2 - y1
      g  = gcd dx dy
      stepx = dx `div` g
      stepy = dy `div` g

      -- Walk forward from (x1,y1)
      forward (cx, cy) acc =
        if cx < 0 || cx >= width || cy < 0 || cy >= height
          then acc
          else forward (cx + stepx, cy + stepy) ((cx, cy):acc)

      -- Walk backward
      backward (cx, cy) acc =
        if cx < 0 || cx >= width || cy < 0 || cy >= height
          then acc
          else backward (cx - stepx, cy - stepy) ((cx, cy):acc)

      forwards  = forward  (x1, y1) []
      backwards = backward (x1, y1) []
      -- Each includes (x1,y1), so there's a duplication of that point.
      -- That duplication is not a problem once we union them into a Set.
  in forwards ++ backwards
