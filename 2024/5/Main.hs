module Main where
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ show (part2 input)

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input =
  let [rules, pageSets] = splitOn "\n\n" input
  in (parsePairs rules, parseLists pageSets)

parsePairs :: String -> [(Int, Int)]
parsePairs = map (toPair . splitOn "|") . lines
  where
    toPair [a, b] = (read a, read b)

parseLists :: String -> [[Int]]
parseLists = map (map read . splitOn ",") . lines

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

validatePageSets :: [(Int, Int)] -> [[Int]] -> [Int]
validatePageSets rules = foldl processPageSet []
  where
    processPageSet :: [Int] -> [Int] -> [Int]
    processPageSet acc x
      | isValidPageSet rules x = acc ++ [middle x]
      | otherwise              = acc

    isValidPageSet :: [(Int, Int)] -> [Int] -> Bool
    isValidPageSet rules []       = True
    isValidPageSet rules [_]      = True
    isValidPageSet rules (x:y:ys) = isValidPage rules x y && isValidPageSet rules (y:ys)

    isValidPage :: [(Int, Int)] -> Int -> Int -> Bool
    isValidPage [] _ _ = False
    isValidPage ((a, b):rs) x y
      | x == a && y == b = True
      | otherwise        = isValidPage rs x y

part1 :: String -> Int
part1 = sum
      . uncurry validatePageSets
      . parseInput
