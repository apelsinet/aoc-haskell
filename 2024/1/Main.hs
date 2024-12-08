module Main where
import Data.Char (isDigit, digitToInt)
import Data.List (sort)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

  putStrLn $ "Part 2: " ++ show (part2 input)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

splitAndSortLists :: [[Int]] -> ([Int], [Int])
splitAndSortLists xs = (sort $ map (!! 0) xs, sort $ map (!! 1) xs)

splitLists :: [[Int]] -> ([Int], [Int])
splitLists xs = (map (!! 0) xs, map (!! 1) xs)

differenceAtSameIndex :: ([Int], [Int]) -> [Int]
differenceAtSameIndex (xs, ys) = map abs $ zipWith (-) ys xs

countSimilarity :: ([Int], [Int]) -> [Int]
countSimilarity (xs, ys) = zipWith (*) xs $ map (\n -> Map.findWithDefault 0 n freqMap) xs
  where
    freqMap :: Map.Map Int Int
    freqMap = Map.fromListWith (+) [(y, 1) | y <- ys]

part1 :: String -> Int
part1 = sum
      . differenceAtSameIndex
      . splitAndSortLists
      . parseInput

part2 :: String -> Int
part2 = sum
      . countSimilarity
      . splitLists
      . parseInput 

