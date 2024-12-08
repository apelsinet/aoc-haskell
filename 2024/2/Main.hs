module Main where
import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

isAscending :: Ord a => [a] -> Bool
isAscending xs = and $ zipWith (<=) xs (tail xs)

isDescending :: Ord a => [a] -> Bool
isDescending xs = and $ zipWith (>=) xs (tail xs)

validateSafety :: [Int] -> Bool
validateSafety xs
  | not (isAscending xs || isDescending xs) = False
validateSafety [] = True
validateSafety [_] = True
validateSafety (x:y:xs) = if diff >= 1 && diff <= 3 then validateSafety (y:xs) else False
  where diff = abs (x - y)

validateSafetyWithOneFail :: [Int] -> Bool
validateSafetyWithOneFail xs =
  validateSafety xs || any validateSafety (removeOne xs)
  where
    removeOne [] = []
    removeOne (x:xs) = xs : map (x :) (removeOne xs)


part1 :: String -> Int
part1 = length
      . filter id
      . map validateSafety
      . parseInput

part2 :: String -> Int
part2 = length
      . filter id
      . map validateSafetyWithOneFail
      . parseInput


