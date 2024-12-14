module Main where
import Data.List.Split (splitOn)
import Control.Monad (replicateM)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parseInput :: String -> [(Int, [Int])]
parseInput input = map (toPair . splitOn ":") $ lines input
  where
    toPair [a, b] = (read a, map read $ words b)

isValid :: Int -> [Int] -> Bool
isValid n xs =
  let ops = [add, mul]
      add a b = a + b
      mul a b = a * b
  in any (\opSeq -> evaluate xs opSeq == n) (replicateM (length xs - 1) ops)

evaluate :: [Int] -> [(Int -> Int -> Int)] -> Int
evaluate [x] [] = x
evaluate (x:y:xs) (op:ops) = evaluate (op x y : xs) ops
evaluate _ _ = error "Mismatched numbers and operators"

part1 :: String -> Int
part1 = sum
      . map fst
      . filter (uncurry isValid)
      . parseInput

isValid' :: Int -> [Int] -> Bool
isValid' n xs =
  let ops = [add, mul, concat]
      add a b = a + b
      mul a b = a * b
      concat a b = read $ show a ++ show b
  in any (\opSeq -> evaluate xs opSeq == n) (replicateM (length xs - 1) ops)

part2 :: String -> Int
part2 = sum
      . map fst
      . filter (uncurry isValid')
      . parseInput

