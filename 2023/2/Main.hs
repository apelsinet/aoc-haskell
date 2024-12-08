module Main where

import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

  -- putStrLn $ "Part 2: " ++ show (part2 input)

red   = 0
green = 1
blue  = 2
allowedReds   = 12
allowedGreens = 13
allowedBlues  = 14

part1 :: String -> String
part1 c = lines c !! 0

-- convert each line to a tuple of (id :: Int, [[(color :: Int, count :: Int)]])
-- test each inner list to see if they are within the limits
-- for each inner list that passes the test, continue to the next inner list
-- if all inner lists pass the test, add the id to the sum
-- return the sum
