module Main where

import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

  putStrLn $ "Part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 = sum
      . map (read . (\xs -> [head xs, last xs]) . filter isDigit)
      . lines

getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers ('z':'e':'r':'o':cs)     = 0 : getNumbers ('o':cs)
getNumbers ('o':'n':'e':cs)         = 1 : getNumbers ('e':cs)
getNumbers ('t':'w':'o':cs)         = 2 : getNumbers ('o':cs)
getNumbers ('t':'h':'r':'e':'e':cs) = 3 : getNumbers ('e':cs)
getNumbers ('f':'o':'u':'r':cs)     = 4 : getNumbers cs
getNumbers ('f':'i':'v':'e':cs)     = 5 : getNumbers ('e':cs)
getNumbers ('s':'i':'x':cs)         = 6 : getNumbers cs
getNumbers ('s':'e':'v':'e':'n':cs) = 7 : getNumbers ('n':cs)
getNumbers ('e':'i':'g':'h':'t':cs) = 8 : getNumbers ('t':cs)
getNumbers ('n':'i':'n':'e':cs)     = 9 : getNumbers ('e':cs)
getNumbers (c:cs)
  | isDigit c = digitToInt c : getNumbers cs
  | otherwise = getNumbers cs

part2 :: String -> Int
part2 = sum
      . map ((\xs -> 10*head xs + last xs) . getNumbers)
      . lines
