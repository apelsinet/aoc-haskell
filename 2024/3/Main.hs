module Main where
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

getMatches :: String -> [String]
getMatches s = getAllTextMatches (s =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

parseMul :: String -> (Int, Int)
parseMul s =
  let withoutPrefix = drop 4 s
      withoutSuffix = init withoutPrefix
      (a, bWithComma) = break (== ',') withoutSuffix
      b = drop 1 bWithComma
  in (read a, read b)

part1 :: String -> Int
part1 = sum
      . map (\(a,b) -> a * b)
      . map parseMul
      . getMatches

removeDisabledInstructions :: String -> String
removeDisabledInstructions input = go input True
  where
    pattern = "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)"
    go :: String -> Bool -> String
    go remaining isEnabled
      | null remaining = ""
      | otherwise      =
        case remaining =~ pattern :: (String, String, String) of
          (before, match, after)
            | "do()" == match                        -> go after True
            | "don't()" == match                     -> go after False
            | "mul(" `isPrefixOf` match && isEnabled -> match ++ " " ++ go after isEnabled
            | otherwise                              -> go after isEnabled

part2 = sum
      . map (\(a,b) -> a * b)
      . map parseMul
      . getMatches
      . removeDisabledInstructions
