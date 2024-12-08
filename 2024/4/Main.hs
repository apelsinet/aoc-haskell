module Main where
import Data.List (transpose, sortOn, groupBy)
import Data.Function (on)
import Text.Regex.TDFA ((=~), getAllTextMatches)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

horizontal :: [[Char]] -> [[Char]]
horizontal = id

horizontalReversed :: [[Char]] -> [[Char]]
horizontalReversed = map reverse

vertical :: [[Char]] -> [[Char]]
vertical = transpose

verticalReversed :: [[Char]] -> [[Char]]
verticalReversed = map reverse . transpose

diagonals :: [[Char]] -> [[Char]]
diagonals xss =
  let indexed = [((r,c), x) | (r,row) <- zip [0..] xss, (c,x) <- zip [0..] row]
      grouped = groupByKey (\((r,c),_) -> c - r) indexed
  in map (map snd) grouped

groupByKey :: Ord k => (((Int,Int), Char) -> k) -> [((Int,Int), Char)] -> [[((Int,Int), Char)]]
groupByKey f xs =
  let sorted  = sortOn f xs
      grouped = groupBy (\a b -> f a == f b) sorted
  in grouped

diagonalsReversed :: [[Char]] -> [[Char]]
diagonalsReversed = map reverse . diagonals

rotate90CCW :: [[a]] -> [[a]]
rotate90CCW = reverse . transpose

diagonalsCCW :: [[Char]] -> [[Char]]
diagonalsCCW = diagonals . rotate90CCW

diagonalsReversedCCW :: [[Char]] -> [[Char]]
diagonalsReversedCCW = diagonalsReversed . rotate90CCW

getMatches :: String -> String -> [String]
getMatches x y = getAllTextMatches (y =~ x)

part1 :: String -> Int
part1 input =
  let lns   = lines input
      transformations = [ horizontal
                        , horizontalReversed
                        , vertical
                        , verticalReversed
                        , diagonals
                        , diagonalsReversed
                        , diagonalsCCW
                        , diagonalsReversedCCW ]

      sumMatchesFor t = sum . map (length . getMatches("XMAS")) . t $ lns
  in sum (map sumMatchesFor transformations)

part2 :: String -> Int
part2 input =
  let lns = lines input
  in sum [ 1 | r <- [1 .. length lns - 2] , c <- [1 .. length (head lns) - 2] , isXMAS lns r c ]

isXMAS :: [String] -> Int -> Int -> Bool
isXMAS matrix r c =
  let getChar r c = if r >= 0 && r < length matrix && c >= 0 && c < length (head matrix)
                      then matrix !! r !! c
                      else '.'
      topLeft     = [getChar (r-1) (c-1), getChar r c, getChar (r+1) (c+1)]
      topRight    = [getChar (r-1) (c+1), getChar r c, getChar (r+1) (c-1)]
      isMAS s     = s == "MAS" || s == "SAM"
  in isMAS topLeft && isMAS topRight
