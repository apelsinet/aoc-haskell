module Main where
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

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

isValidPageSet :: [(Int, Int)] -> [Int] -> Bool
isValidPageSet rules []       = True
isValidPageSet rules [_]      = True
isValidPageSet rules (x:y:ys) = isValidPage rules x y && isValidPageSet rules (y:ys)

isValidPage :: [(Int, Int)] -> Int -> Int -> Bool
isValidPage [] _ _ = False
isValidPage ((a, b):rs) x y
  | x == a && y == b = True
  | otherwise        = isValidPage rs x y

validatePageSets :: [(Int, Int)] -> [[Int]] -> [Int]
validatePageSets rules = foldl processPageSet []
  where
    processPageSet acc x
      | isValidPageSet rules x = acc ++ [middle x]
      | otherwise              = acc

part1 :: String -> Int
part1 = sum
      . uncurry validatePageSets
      . parseInput

topologicalSort :: [(Int, Int)] -> [Int] -> [Int]
topologicalSort rules pages =
  let graph = buildGraph rules pages
  in reverse (dfsAll graph)

buildGraph :: [(Int, Int)] -> [Int] -> Map.Map Int [Int]
buildGraph rules pages =
  let relevantRules = [(a, b) | (a, b) <- rules, a `elem` pages && b `elem` pages]
  in foldr (\(a, b) g -> Map.insertWith (++) a [b] g) Map.empty relevantRules

dfsAll :: Map.Map Int [Int] -> [Int]
dfsAll graph = exec [] (Map.keysSet graph) []
  where
    exec visited remaining sorted
      | Set.null remaining = sorted
      | otherwise =
          let next = Set.findMin remaining
          in if next `elem` visited
               then exec visited (Set.delete next remaining) sorted
               else let (visited', sorted') = dfs graph visited [] next
                    in exec visited' (Set.delete next remaining) (sorted' ++ sorted)

dfs :: Map.Map Int [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
dfs graph visited sorted node
  | node `elem` visited = (visited, sorted)
  | otherwise =
      let neighbors = Map.findWithDefault [] node graph
          (visited', sorted') = foldl (\(v, s) n -> dfs graph v s n) (node : visited, sorted) neighbors
      in (visited', node : sorted')

orderInvalidPageSets :: [(Int, Int)] -> [[Int]] -> [[Int]]
orderInvalidPageSets rules = map reorder . filter (not . isValidPageSet rules)
  where
    reorder xs = topologicalSort rules xs

part2 :: String -> Int
part2 = sum
      . map middle
      . uncurry orderInvalidPageSets
      . parseInput
