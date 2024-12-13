module Main where
import qualified Data.Set as Set

type Position = (Int, Int)
type Direction = Char -- '^', '>', 'v', '<'
type GameState = [String]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (visited, finalState) = part1 input
  putStrLn $ "Part 1: " ++ show (Set.size visited)
  putStrLn "Final State:"
  putStrLn finalState

turnRight :: Direction -> Direction
turnRight '^' = '>'
turnRight '>' = 'v'
turnRight 'v' = '<'
turnRight '<' = '^'

stepForward :: Position -> Direction -> Position
stepForward (x, y) '^' = (x - 1, y)
stepForward (x, y) '>' = (x, y + 1)
stepForward (x, y) 'v' = (x + 1, y)
stepForward (x, y) '<' = (x, y - 1)
stepForward _ _ = error "Invalid step"

isPlayerOut :: Position -> GameState -> Bool
isPlayerOut (x, y) state = x < 0 || y < 0 || x >= length state || y >= length (head state)

isObstacle :: Position -> GameState -> Bool
isObstacle (x, y) state
  | isPlayerOut (x, y) state = True -- Treat out-of-bounds as an obstacle
  | otherwise = state !! x !! y == '#'

tick :: Position -> Direction -> GameState -> (Position, Direction, Bool)
tick position direction state =
  let newPosition = stepForward position direction
  in if isPlayerOut newPosition state
        then (newPosition, direction, True) -- Stop when out of bounds
        else if isObstacle newPosition state
            then (position, turnRight direction, False) -- Turn right if there's an obstacle
            else (newPosition, direction, False) -- Move forward otherwise

gameLoop :: Position -> Direction -> Set.Set (Position, Direction) -> GameState -> Set.Set Position
gameLoop position direction visited state
  | isPlayerOut position state = Set.map fst visited -- Stop when out of bounds
  | otherwise =
      let (newPosition, newDirection, shouldStop) = tick position direction state
      in if shouldStop
            then Set.insert position (Set.map fst visited) -- Include the last position and stop
            else gameLoop newPosition newDirection (Set.insert (position, direction) visited) state

findStart :: GameState -> Position
findStart state = head [(x, y) | x <- [0..length state - 1], y <- [0..length (head state) - 1], state !! x !! y == '^']

markVisited :: GameState -> Set.Set Position -> GameState
markVisited state visited =
  [ [if (x, y) `Set.member` visited then 'X' else cell
     | (y, cell) <- zip [0..] row]
     | (x, row) <- zip [0..] state]

part1 :: String -> (Set.Set Position, String)
part1 input =
  let state = lines input
      initialPosition = findStart state
      initialDirection = '^'
      visited = gameLoop initialPosition initialDirection Set.empty state
      finalState = markVisited state visited
  in (visited, unlines finalState)
