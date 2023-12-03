import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "../inputs/day02.txt"
  let games = map parseGame $ lines input
  let possibleGames = filter (`isGamePossible` CubeSet 12 13 14) games
  -- print $ map idNumber $ take 10 possibleGames
  print $ map idNumber $ take 10 possibleGames
  print $ sum $ map idNumber possibleGames

  let powersOfMinSet = map (powerOfSet . minimunCubeSet) games
  print $ take 10 powersOfMinSet
  print $ sum powersOfMinSet

example1_input :: String
example1_input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

example1 :: IO ()
example1 = do
  let games = map parseGame $ lines example1_input
  let possibleGames = filter (`isGamePossible` CubeSet 12 13 14) games
  print $ take 10 possibleGames
  print $ sum $ map idNumber possibleGames

example2_input :: String
example2_input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

example2 :: IO ()
example2 = do
  let games = map parseGame $ lines example2_input
  let powersOfMinSet = map (powerOfSet . minimunCubeSet) games
  print powersOfMinSet
  print $ sum powersOfMinSet

data Game = Game {idNumber :: Int, sets :: [CubeSet]} deriving (Show)

data CubeSet = CubeSet {reds :: Int, greens :: Int, blues :: Int} deriving (Show)

-- Check if a game is possible, given that the bag contains the cubes in `CubeSet`
isGamePossible :: Game -> CubeSet -> Bool
isGamePossible game bag =
  let minset = minimunCubeSet game
      reds' = reds minset
      greens' = greens minset
      blues' = blues minset
   in reds' <= reds bag && greens' <= greens bag && blues' <= blues bag

minimunCubeSet :: Game -> CubeSet
minimunCubeSet game =
  let sets' = sets game
      reds' = maximum $ map reds sets'
      greens' = maximum $ map greens sets'
      blues' = maximum $ map blues sets'
   in CubeSet reds' greens' blues'

powerOfSet :: CubeSet -> Int
powerOfSet set = reds set * greens set * blues set

-- Parse `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green` into `Game 1 [CubeSet 4 0 3, CubeSet 1 2 6, CubeSet 0 2 0]`
parseGame :: String -> Game
parseGame s =
  let (game, sets) = splitAt (fromJust $ elemIndex ':' s) s
      game_id = read $ filter isDigit game
      sets' = map parseSet $ splitOn "; " $ tail $ tail sets
   in Game game_id sets'

-- Parse `3 blue, 4 red` into `CubeSet 4 0 3`
parseSet :: String -> CubeSet
parseSet s =
  let colors = map (splitOn " ") $ splitOn ", " s
      reds = sum $ map (read . head) $ filter (\x -> (x !! 1) == "red") colors
      greens = sum $ map (read . head) $ filter (\x -> (x !! 1) == "green") colors
      blues = sum $ map (read . head) $ filter (\x -> (x !! 1) == "blue") colors
   in CubeSet reds greens blues

splitOn :: String -> String -> [String]
splitOn = splitOn' []
  where
    splitOn' :: String -> String -> String -> [String]
    splitOn' acc _ [] = [acc]
    splitOn' acc sep (x : xs)
      | sep `isPrefixOf` (x : xs) = acc : splitOn' [] sep (drop (length sep) (x : xs))
      | otherwise = splitOn' (acc ++ [x]) sep xs
