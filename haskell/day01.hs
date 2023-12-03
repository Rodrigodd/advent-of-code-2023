import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, mapMaybe)

main :: IO ()
main = do
  input <- readFile "../inputs/day01.txt"
  let values = calibrationValues input
  print $ take 10 values
  print $ sum values

  let values = actualCalibrationValues input
  print $ take 10 values
  print $ sum values

example1_input :: String
example1_input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

example1 :: IO ()
example1 = do
  let values = calibrationValues example1_input
  print values
  print $ sum values

example2_input :: String
example2_input = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

example2 :: IO ()
example2 = do
  let values = actualCalibrationValues example2_input
  print values
  print $ sum values

-- Part 1

calibrationValues :: String -> [Int]
calibrationValues = map calibrationValue . lines

calibrationValue :: String -> Int
calibrationValue x =
  let y = filter isDigit x
   in read [head y, last y]

-- Part 2

actualCalibrationValues :: String -> [Int]
actualCalibrationValues = map actualCalibrationValue . lines

matches :: [String]
matches = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

actualCalibrationValue :: String -> Int
actualCalibrationValue x =
  let m = searchForMatches matches x
   in matchToNumber (head m) * 10 + matchToNumber (last m)

matchToNumber :: String -> Int
matchToNumber "one" = 1
matchToNumber "two" = 2
matchToNumber "three" = 3
matchToNumber "four" = 4
matchToNumber "five" = 5
matchToNumber "six" = 6
matchToNumber "seven" = 7
matchToNumber "eight" = 8
matchToNumber "nine" = 9
matchToNumber x = read x

-- Given a list of patterns, search for the first match in the string
-- and return it. If no matches are found, return Nothing.
searchForMatches :: [String] -> String -> [String]
searchForMatches [] _ = []
searchForMatches patterns [] = []
searchForMatches patterns string =
  case filter (`isPrefixOf` string) patterns of
    [] -> searchForMatches patterns (tail string)
    (x : _) -> x : searchForMatches patterns (tail string)
