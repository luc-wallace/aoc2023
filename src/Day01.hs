module Day01 where

import Data.Char (isDigit)
import Data.List (elemIndex)

solve :: String -> (Int, Int)
solve input = (partOne, partTwo)
  where
    partOne = sum $ map calibrationValueNum (lines input)
    partTwo = sum $ map calibrationValueText (lines input)

calibrationValueNum :: String -> Int
calibrationValueNum str = read [head dgts, last dgts] :: Int
  where
    dgts = filter isDigit str

digits :: [String]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitsBw :: [String]
digitsBw = map reverse digits

firstDigit :: [String] -> String -> String -> Int
firstDigit dgts (c : cs) accum =
  if isDigit c
    then
      if null accum
        then read [c]
        else firstDigit dgts (tail cur) ""
    else case elemIndex cur dgts of
      Just n -> n + 1
      Nothing -> firstDigit dgts cs cur
  where
    cur = accum ++ [c]
firstDigit dgts [] accum = firstDigit dgts (tail accum) ""

calibrationValueText :: String -> Int
calibrationValueText str = 10 * firstDigit digits str "" + firstDigit digitsBw (reverse str) ""
