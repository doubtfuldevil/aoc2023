{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module AoC01 where

import Data.List ( sort, isInfixOf )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Text.Read (readMaybe)

---------------------------------------
-- helper functions
---------------------------------------
getInput :: String -> String -> IO [String]
getInput mode day = do
    let filename = mode ++ day
    h <- openFile filename ReadMode
    content <- hGetContents h
    return (lines content)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)
---------------------------------------
-- main functionality
---------------------------------------
type Input = [String]

firstNum :: String -> Int
firstNum (x:xs) = case readMaybe (x:"") of
    Just num -> num
    _ -> firstNum xs
firstNum _ = error "Invalid string"

calibrationValue :: String -> Int
calibrationValue s = 10 * firstNum s + (firstNum . reverse) s

solution1 :: [String] -> Int
solution1 = sum . map calibrationValue

-- part 2

parseWordDigit :: String -> Maybe Int
parseWordDigit xs
    | isInfixOf "one" xs = Just 1
    | isInfixOf "two" xs = Just 2
    | isInfixOf "three" xs = Just 3
    | isInfixOf "four" xs = Just 4
    | isInfixOf "five" xs = Just 5
    | isInfixOf "six" xs = Just 6
    | isInfixOf "seven" xs = Just 7
    | isInfixOf "eight" xs = Just 8
    | isInfixOf "nine" xs = Just 9
    | otherwise = Nothing

firstDigit :: String -> String -> Int
firstDigit (x:xs) old = case readMaybe (x:"") of
    Just num -> num
    _ -> case parseWordDigit currentString of
      Just num -> num
      _ -> firstDigit xs currentString
    where currentString = old ++ [x]
firstDigit _ _ = error "Invalid string"

lastDigit :: String -> String -> Int
lastDigit (x:xs) old = case readMaybe (x:"") of
    Just num -> num
    _ -> case parseWordDigit currentString of
      Just num -> num
      _ -> lastDigit xs currentString
    where currentString = x:old
lastDigit _ _ = error "Invalid string"

calibrationValue2 :: String -> Int
calibrationValue2 s = 10 * firstDigit s "" + (lastDigit . reverse) s ""

solution2 :: [String] -> Int
solution2 = sum . map calibrationValue2
---------------------------------------
-- start functions
---------------------------------------
day :: [Char]
day = "01"

part1 :: IO ()
part1 = do
    testInput <- getInput "input/test" day
    print (solution1 testInput)
    realInput <- getInput "input/input" day
    print(solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "input/testb" day
    print (solution2 testInput)
    realInput <- getInput "input/input" day
    print(solution2 realInput)