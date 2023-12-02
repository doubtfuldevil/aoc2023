{-# LANGUAGE InstanceSigs #-}
module AoC02 where

import Data.List ( sort, isInfixOf )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Char

---------------------------------------
-- helper functions
---------------------------------------
type Input = [String]

getInput :: String -> String -> IO [String]
getInput mode day = do
    let filename = mode ++ day
    h <- openFile filename ReadMode
    content <- hGetContents h
    return (lines content)

parseNum :: String -> Int
parseNum = read . takeWhile (\x -> isDigit x || x == '-' ) . dropWhile (not . (\x -> isDigit x || x == '-' ))

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)
---------------------------------------
-- main functionality
---------------------------------------
data Color = Red | Blue | Green
    deriving Show

data Game = Game Int [[(Int, Color)]]
    deriving Show

instance Eq Color where
    (==) :: Color -> Color -> Bool
    Red == Red      = True
    Blue == Blue    = True
    Green == Green  = True
    _ == _          = False

gameID :: Game -> Int
gameID (Game id _) = id

gamePulls :: Game -> [[(Int,Color)]]
gamePulls (Game _ xs) = xs

gamify :: String -> Game
gamify s =  Game id pulls
    where game = splitOn ':' s
          id = (parseNum . head) game
          pulls = (map (map tuplify . splitOn ',') . splitOn ';' . head . tail) game

getColor :: String -> Color
getColor s
    | "red" `isInfixOf` s = Red
    | "blue" `isInfixOf` s = Blue
    | "green" `isInfixOf` s = Green
    | otherwise = error "invalid color string"

tuplify :: String -> (Int,Color)
tuplify s = (parseNum s, getColor s)

checkValidity :: (Int,Int,Int) -> [(Int,Color)] -> Bool
checkValidity (_,_,_) [] = True
checkValidity (r,g,b) ((n,Red):ts) = (n <= r) && checkValidity (r,g,b) ts
checkValidity (r,g,b) ((n,Green):ts) = (n <= g) && checkValidity (r,g,b) ts
checkValidity (r,g,b) ((n,Blue):ts) = (n <= b) && checkValidity (r,g,b) ts

validGames :: (Int,Int,Int) -> [Game] -> [Bool]
validGames v = map (all (checkValidity v) . gamePulls )

solution1 :: Input -> Int
solution1 input = (sum . map fst . filter snd) (zip (map gameID g) (validGames (12,13,14) g))
    where g = map gamify input

-- part 2

getMinColor :: Color -> [[(Int,Color)]] -> Int
getMinColor color = maximum . map (fst . head) . filter (not . null) . map ( filter (\(_,c) -> c == color))

power :: [[(Int,Color)]] -> Int
power xs = getMinColor Red xs * getMinColor Green xs * getMinColor Blue xs

solution2 :: Input -> Int
solution2 = sum . map (power . gamePulls . gamify)
---------------------------------------
-- start functions
---------------------------------------
day :: [Char]
day = "02"

part1 :: IO ()
part1 = do
    testInput <- getInput "input/test" day
    print (solution1 testInput)
    realInput <- getInput "input/input" day
    print(solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "input/test" day
    print (solution2 testInput)
    realInput <- getInput "input/input" day
    print(solution2 realInput)