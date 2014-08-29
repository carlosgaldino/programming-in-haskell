module Nim where

import Data.Char (ord)

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

cls :: IO ()
cls = putStr "\ESC[2J"

showBoard :: Board -> IO ()
showBoard b = mapM_ showRow (mountRows b)

mountRows :: Board -> [(Int, String)]
mountRows b = zip [1..5] (map stars b)

showRow :: (Int, String) -> IO ()
showRow (n, s) = do putStr (show n)
                    putStr ": "
                    putStrLn s

stars :: Int -> String
stars n = replicate n '*'

newLine :: IO ()
newLine = putChar '\n'

getDigit :: String -> IO Int
getDigit msg = do newLine
                  putStr msg
                  x <- getChar
                  return (ord x - ord '0')

next :: Int -> Int
next 1 = 2
next 2 = 1

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b r n = n <= (b !! (pred r))

update :: Board -> Int -> Int -> Board
update b row num = [if r == row then n - num else n | (r, n) <- zip [1..5] b]

play :: Board -> Int -> IO ()
play b player = do if finished b then
                                 putStrLn $ "Player " ++ (show $ next player) ++ " wins!"
                   else
                    do newLine
                       showBoard b
                       newLine
                       putStr $ "Player " ++ (show player)
                       row <- getDigit "Enter a row number: "
                       n <- getDigit "Enter number of stars to remove: "
                       newLine
                       if valid b row n then
                                        play (update b row n) (next player)
                       else
                         do newLine
                            putStr "ERROR: Invalid input"
                            play b player

nim :: IO ()
nim = play initial 1
